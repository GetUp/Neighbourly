require 'sinatra'
require "sinatra/reloader" if development?
require 'byebug' if development?
require 'dotenv'
require 'haml'
require 'sequel'
require 'time'
require "httparty"
require 'json'
require 'sinatra/flash'
require 'sinatra/json'
require 'sinatra/cookies'

require_relative "lib/login_helper"
require_relative 'lib/view_helper'
require_relative './services/claim_service'
require_relative './services/geo_service'
require_relative "models/user"

Dotenv.load
enable :sessions
set :session_secret, ENV["SECRET_KEY_BASE"]
set :unclaim_token, ENV['DATA_ENTRY_UNCLAIM_TOKEN']

def test_db_connection
  Sequel.connect(ENV['TEST_DATABASE_URL'] || "postgres://localhost/neighbourly_test")
end

configure do
  db = Sequel.connect(ENV['DATABASE_URL'] || 'postgres://localhost/neighbourly')
  set :db, db
end

configure :production do
  db = Sequel.connect(ENV['DATABASE_URL'])
  set :db, db
end

configure :test do
  set :db, test_db_connection
end

Sequel.datetime_class = DateTime

get '/' do
  if authorised?
    redirect '/map'
  else
    haml :main, locals: {page: 'main', body: 'main'}
  end
end

def login_attempt
  #Primary login method is e-mail
  #If a param is passed (form or URL) - use that
  #If a cookie is set - use that secondarily
  #If no e-mail is present, send to the frontpage
  if params.has_key?("email")
    authorise(params[:email].strip)
  elsif cookies.has_key?("email")
    authorise(cookies[:email])
  else
    redirect '/'
  end

  user_params = Hash.new
  fields = ["email", "first_name", "last_name", "phone", "postcode"]

  #Check that user exists for current e-mail
  if authorised?
    redirect "/map"

  #If e-mail is set in param - go straight to the user_details page
  elsif params.has_key?("email")
    redirect "/user_details?email=#{CGI.escape(user_email)}"

  #If user does not exist and all fields exist in cookie - create_user
  elsif fields.all? {|s| cookies.key? s}
    fields.each do |key_get|
      user_params[key_get] = cookies[key_get]
    end
  create_user(user_params)
  redirect "/map"

  end
end

get '/login' do
  login_attempt
end

post '/login' do
  login_attempt
end

get "/user_details" do
  haml :user_details, locals: {page: "user_details", email: params[:email] }
end

def create_user(user_params)
  puts "Creating user: #{user_params}"
  user = User.new(settings.db)
  #Submit user details to database
  #And, Catch double-submission errors and send details to Zapier
  begin
    if user.create!(user_params)

      #Send user details to the Zapier endpoint
      if ENV["ZAP_API_ON"] == "True"
        HTTParty.post(ENV["ZAP_API"],:body => user_params, timeout: 2)
      end

      #Once the user is created - authorise them
      authorise(user_params['email'])
      redirect "/map"
    else
      #TODO - needs validation
      flash[:error] = "Please enter correct details."
      haml :user_details
    end

  #Skip all errors and retry auth without ZAP_API call
  #REDUNDANT - Skip details re-entry if e-mail already exists in database
  #REDUNDANT - Skip if HTTParty fails to make the API call
rescue StandardError, Sequel::UniqueConstraintViolation, HTTParty::Error => e
    puts "Error in User Details Submission: #{e.message}"
    authorise(user_params['email'])
    redirect "/map"
  end
end

post "/user_details" do
  create_user(params[:user_details])
end

get '/map' do
  authorised do
    haml :map, locals: {page: 'map'}
  end
end

get '/logout' do
  session.clear
  flash[:notice] = 'You have been logged out.'
  redirect '/'
end

def claim_status(claimer)
  if is_admin?(claimer)
    "quarantine"
  elsif claimer == session[:user_email]
    "claimed_by_you"
  else
    "claimed"
  end
end

def get_meshblocks_with_status(json)
  slugs = json["features"].map{|feature| feature["properties"]["slug"] }
  claim_service = ClaimService.new(settings.db)
  claims = claim_service.claims(slugs)

  statuses = claims.map do |c|
    [ c[:mesh_block_slug], claim_status(c[:mesh_block_claimer]) ]
  end.to_h

  json["features"] = json["features"].map do |feature|
    state = statuses[feature["properties"]["slug"].to_s]
    feature["properties"]["claim_status"] = state || "unclaimed"
    feature
  end

  json
end

#For loading new SA1s when scrolling on the map
get '/meshblocks_bounds' do
  authorised do
    url = "#{ENV['LAMBDA_BASE_URL']}/territories/bounds"
    response = HTTParty.get(url, {query: params})
    data = JSON.parse response.body

    if data['features'] == nil
        puts "404 due to map location returning no meshblocks"
        status 404
    else
      json get_meshblocks_with_status(data)
    end
  end
end

#For finding out the bounds of a postcode
get '/pcode_get_bounds' do
  authorised do
    geo_service = GeoService.new(settings.db)
    bounds = geo_service.pcode_bounds(params[:pcode])
    json bounds[0]
  end
end

post '/claim_meshblock/:id' do
  authorised do
    claim_service = ClaimService.new(settings.db)
    claim_service.claim(params['id'], user_email)
    status 200
  end
end

post '/unclaim_meshblock/:id' do
  authorised do
    claim_service = ClaimService.new(settings.db)
    #TODO - return error on fail
    if is_admin?(user_email)
      claim_service.admin_unclaim(params['id'])
    else
      claim_service.unclaim(params['id'], user_email)
    end
    status 200
  end
end

get '/data_entry' do
  haml :data_entry, locals: {page: 'data_entry'}, layout: false
end

post '/survey' do
  authorised do
    haml :data_entry, locals: {page: 'data_entry'}
  end
end

post '/unclaim_from_data_entry' do
  data = JSON.parse request.body.read

  return 400 unless settings.unclaim_token &&
    settings.unclaim_token.length > 5 &&
    settings.unclaim_token == data['token']

  ClaimService.new(settings.db).data_entry_unclaim(data['id'])
  status 200
end
