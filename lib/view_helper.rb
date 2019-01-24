require 'sinatra/base'

module Sinatra
  module ViewHelper
    module Helpers
      def body_class(body)
        body ? body : nil
      end

      def campaigns
        [
          'If applicable, select campaign before claiming',
          'Dickson',
          'Warringah',
        ]
      end
    end

    def self.registered(app)
      app.helpers ViewHelper::Helpers
    end
  end

  register ViewHelper
end
