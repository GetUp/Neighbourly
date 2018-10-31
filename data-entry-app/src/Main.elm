module Main exposing (Address, BlockID, Model, Msg(..), addressDecoder, getAddressesForBlockId, httpErrorString, init, main, subscriptions, toUrl, update, view, viewCanvas)

import Browser
import Date exposing (Date, day, month, today, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Debug exposing (toString)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, andThen, at, dict, field, int, list, map3, map5, map6, string)
import Json.Encode
import Regex
import Task
import Url.Builder as Url


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy eq xs =
    case xs of
        [] ->
            []

        x :: xs2 ->
            let
                ( ys, zs ) =
                    listspan (eq x) xs2
            in
            (x :: ys) :: groupBy eq zs


listspan : (a -> Bool) -> List a -> ( List a, List a )
listspan p xs =
    ( takeWhile p xs, dropWhile p xs )


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                x :: takeWhile predicate xs

            else
                []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs

            else
                list



-- Models


type ValidationStatus
    = Valid
    | Invalid
    | Unknown


type alias Model =
    { blockId : BlockID
    , status : String
    , valid : ValidationStatus
    , addresses : List Address
    , canvas : Canvas
    , date : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


type alias Canvas =
    Dict String Survey


type alias AddressAndCanvasData =
    { addresses : List Address
    , canvas : Canvas
    }


type Msg
    = UpdateBlockID String
    | LoadAddresses (Result Http.Error AddressAndCanvasData)
    | ToDatePicker DatePicker.Msg
    | SetDate (Maybe Date)
    | UpdateOutcome Survey String
    | UpdateDuttonSupport Survey String
    | UpdateWorthReturning Survey String
    | UpdateVoterID Survey String
    | UpdateDuttonLast Survey String
    | UpdateNotes Survey String
    | SaveSurvey Survey
    | LoadSurvey (Result Http.Error Survey)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { blockId = ""
      , status = ""
      , valid = Unknown
      , addresses = []
      , canvas = Dict.empty
      , date = Nothing
      , datePicker = datePicker
      }
    , Cmd.batch [ Cmd.map ToDatePicker datePickerFx ]
    )


type alias BlockID =
    String


type alias SurveyResponse =
    String


type alias Address =
    { gnaf_pid : String, address : String, street : String }


type alias SurveyResponses =
    { outcome : SurveyResponse
    , dutton_support : SurveyResponse
    , worth_returning : SurveyResponse
    , voter_id : SurveyResponse
    , dutton_last : SurveyResponse
    , notes : SurveyResponse
    }


type alias Survey =
    { gnaf_pid : String
    , block_id : String
    , survey_on : Maybe Date
    , updated_at : String
    , responses : SurveyResponses
    }


emptySurvey : String -> String -> Maybe Date -> Survey
emptySurvey gnaf_pid block_id survey_on =
    { gnaf_pid = gnaf_pid
    , block_id = block_id
    , survey_on = survey_on
    , updated_at = ""
    , responses =
        { outcome = ""
        , dutton_support = ""
        , worth_returning = ""
        , voter_id = ""
        , dutton_last = ""
        , notes = ""
        }
    }


addressAndCanvasDecoder : Decoder AddressAndCanvasData
addressAndCanvasDecoder =
    Json.Decode.map2 AddressAndCanvasData (field "addresses" addressDecoder) (field "surveys" surveysDecoder)


addressDecoder : Decoder (List Address)
addressDecoder =
    list (map3 Address (field "gnaf_pid" string) (field "address" string) (field "street" string))


surveysDecoder : Decoder Canvas
surveysDecoder =
    dict surveyDecoder


responsesDecoder : Decoder SurveyResponses
responsesDecoder =
    map6 SurveyResponses (field "outcome" string) (field "dutton_support" string) (field "worth_returning" string) (field "voter_id" string) (field "dutton_last" string) (field "notes" string)


stringToDate : String -> Decoder (Maybe Date)
stringToDate date =
    let
        result =
            Date.fromIsoString (String.left 10 date)
    in
    case result of
        Ok parsedDate ->
            Json.Decode.succeed (Just parsedDate)

        Err err ->
            Json.Decode.fail err


surveyDecoder : Decoder Survey
surveyDecoder =
    map5 Survey (field "gnaf_pid" string) (field "block_id" string) (field "survey_on" string |> andThen stringToDate) (field "updated_at" string) (at [ "responses" ] responsesDecoder)


getAddressesForBlockId : BlockID -> Date -> Cmd Msg
getAddressesForBlockId id survey_on =
    Http.send LoadAddresses (Http.get (toUrl id survey_on) addressAndCanvasDecoder)


surveyToJson : Survey -> Json.Encode.Value
surveyToJson survey =
    case survey.survey_on of
        Nothing ->
            Json.Encode.object []

        Just set_survey_on ->
            Json.Encode.object
                [ ( "gnaf_pid", Json.Encode.string survey.gnaf_pid )
                , ( "block_id", Json.Encode.string survey.block_id )
                , ( "survey_on", Json.Encode.string (Date.toIsoString set_survey_on) )
                , ( "responses"
                  , Json.Encode.object
                        [ ( "outcome", Json.Encode.string survey.responses.outcome )
                        , ( "dutton_support", Json.Encode.string survey.responses.dutton_support )
                        , ( "worth_returning", Json.Encode.string survey.responses.worth_returning )
                        , ( "voter_id", Json.Encode.string survey.responses.voter_id )
                        , ( "dutton_last", Json.Encode.string survey.responses.dutton_last )
                        , ( "notes", Json.Encode.string survey.responses.notes )
                        ]
                  )
                ]


upsertSurvey : Survey -> Cmd Msg
upsertSurvey survey =
    Http.send LoadSurvey
        (Http.post (Url.crossOrigin apiBase [ "prod", "survey" ] []) (Http.jsonBody (surveyToJson survey)) surveyDecoder)


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl text ->
            "Bad Url: " ++ text

        Http.Timeout ->
            "Http Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus response ->
            "Bad Http Status: " ++ toString response.status.code

        Http.BadPayload message response ->
            "Bad Http Payload: "
                ++ toString message
                ++ " ("
                ++ toString response.status.code
                ++ ")"


datepickerSettings : DatePicker.Settings
datepickerSettings =
    { defaultSettings
        | inputClassList = [ ( "mdl-textfield__input", True ) ]
        , placeholder = ""
        , inputId = Just "date-field"
    }


apiBase : String
apiBase =
    "https://4oqtu02x7f.execute-api.ap-southeast-2.amazonaws.com"


toUrl : BlockID -> Date -> String
toUrl id survey_on =
    Url.crossOrigin apiBase [ "prod", "addresses" ] [ Url.string "slug" id, Url.string "survey_on" (Date.toIsoString survey_on) ]



-- Commands


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateModelWithSurveyResponse : Model -> Survey -> (SurveyResponses -> SurveyResponses) -> Model
        updateModelWithSurveyResponse modelToUpdate survey fn =
            { modelToUpdate
                | canvas =
                    Dict.insert survey.gnaf_pid { survey | responses = fn survey.responses } modelToUpdate.canvas
            }

        _ =
            Debug.log "Message: " msg
    in
    case msg of
        UpdateBlockID newBlockId ->
            case model.date of
                Nothing ->
                    ( model, Cmd.none )

                Just existingDate ->
                    if String.length newBlockId == 11 then
                        ( { model | blockId = newBlockId, valid = Valid, status = "Loading..", addresses = [] }, getAddressesForBlockId newBlockId existingDate )

                    else
                        ( { model | blockId = newBlockId, valid = Invalid, status = validationMessage newBlockId, addresses = [] }, Cmd.none )

        UpdateOutcome survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | outcome = newValue }), Cmd.none )

        UpdateDuttonSupport survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | dutton_support = newValue }), Cmd.none )

        UpdateVoterID survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | voter_id = newValue }), Cmd.none )

        UpdateWorthReturning survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | worth_returning = newValue }), Cmd.none )

        UpdateDuttonLast survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | dutton_last = newValue }), Cmd.none )

        UpdateNotes survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | notes = newValue }), Cmd.none )

        SaveSurvey survey ->
            ( model, upsertSurvey survey )

        LoadAddresses result ->
            case result of
                Ok newData ->
                    case newData.addresses of
                        [] ->
                            ( { model | status = "Could not find the block for that ID", addresses = [] }, Cmd.none )

                        _ ->
                            ( { model | addresses = newData.addresses, status = "Enter the 11 digit Block ID", canvas = newData.canvas }, Cmd.none )

                Err err ->
                    ( { model | status = httpErrorString err, addresses = [] }, Cmd.none )

        LoadSurvey result ->
            case result of
                Ok newSurvey ->
                    ( { model | canvas = Dict.insert newSurvey.gnaf_pid newSurvey model.canvas }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        SetDate date ->
            ( { model | date = date }, Cmd.none )

        ToDatePicker subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update datepickerSettings subMsg model.datePicker
            in
            ( { model
                | date =
                    case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.date
                , datePicker = newDatePicker
              }
            , Cmd.none
            )


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- Views


questions : String -> List String
questions question =
    let
        options =
            Dict.fromList
                [ ( "outcome", [ "", "unable to knock", "not home", "not interested", "meaningful conversation" ] )
                , ( "dutton_support", [ "", "1 - strongly against", "2 - against", "3 - neutral", "4 - support", "5 - strongly support" ] )
                , ( "worth_returning", [ "", "yes", "no" ] )
                , ( "voter_id", [ "", "ALP", "LIB", "GRN", "ONP", "other", "refused to say" ] )
                , ( "dutton_last", [ "", "yes", "no" ] )
                ]
    in
    Maybe.withDefault [] (Dict.get question options)


answerOptions : String -> String -> List (Html Msg)
answerOptions question selectedValue =
    let
        answerOption value =
            option [ selected (value == selectedValue) ] [ text value ]
    in
    List.map answerOption (questions question)


validationMessage : BlockID -> String
validationMessage blockId =
    "Enter " ++ toString (11 - String.length blockId) ++ " more digits"


viewCanvases : Model -> List (Html Msg)
viewCanvases model =
    let
        addressesByStreet =
            groupBy (\a a2 -> a.street == a2.street) model.addresses
    in
    List.map (viewCanvas model) model.addresses


canvasHeader : Html Msg
canvasHeader =
    tr []
        [ th [] [ text "Address" ]
        , th [] [ text "Outcome" ]
        , th [] [ text "Dutton Support" ]
        , th [] [ text "Return" ]
        , th [] [ text "Voter ID" ]
        , th [] [ text "Dutton last" ]
        , th [] [ text "Notes" ]
        , th [] [ text "Last saved" ]
        , th [] [ text "Actions" ]
        ]


viewCanvas : Model -> Address -> Html Msg
viewCanvas model address =
    let
        newSurvey =
            emptySurvey address.gnaf_pid model.blockId model.date

        survey =
            Maybe.withDefault newSurvey (Dict.get address.gnaf_pid model.canvas)

        disabledUnlessMeaningful =
            survey.responses.outcome /= "meaningful conversation"
    in
    tr []
        [ td []
            -- [ text (String.replace address.street "" address.address), br [] [], span [ class "small" ] [ text address.gnaf_pid ] ]
            [ text address.address, br [] [], span [ class "small" ] [ text address.gnaf_pid ] ]
        , td []
            [ select [ onInput (UpdateOutcome survey) ] (answerOptions "outcome" survey.responses.outcome) ]
        , td []
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateDuttonSupport survey) ] (answerOptions "dutton_support" survey.responses.dutton_support) ]
        , td []
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateWorthReturning survey) ] (answerOptions "worth_returning" survey.responses.worth_returning) ]
        , td []
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateVoterID survey) ] (answerOptions "voter_id" survey.responses.voter_id) ]
        , td []
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateDuttonLast survey) ] (answerOptions "dutton_last" survey.responses.dutton_last) ]
        , td []
            [ textarea [ disabled disabledUnlessMeaningful, onInput (UpdateNotes survey) ] [ text survey.responses.notes ] ]
        , td []
            [ span [ class "small" ] [ text (String.left 10 survey.updated_at) ] ]
        , td []
            [ button [ onClick (SaveSurvey survey), class "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored" ] [ text "Save" ] ]
        ]


statusMessage : Model -> String
statusMessage model =
    case model.status of
        "" ->
            "Enter the 11 digit Block ID"

        status ->
            status


validationClass : Model -> String
validationClass model =
    case model.valid of
        Invalid ->
            "has-error"

        Valid ->
            "has-success"

        _ ->
            ""


view : Model -> Html Msg
view model =
    let
        hiddenClass =
            case model.addresses of
                [] ->
                    " hidden"

                _ ->
                    ""

        loadingClass =
            case model.status of
                "Loading.." ->
                    ""

                _ ->
                    " hidden"
    in
    div [ class "mdl-grid" ]
        [ div [ class "mdl-card mdl-card mdl-cell mdl-cell--9-col-desktop mdl-cell--6-col-tablet mdl-cell--4-col-phone mdl-shadow--2dp date-picker-cell" ]
            [ div [ class "mdl-card__supporting-text" ]
                [ div [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" ]
                    [ DatePicker.view model.date datepickerSettings model.datePicker
                        |> Html.map ToDatePicker
                    , label [ class "mdl-textfield__label" ] [ text "Select the date of the doorknock" ]
                    ]
                , div [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" ]
                    [ input [ onInput UpdateBlockID, value model.blockId, id "block-id", class "mdl-textfield__input", type_ "text" ] []
                    , label [ class "mdl-textfield__label", for "block-id" ] [ text (statusMessage model) ]
                    ]
                , div [ class ("mdl-progress mdl-js-progress mdl-progress__indeterminate" ++ loadingClass), style "width" "100%" ] []
                , br [] []
                , br [] []
                , text "Quick hints:"
                , ol []
                    [ li [] [ text "Use the 'Tab' key to move between fields." ]
                    , li [] [ text "On dropdowns, type the first first letter of an option to jump to it e.g. type 'm' to jump to \"meaningful conversation\"." ]
                    , li [] [ text "On dropdowns, you can also use the arrow keys to move between options." ]
                    , li [] [ text "Once you have selected an option press 'Enter' to select it." ]
                    , li [] [ text "Press Enter while on the Save button to trigger it." ]
                    ]
                ]
            ]
        , div [ class "mdl-cell" ]
            [ table
                [ class ("mdl-data-table mdl-js-data-table mdl-shadow--2dp" ++ hiddenClass) ]
                [ thead [] [ canvasHeader ]
                , tbody [] (viewCanvases model)
                ]
            ]
        ]