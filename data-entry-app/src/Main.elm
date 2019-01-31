module Main exposing (Model, ValidationStatus(..), view)

import Browser
import Date exposing (Date, day, month, today, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Debug exposing (toString)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, andThen, at, dict, field, int, list, map3, map5, map7, string)
import Json.Encode
import Task
import Url.Builder as Url


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- Models --


type alias Model =
    { campaign : String
    , blockId : BlockID
    , status : String
    , valid : ValidationStatus
    , addresses : List Address
    , canvas : Canvas
    , date : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


type ValidationStatus
    = Valid
    | Invalid
    | Unknown


type alias Canvas =
    Dict String Survey


type alias AddressAndCanvasData =
    { addresses : List Address
    , canvas : Canvas
    }


type alias BlockID =
    String


type alias SurveyResponse =
    String


type alias Address =
    { gnaf_pid : String, address : String, street : String }


type alias SurveyResponses =
    { outcome : SurveyResponse
    , mp_support : SurveyResponse
    , worth_returning : SurveyResponse
    , q3_enum : SurveyResponse
    , q4_boolean : SurveyResponse
    , key_issue : SurveyResponse
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
        , mp_support = ""
        , worth_returning = ""
        , q3_enum = ""
        , q4_boolean = ""
        , key_issue = ""
        , notes = ""
        }
    }



-- JSON Decoders --


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
    map7
        SurveyResponses
        (field "outcome" string)
        (field "mp_support" string)
        (field "worth_returning" string)
        (field "q3_enum" string)
        (field "q4_boolean" string)
        (field "key_issue" string)
        (field "notes" string)


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
    map5
        Survey
        (field "gnaf_pid" string)
        (field "block_id" string)
        (field "survey_on" string |> andThen stringToDate)
        (field "updated_at" string)
        (at [ "responses" ] responsesDecoder)


getAddressesForBlockId : BlockID -> Date -> Cmd Msg
getAddressesForBlockId id survey_on =
    Http.get
        { url = toUrl id survey_on
        , expect = Http.expectJson LoadAddresses addressAndCanvasDecoder
        }



-- JSON Encoder --


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
                        , ( "mp_support", Json.Encode.string survey.responses.mp_support )
                        , ( "worth_returning", Json.Encode.string survey.responses.worth_returning )
                        , ( "q3_enum", Json.Encode.string survey.responses.q3_enum )
                        , ( "q4_boolean", Json.Encode.string survey.responses.q4_boolean )
                        , ( "key_issue", Json.Encode.string survey.responses.key_issue )
                        , ( "notes", Json.Encode.string survey.responses.notes )
                        ]
                  )
                ]



-- HTTP --


upsertSurvey : Survey -> Cmd Msg
upsertSurvey survey =
    Http.post
        { url = Url.crossOrigin apiBase [ "prod", "survey" ] []
        , body = Http.jsonBody (surveyToJson survey)
        , expect = Http.expectJson LoadSurvey surveyDecoder
        }


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl text ->
            "Bad Url: " ++ text

        Http.Timeout ->
            "Http Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus statusCode ->
            "Bad Http Status: " ++ toString statusCode

        Http.BadBody message ->
            "Bad Http Payload: " ++ toString message


apiBase : String
apiBase =
    "https://4oqtu02x7f.execute-api.ap-southeast-2.amazonaws.com"


toUrl : BlockID -> Date -> String
toUrl id survey_on =
    Url.crossOrigin apiBase [ "prod", "addresses" ] [ Url.string "slug" id, Url.string "survey_on" (Date.toIsoString survey_on) ]



-- Update --


type Msg
    = UpdateCampaign String
    | UpdateBlockID String
    | LoadAddresses (Result Http.Error AddressAndCanvasData)
    | ToDatePicker DatePicker.Msg
    | SetDate (Maybe Date)
    | UpdateOutcome Survey String
    | UpdateMpSupport Survey String
    | UpdateWorthReturning Survey String
    | UpdateQ3Enum Survey String
    | UpdateQ4Boolean Survey String
    | UpdateKeyIssue Survey String
    | UpdateNotes Survey String
    | SaveSurvey Survey
    | LoadSurvey (Result Http.Error Survey)


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
        UpdateCampaign newCampaign ->
            ( { model | campaign = newCampaign }, Cmd.none )

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

        UpdateMpSupport survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | mp_support = newValue }), Cmd.none )

        UpdateQ3Enum survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | q3_enum = newValue }), Cmd.none )

        UpdateWorthReturning survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | worth_returning = newValue }), Cmd.none )

        UpdateQ4Boolean survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | q4_boolean = newValue }), Cmd.none )

        UpdateKeyIssue survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | key_issue = newValue }), Cmd.none )

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


likertScale =
    [ "", "1 - strongly against", "2 - against", "3 - neutral", "4 - support", "5 - strongly support" ]


booleanAnswer =
    [ "", "yes", "no" ]


questionOptions =
    Dict.fromList
        [ ( "outcome", [ "", "unable to knock", "not home", "not interested", "meaningful conversation" ] )
        , ( "mp_support", likertScale )
        , ( "worth_returning", booleanAnswer )
        , ( "q3_enum", [ "", "ALP", "LIB", "GRN", "ONP", "other", "refused to say" ] )
        , ( "q4_boolean", booleanAnswer )
        , ( "key_issue", [ "", "Abortion", "Aged care", "Childcare", "Corp/High income tax", "Cost of living", "Donation disclosure", "Education funding", "Environment", "Healthcare funding", "Immigration", "Other", "Pension", "Unemployment" ] )
        ]


defaultQuestions : String -> List String
defaultQuestions question =
    Maybe.withDefault [] (Dict.get question questionOptions)


warringahQuestions : String -> List String
warringahQuestions question =
    let
        options =
            Dict.update "q3_enum" (\v -> Just likertScale) questionOptions
    in
    Maybe.withDefault [] (Dict.get question options)


datepickerSettings : DatePicker.Settings
datepickerSettings =
    { defaultSettings
        | inputClassList = [ ( "mdl-textfield__input", True ) ]
        , placeholder = ""
        , inputId = Just "date-field"
    }


answerOptions : String -> String -> String -> List (Html Msg)
answerOptions campaign question selectedValue =
    let
        questions =
            case campaign of
                "Dickson" ->
                    defaultQuestions

                "Warringah" ->
                    warringahQuestions

                _ ->
                    defaultQuestions
    in
    buildOptions (questions question) selectedValue


buildOptions : List String -> String -> List (Html Msg)
buildOptions options selectedValue =
    let
        answerOption value =
            option [ selected (value == selectedValue) ] [ text value ]
    in
    List.map answerOption options


campaignOptions : String -> List (Html Msg)
campaignOptions selectedCampaign =
    buildOptions [ "", "Dickson", "Warringah" ] selectedCampaign


validationMessage : BlockID -> String
validationMessage blockId =
    "Enter " ++ toString (11 - String.length blockId) ++ " more digits"


viewCanvases : Model -> List (Html Msg)
viewCanvases model =
    let
        rowWithOptionalHeader : Address -> ( List (Html Msg), String ) -> ( List (Html Msg), String )
        rowWithOptionalHeader address result =
            let
                previousRows =
                    Tuple.first result

                lastStreet =
                    Tuple.second result
            in
            if lastStreet /= address.street then
                ( previousRows ++ [ canvasHeader model.campaign address.street ] ++ [ viewCanvas model address ], address.street )

            else
                ( previousRows ++ [ viewCanvas model address ], address.street )
    in
    Tuple.first
        (List.foldl
            rowWithOptionalHeader
            ( [], "" )
            model.addresses
        )


canvasHeader : String -> String -> Html Msg
canvasHeader campaign street =
    let
        headers =
            case campaign of
                "Dickson" ->
                    [ street, "Outcome", "Dutton Support", "Return", "Voter ID", "Dutton last", "Key Issue", "Notes", "Last saved", "Actions" ]

                "Warringah" ->
                    [ street, "Outcome", "Abbott Support", "Return", "Independent Support", "Get involved", "Key Issue", "Notes", "Last saved", "Actions" ]

                _ ->
                    [ street, "Outcome", "MP Support", "Return", "Voter ID", "MP last", "Key Issue", "Notes", "Last saved", "Actions" ]

        headerRow : String -> Html Msg
        headerRow header =
            th [ class "mdl-data-table__cell--non-numeric" ] [ text header ]
    in
    tr [] <| List.map headerRow headers


viewCanvas : Model -> Address -> Html Msg
viewCanvas model address =
    let
        answerOptionsForCampaign =
            answerOptions model.campaign

        newSurvey =
            emptySurvey address.gnaf_pid model.blockId model.date

        survey =
            Maybe.withDefault newSurvey (Dict.get address.gnaf_pid model.canvas)

        disabledUnlessMeaningful =
            survey.responses.outcome /= "meaningful conversation"
    in
    tr []
        [ td [ class "mdl-data-table__cell--non-numeric" ]
            [ text (String.replace address.street "" address.address), br [] [], span [ class "small" ] [ text address.gnaf_pid ] ]
        , td [ class "mdl-data-table__cell--non-numeric" ]
            [ select [ onInput (UpdateOutcome survey) ] (answerOptionsForCampaign "outcome" survey.responses.outcome) ]
        , td [ class "mdl-data-table__cell--non-numeric" ]
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateMpSupport survey) ] (answerOptionsForCampaign "mp_support" survey.responses.mp_support) ]
        , td [ class "mdl-data-table__cell--non-numeric" ]
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateWorthReturning survey) ] (answerOptionsForCampaign "worth_returning" survey.responses.worth_returning) ]
        , td [ class "mdl-data-table__cell--non-numeric" ]
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateQ3Enum survey) ] (answerOptionsForCampaign "q3_enum" survey.responses.q3_enum) ]
        , td [ class "mdl-data-table__cell--non-numeric" ]
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateQ4Boolean survey) ] (answerOptionsForCampaign "q4_boolean" survey.responses.q4_boolean) ]
        , td [ class "mdl-data-table__cell--non-numeric" ]
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateKeyIssue survey) ] (answerOptionsForCampaign "key_issue" survey.responses.key_issue) ]
        , td [ class "mdl-data-table__cell--non-numeric" ]
            [ textarea [ disabled disabledUnlessMeaningful, onInput (UpdateNotes survey) ] [ text survey.responses.notes ] ]
        , td [ class "mdl-data-table__cell--non-numeric" ]
            [ span [ class "small" ] [ text (String.left 10 survey.updated_at) ] ]
        , td [ class "mdl-data-table__cell--non-numeric" ]
            [ button [ onClick (SaveSurvey survey), class "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored" ] [ text "Save" ] ]
        ]


statusMessage : Model -> String
statusMessage model =
    case model.status of
        "" ->
            "Enter the 11 digit Block ID"

        status ->
            status


ifThen : Bool -> String -> String
ifThen condition stringToAppend =
    if condition then
        " " ++ stringToAppend

    else
        ""


hiddenUnlessDate : Maybe Date -> String
hiddenUnlessDate maybeDate =
    case maybeDate of
        Nothing ->
            "hidden"

        Just _ ->
            "initial"


view : Model -> Html Msg
view model =
    let
        hiddenClass =
            ifThen (model.addresses == []) "hidden"

        loadingClass =
            ifThen (model.status /= "Loading..") "hidden"
    in
    div [ class "mdl-grid" ]
        [ div [ class "mdl-card mdl-card mdl-cell mdl-cell--9-col-desktop mdl-cell--6-col-tablet mdl-cell--4-col-phone mdl-shadow--2dp date-picker-cell" ]
            [ div [ class "mdl-card__supporting-text" ]
                [ div [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label", style "margin-right" "10px" ]
                    [ DatePicker.view model.date datepickerSettings model.datePicker
                        |> Html.map ToDatePicker
                    , label [ class "mdl-textfield__label" ] [ text "Select the date of the doorknock" ]
                    ]
                , div [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label", style "margin-right" "10px", style "visibility" (hiddenUnlessDate model.date) ]
                    [ select [ onInput UpdateCampaign, id "campaign", class "mdl-textfield__input" ] (campaignOptions model.campaign)
                    , label [ class "mdl-textfield__label", for "campaign" ] [ text "Campaign" ]
                    ]
                , div [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label", style "margin-right" "10px", style "visibility" (hiddenUnlessDate model.date) ]
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
                [ tbody [] (viewCanvases model) ]
            ]
        ]



--- Init --


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { campaign = ""
      , blockId = ""
      , status = ""
      , valid = Unknown
      , addresses = []
      , canvas = Dict.empty
      , date = Nothing
      , datePicker = datePicker
      }
    , Cmd.batch [ Cmd.map ToDatePicker datePickerFx ]
    )
