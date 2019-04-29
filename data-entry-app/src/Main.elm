module Main exposing (Model, ValidationStatus(..), view)

import Browser
import Browser.Navigation
import Date exposing (Date, day, month, today, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Debug exposing (toString)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, andThen, at, dict, field, int, list, map3, map5, map8, string)
import Json.Encode
import Task
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query


main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



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
    , mp_support_before : SurveyResponse
    , mp_support_after : SurveyResponse
    , get_involved : SurveyResponse
    , name : SurveyResponse
    , phone : SurveyResponse
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
        , mp_support_before = ""
        , mp_support_after = ""
        , get_involved = "no"
        , name = ""
        , phone = ""
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
    map8
        SurveyResponses
        (field "outcome" string)
        (field "mp_support_before" string)
        (field "mp_support_after" string)
        (field "get_involved" string)
        (field "name" string)
        (field "phone" string)
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


canvasToJson : Canvas -> Json.Encode.Value
canvasToJson canvas =
    Json.Encode.dict identity surveyToJson canvas


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
                        , ( "mp_support_before", Json.Encode.string survey.responses.mp_support_before )
                        , ( "mp_support_after", Json.Encode.string survey.responses.mp_support_after )
                        , ( "get_involved", Json.Encode.string survey.responses.get_involved )
                        , ( "name", Json.Encode.string survey.responses.name )
                        , ( "phone", Json.Encode.string survey.responses.phone )
                        , ( "key_issue", Json.Encode.string survey.responses.key_issue )
                        , ( "notes", Json.Encode.string survey.responses.notes )
                        ]
                  )
                ]



-- HTTP --


upsertSurvey : Survey -> Cmd Msg
upsertSurvey survey =
    Http.post
        { url = Url.Builder.crossOrigin apiBase [ "prod", "survey" ] []
        , body = Http.jsonBody (surveyToJson survey)
        , expect = Http.expectJson LoadSurvey surveyDecoder
        }


upsertCanvas : Canvas -> Cmd Msg
upsertCanvas canvas =
    Http.post
        { url = Url.Builder.crossOrigin apiBase [ "prod", "surveys" ] []
        , body = Http.jsonBody (canvasToJson canvas)
        , expect = Http.expectJson LoadSurveys surveysDecoder
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
    Url.Builder.crossOrigin apiBase [ "prod", "addresses" ] [ Url.Builder.string "slug" id, Url.Builder.string "survey_on" (Date.toIsoString survey_on) ]



-- Update --


validateAndGetAddresses : Model -> ( Model, Cmd Msg )
validateAndGetAddresses model =
    case model.date of
        Nothing ->
            ( model, Cmd.none )

        Just existingDate ->
            if String.length model.blockId == 11 then
                ( { model | valid = Valid, status = "Loading..", addresses = [] }, getAddressesForBlockId model.blockId existingDate )

            else
                ( { model | valid = Invalid, status = validationMessage model.blockId, addresses = [] }, Cmd.none )


type Msg
    = UpdateCampaign String
    | UpdateBlockID String
    | LoadAddresses (Result Http.Error AddressAndCanvasData)
    | ToDatePicker DatePicker.Msg
    | SetDate (Maybe Date)
    | UpdateOutcome Survey String
    | UpdateMpSupportBefore Survey String
    | UpdateMpSupportAfter Survey String
    | UpdateGetInvolved Survey String
    | UpdateName Survey String
    | UpdatePhone Survey String
    | UpdateKeyIssue Survey String
    | UpdateNotes Survey String
    | SaveSurvey Survey
    | SaveCanvas Canvas
    | LoadSurvey (Result Http.Error Survey)
    | LoadSurveys (Result Http.Error Canvas)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


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
            validateAndGetAddresses { model | blockId = newBlockId }

        UpdateOutcome survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | outcome = newValue }), Cmd.none )

        UpdateMpSupportBefore survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | mp_support_before = newValue }), Cmd.none )

        UpdateMpSupportAfter survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | mp_support_after = newValue }), Cmd.none )

        UpdateGetInvolved survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | get_involved = newValue }), Cmd.none )

        UpdateName survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | name = newValue }), Cmd.none )

        UpdatePhone survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | phone = newValue }), Cmd.none )

        UpdateKeyIssue survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | key_issue = newValue }), Cmd.none )

        UpdateNotes survey newValue ->
            ( updateModelWithSurveyResponse model survey (\r -> { r | notes = newValue }), Cmd.none )

        SaveSurvey survey ->
            ( model, upsertSurvey survey )

        SaveCanvas canvas ->
            ( model, upsertCanvas canvas )

        UrlChanged url ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            ( model, Cmd.none )

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

        LoadSurveys result ->
            case result of
                Ok newCanvas ->
                    ( { model | canvas = newCanvas }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        SetDate date ->
            validateAndGetAddresses { model | date = date }

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
        , ( "mp_support_before", likertScale )
        , ( "mp_support_after", likertScale )
        , ( "get_involved", booleanAnswer )
        , ( "key_issue", [ "", "Abortion", "Aged care", "Childcare", "Corp/High income tax", "Cost of living", "Donation disclosure", "Education funding", "Environment", "Healthcare funding", "Immigration", "Other", "Pension", "Unemployment" ] )
        ]


defaultQuestions : String -> List String
defaultQuestions question =
    Maybe.withDefault [] (Dict.get question questionOptions)


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
                    defaultQuestions

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
    li [ class "mdl-list__item" ] [ text street ]


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
    li [ class "mdl-list__item mdl-list__item--two-line" ]
        [ span [ class "mdl-list__item-primary-content" ]
            [ i [ class "material-icons mdl-list__item-icon" ] [ text "thumb_down" ]
            , text (String.replace address.street "" address.address)
            ]
        , span [ class "mdl-list__item-secondary-content" ]
            [ select [ onInput (UpdateOutcome survey), class "mdl-list__item-secondary-action" ] (answerOptionsForCampaign "outcome" survey.responses.outcome)
            ]
        , div [ class "hidden" ]
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateMpSupportBefore survey) ] (answerOptionsForCampaign "mp_support_before" survey.responses.mp_support_before) ]
        , div [ class "hidden" ]
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateMpSupportAfter survey) ] (answerOptionsForCampaign "mp_support_after" survey.responses.mp_support_after) ]
        , div [ class "hidden" ]
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateGetInvolved survey) ] (answerOptionsForCampaign "get_involved" survey.responses.get_involved) ]
        , div [ class "hidden" ]
            [ input [ disabled disabledUnlessMeaningful, onInput (UpdateName survey) ] [ text survey.responses.name ] ]
        , div [ class "hidden" ]
            [ input [ disabled disabledUnlessMeaningful, onInput (UpdatePhone survey) ] [ text survey.responses.phone ] ]
        , div [ class "hidden" ]
            [ select [ disabled disabledUnlessMeaningful, onInput (UpdateKeyIssue survey) ] (answerOptionsForCampaign "key_issue" survey.responses.key_issue) ]
        , div [ class "hidden" ]
            [ textarea [ disabled disabledUnlessMeaningful, onInput (UpdateNotes survey) ] [ text survey.responses.notes ] ]
        , div [ class "hidden" ]
            [ span [ class "small" ] [ text (String.left 10 survey.updated_at) ] ]
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


view : Model -> Browser.Document Msg
view model =
    { title = "Neightbourly Data Entry App", body = [ viewBody model ] }


viewBody : Model -> Html Msg
viewBody model =
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
                , div [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label", style "margin-right" "10px" ]
                    [ select [ onInput UpdateCampaign, id "campaign", class "mdl-textfield__input" ] (campaignOptions model.campaign)
                    , label [ class "mdl-textfield__label", for "campaign" ] [ text "Campaign" ]
                    ]
                , div [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label", style "margin-right" "10px" ]
                    [ input [ onInput UpdateBlockID, value model.blockId, id "block-id", class "mdl-textfield__input", type_ "text" ] []
                    , label [ class "mdl-textfield__label", for "block-id" ] [ text (statusMessage model) ]
                    ]
                , div [ class ("mdl-progress mdl-js-progress mdl-progress__indeterminate" ++ loadingClass), style "width" "100%" ] []
                ]
            ]
        , div [ class "mdl-cell" ]
            [ ul
                [ class ("mdl-list" ++ hiddenClass) ]
                (viewCanvases model)
            ]
        , div [ class "mdl-cell" ]
            [ button [ onClick (SaveCanvas model.canvas), class "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored" ] [ text "Save" ] ]
        ]



--- Init --


type alias InitQuery =
    { blockid : Maybe String, campaign : Maybe String }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init

        _ =
            Debug.log "url: " url

        parseParam : String -> String
        parseParam param =
            Maybe.withDefault "" (Maybe.withDefault Nothing (Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string param)) url))
    in
    ( { campaign = parseParam "campaign"
      , blockId = parseParam "blockid"
      , status = ""
      , valid = Unknown
      , addresses = []
      , canvas = Dict.empty
      , date = Nothing
      , datePicker = datePicker
      }
    , Cmd.batch [ Cmd.map ToDatePicker datePickerFx ]
    )
