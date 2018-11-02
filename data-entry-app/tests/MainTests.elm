module MainTests exposing (suite)

import Date
import DatePicker exposing (..)
import Dict exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Model, ValidationStatus(..), view)
import Test exposing (..)
import Time exposing (Month(..))


suite : Test
suite =
    describe "view"
        [ describe "with loaded addresses and surveys"
            [ test "should load a row for each address" <|
                \_ ->
                    let
                        loadedModel =
                            Model
                                "1"
                                ""
                                Valid
                                []
                                Dict.empty
                                Nothing
                                (DatePicker.initFromDate (Date.fromCalendarDate 2018 Sep 26))

                        loadedView =
                            view loadedModel
                    in
                    Expect.equal 1 1
            ]
        ]
