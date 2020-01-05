module DigitalStyledClock exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Maybe Time.Posix
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc Nothing False
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Pause Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( if model.paused then
                model

              else
                { model | time = Just newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        Pause b ->
            ( { model | paused = b }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ clock model.time model.zone
        , button [ onClick (Pause (not model.paused)) ]
            [ text
                (if model.paused then
                    "resume"

                 else
                    "pause"
                )
            ]
        ]


clock : Maybe Time.Posix -> Time.Zone -> Html msg
clock mtime zone =
    case mtime of
        Nothing ->
            p clockStyles [ text "__:__:__" ]

        Just time ->
            let
                hour =
                    String.fromInt (Time.toHour zone time)

                minute =
                    String.fromInt (Time.toMinute zone time)

                second =
                    String.fromInt (Time.toSecond zone time)
            in
            p clockStyles [ text (s hour ++ ":" ++ s minute ++ ":" ++ s second) ]


s : String -> String
s x =
    String.padLeft 2 '0' x


clockStyles : List (Attribute msg)
clockStyles =
    [ style "font-style" "bold"
    , style "font-size" "2em"
    , style "color" "blue"
    ]
