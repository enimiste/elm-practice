module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- TYPES


type alias Url =
    String


type alias Model =
    { url : Url, gifs : List Url }


type Msg
    = NoOp



-- STATE


url : Url
url =
    "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=girl"


initState : Model
initState =
    Model url []



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subs }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initState, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBS


subs : Model -> Sub Msg
subs model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    a [ href model.url ] [ text model.url ]
