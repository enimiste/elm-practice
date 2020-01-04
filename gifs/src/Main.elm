module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D



-- TYPES


type alias Url =
    String


type alias Image =
    { url : Url, title : String }


type alias Model =
    { url : Url, gifs : List Image, status : Status }


type Status
    = Loading
    | Oops
    | Success


type Msg
    = NoOp
    | LoadRandomGif
    | GifLoadingResult (Result Http.Error Image)



-- STATE


url : Url
url =
    "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"


initState : Model
initState =
    Model url [] Success



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initState, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoadRandomGif ->
            loadGif model

        GifLoadingResult result ->
            case result of
                Ok img ->
                    ( { model | gifs = img :: model.gifs, status = Success }, Cmd.none )

                Err _ ->
                    ( { model | status = Oops }, Cmd.none )


loadGif : Model -> ( Model, Cmd Msg )
loadGif model =
    ( { model | status = Loading }, gifReq model )


gifReq : Model -> Cmd Msg
gifReq model =
    Http.get { url = model.url, expect = Http.expectJson GifLoadingResult gifDecoder }


gifDecoder : D.Decoder Image
gifDecoder =
    D.map2 Image
        (D.field "data" (D.field "image_url" D.string))
        (D.field "data" (D.field "title" D.string))



-- SUBS


subs : Model -> Sub Msg
subs model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "width" "100%" ] [ header, content model ]


header : Html msg
header =
    div
        [ style "margin-top" "-22px"
        , style "margin-bottom" "10px"
        , style "width" "100%"
        , style "background-color" "black"
        , style "height" "40px"
        , style "color" "white"
        ]
        [ h1 [] [ text "Gifs" ] ]


content : Model -> Html Msg
content model =
    let
        inner =
            case model.status of
                Loading ->
                    div [] [ p [] [ text "Loading ..." ], showGifs model ]

                Oops ->
                    div [] [ p [ style "color" "red" ] [ text "Somthing goes wrong !!" ], showGifs model ]

                Success ->
                    showGifs model
    in
    div [] [ moreButton, inner ]


showGifs : Model -> Html Msg
showGifs model =
    div
        [ style "display" "flex"
        , style "flex-wrap" "wrap"
        , style "margin-top" "10px"
        ]
        (List.map showGif model.gifs)


moreButton : Html Msg
moreButton =
    button [ onClick LoadRandomGif, style "cursor" "pointer" ] [ text "Show more !" ]


showGif : Image -> Html msg
showGif im =
    div
        [ style "width" "90px"
        , style "height" "100px"
        , style "padding" "0px 2px 0px 2px"
        , style "text-align" "center"
        ]
        [ img
            [ src im.url
            , alt ""
            , style "display" "block"
            , style "width" "100%"
            , style "height" "90px"
            ]
            []
        , p
            [ style "font-style" "italic"
            , style "font-size" "0.8em"
            , style "color" "gray"
            ]
            [ text im.title ]
        ]
