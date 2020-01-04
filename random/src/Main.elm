module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Svg
import Svg.Attributes as SA



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFace : Int
    , history : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model -1 []
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( { model | dieFace = newFace, history = newFace :: model.history }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (String.fromInt model.dieFace) ]
        , button [ onClick Roll ] [ text "Roll" ]
        , div [ style "margin-top" "10px" ] (showHistory model.history)
        ]


showHistory : List Int -> List (Html msg)
showHistory history =
    case history of
        x :: xs ->
            dieImg (Just 100) x :: List.map (dieImg (Just 90)) xs

        [] ->
            []


dieImgName : Int -> String
dieImgName n =
    "die-" ++ String.fromInt n ++ ".gif"


px : Float -> String
px f =
    String.fromFloat f ++ "px"


dieImg : Maybe Float -> Int -> Html msg
dieImg size n =
    let
        d =
            Maybe.withDefault 100 size |> px
    in
    img [ src ("img/" ++ dieImgName n), style "width" d, style "height" d, style "margin-left" "2px" ] []
