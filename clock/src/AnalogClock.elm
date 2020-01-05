module AnalogClock exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Task
import Time


type alias Model =
    { zone : Time.Zone, time : Maybe Time.Posix }


type Msg
    = NoOp
    | Tick Time.Posix
    | SetLocalTimeZone Time.Zone


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc Nothing, Task.perform SetLocalTimeZone Time.here )


subs : Model -> Sub Msg
subs model =
    Time.every 1000 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick p ->
            ( { model | time = Just p }, Cmd.none )

        SetLocalTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )


humain : Time.Zone -> Maybe Time.Posix -> Maybe ( Int, Int, Int )
humain zone mtime =
    case mtime of
        Nothing ->
            Nothing

        Just time ->
            let
                hour =
                    Time.toHour zone time

                minutes =
                    Time.toMinute zone time

                seconds =
                    Time.toSecond zone time
            in
            Just ( hour, minutes, seconds )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ HA.style "margin-left" "100px" ] [ analogClock (humain model.zone model.time) 400 400 ]


digitalClock : Maybe ( Int, Int, Int ) -> Html msg
digitalClock x =
    case x of
        Just ( h, m, ss ) ->
            Html.p [] [ text (s h ++ ":" ++ s m ++ ":" ++ s ss) ]

        Nothing ->
            Html.p [] []


s : Int -> String
s x =
    String.fromInt x


sf : Float -> String
sf x =
    String.fromFloat x


vb : ( Int, Int ) -> ( Int, Int ) -> String
vb ( x, y ) ( w, h ) =
    s x ++ " " ++ s y ++ " " ++ s w ++ " " ++ s h


analogClock : Maybe ( Int, Int, Int ) -> Int -> Int -> Svg msg
analogClock time w h =
    case time of
        Nothing ->
            svg [] []

        Just ( hr, mn, ss ) ->
            let
                ctx =
                    w // 2

                cty =
                    h // 2

                ry =
                    ctx
            in
            svg [ width (s w), height (s w), viewBox (vb ( 0, 0 ) ( w, h )) ]
                [ circle [ cx (s ctx), cy (s cty), r (s ry), SA.style "fill:beige;stroke:gray" ] []
                , circle [ cx (s ctx), cy (s cty), r "7", SA.style "fill:black;stroke:black" ] []
                , g []
                    (List.range 0 12
                        |> List.map toFloat
                        |> List.map (ticks ( 0, 12 ) ( ctx, cty ) ry 1 5)
                    )
                , g []
                    (List.range 0 60
                        |> List.filter (notDivBy 5)
                        |> List.map toFloat
                        |> List.map (ticks ( 0, 60 ) ( ctx, cty ) ry 0.5 1)
                    )
                , bare ( 0, 12 ) ( ctx, cty ) ry 0.5 5 (toFloat hr)
                , bare ( 0, 60 ) ( ctx, cty ) ry 0.7 3 (toFloat mn)
                , bare ( 0, 60 ) ( ctx, cty ) ry 0.9 1 (toFloat ss)
                ]


notDivBy : Int -> Int -> Bool
notDivBy d i =
    modBy d i /= 0


bare : ( Float, Float ) -> ( Int, Int ) -> Int -> Float -> Int -> Float -> Svg msg
bare ( min, max ) ( ctx, cty ) ry pct sw v =
    let
        theta =
            (*) 360 <| v / (max - min)

        r =
            pct * toFloat ry

        x =
            toFloat ctx + (*) r (sin (degrees theta))

        y =
            toFloat cty - (*) r (cos (degrees theta))
    in
    line [ x1 (s ctx), y1 (s cty), x2 (sf x), y2 (sf y), SA.style ("stroke:black;stroke-width:" ++ s sw) ] []


ticks : ( Float, Float ) -> ( Int, Int ) -> Int -> Float -> Int -> Float -> Svg msg
ticks ( min, max ) ( ctx, cty ) ry pct sw v =
    let
        theta =
            (*) 360 <| v / (max - min)

        r =
            toFloat ry

        x_2 =
            toFloat ctx + (*) r (sin (degrees theta))

        y_2 =
            toFloat cty - (*) r (cos (degrees theta))

        dr =
            r * 0.1 * pct

        dx =
            (*) dr (sin (degrees theta))

        dy =
            (*) dr (cos (degrees theta))

        x_1 =
            x_2 - dx

        y_1 =
            y_2 + dy
    in
    line [ x1 (sf x_1), y1 (sf y_1), x2 (sf x_2), y2 (sf y_2), SA.style ("stroke:black;stroke-width:" ++ s sw) ] []
