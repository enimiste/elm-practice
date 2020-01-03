module Login exposing (..)

import Browser
import Css exposing (..)
import Html
import Html.Attributes as HA
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events exposing (..)
import Login.Style exposing (emailInput, formControl, formSignin, pwdInput)



-- INSTALL :
-- npm install -g elm-format
-- npm install -G http-server
-- BUILD :
-- elm-format --yes src/ && elm make src/Main.elm --output=build/app.js
-- HTTP SERVER :
-- http-server -p 9999


main =
    Browser.sandbox
        { view = view >> toUnstyled
        , update = update
        , init = initialModel
        }



-- MODEL


type alias Model =
    {}


initialModel : Model
initialModel =
    {}



-- UPDATE


update : msg -> Model -> Model
update _ model =
    model



-- VIEW


view : Model -> Html msg
view _ =
    HS.form formAttrs
        [ img logoAttrs []
        , h1 [ class "h3 mb-3 font-weight-normal" ] [ text "Please sign in" ]
        , label [ for "inputEmail", class "sr-only" ] [ text "Email address" ]
        , input emailInputAttrs []
        , label [ for "inputPwd", class "sr-only" ] [ text "Password" ]
        , input pwdInputAttrs []
        , div [ class "checkbox mb-3" ] [ label [] [ input [ type_ "checkbox", value "remember-me", css [ fontWeight (int 400) ] ] [], text " Remember Me" ] ]
        , button [ type_ "submit", class "btn btn-lg btn-primary btn-block" ] [ text "Sign in" ]
        , p [ class "mt-5 mb-3 text-muted" ] [ text "© 2017-2019" ]
        ]


formAttrs : List (Attribute msg)
formAttrs =
    [ formSignin ]


logoAttrs : List (Attribute msg)
logoAttrs =
    [ class "mb-4"
    , src "https://getbootstrap.com/docs/4.4/assets/brand/bootstrap-solid.svg"
    , alt ""
    , HSA.width 72
    , HSA.height 72
    ]


emailInputAttrs : List (Attribute msg)
emailInputAttrs =
    [ formControl, emailInput ]
        ++ [ id "inputEmail"
           , type_ "email"
           , class "form-control"
           , placeholder "Email address"
           , HSA.required True
           , autofocus True
           ]


pwdInputAttrs : List (Attribute msg)
pwdInputAttrs =
    [ formControl, pwdInput ]
        ++ [ id "inputPwd"
           , type_ "password"
           , class "form-control"
           , placeholder "Password"
           , HSA.required True
           ]
