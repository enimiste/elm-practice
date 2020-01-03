module Login.Style exposing (emailInput, formControl, formSignin, pwdInput)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)


formSignin : Attribute msg
formSignin =
    css [ width (pct 100), padding (px 15), maxWidth (px 330) ]


formControl : Attribute msg
formControl =
    css [ position relative, boxSizing borderBox, height auto, padding (px 10), fontSize (px 16), focus [ zIndex (int 2) ] ]


emailInput : Attribute msg
emailInput =
    css [ marginBottom (px -1), borderBottomRightRadius (px 0), borderBottomLeftRadius (px 0) ]


pwdInput : Attribute msg
pwdInput =
    css [ marginBottom (px 10), borderTopRightRadius (px 0), borderTopLeftRadius (px 0) ]
