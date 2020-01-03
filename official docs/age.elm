import Html exposing (Html, div, text, br, input, span)
import Html.Events exposing (onInput)
import Html.Attributes exposing (style, value)
import Browser exposing (sandbox)


type alias Model = {age: Int, error: String}
type Msg = AgeInputed String

-- Model
init: Model
init = Model 20 ""

-- Update
update: Msg -> Model -> Model
update msg model =
  case msg of
    AgeInputed v -> 
      let
        x = checkAge v
      in
        case x of
          Ok y -> {model | age = y, error = ""}
          Err er -> {model | age = 0, error = er}
    
-- View
view: Model -> Html Msg
view model = 
  div []
  [
    input [value (String.fromInt model.age), onInput AgeInputed] []
    , br [] []
    ,text (model.age |> String.fromInt |> ((++) "Age ")) 
    , br [] []
    , span [style "color" "red"] [text model.error]
  ]

main =
  sandbox {init=init, view=view, update=update}


checkAge: String -> Result String Int
checkAge age =
  case String.toInt age of
    Nothing -> Err "That is not a number !"
    Just v ->
      if v < 0 then
        Err "Please try again after you are born."
      else if v > 135 then
        Err "Are you some kind of turtle?"
      else
        Ok v
    