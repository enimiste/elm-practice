module Main exposing(..)

import Html exposing (Html, Attribute, text, div, a, pre, h3, h1, h5, select, option, button, table, tr, td, hr, br, span, p, thead, th, tbody, input)
import Html.Attributes exposing (style, attribute, value, selected, placeholder)
import Html.Events exposing (onClick, on, targetValue, onInput)
import Http
import Json.Decode as D
import Browser exposing (element)

-- SETTINGS
postsApiUrl : String
postsApiUrl = "https://jsonplaceholder.typicode.com/posts"

-- TYPES
type alias Post = { userId: Int, id: Int, title: String, content: String }
type alias Model = 
  {posts: List Post
  , error: String
  , postsCount: Int
  , loading: Bool
  , perPage: Int
  , page: Int
  , pagesCount: Int
  , query: Maybe String
  , selectedPost : Maybe Post
  }
type Msg = 
  NoOp 
  | PostsLoaded (Result Http.Error (List Post)) 
  | ShowPage Int
  | SetPageSize String
  | SetQuery String
  | LoadPosts
  | ShowDetail Post
  | CloseModal
  
-- MAIN
main = 
  element {init = init, update = update, view = view, subscriptions = subscriptions}
  
init: () -> (Model, Cmd Msg)
init _ = 
  let
    requestCmd = loadPosts postsApiUrl
  in
    (state, requestCmd)

-- MODEL
state: Model
state = Model [] "" 0 True 10 0 0 Nothing Nothing

-- CMDs
loadPosts: String -> Cmd Msg
loadPosts url = 
  Http.get {url = url, expect = Http.expectJson PostsLoaded postsListDecoder}
  
loadPostsWithQuery: String -> String -> Cmd Msg
loadPostsWithQuery url q = 
  Http.get {url = url ++ "?userId=" ++ q, expect = Http.expectJson PostsLoaded postsListDecoder}
  
-- DECODERS
postsListDecoder: D.Decoder (List Post)
postsListDecoder = 
  D.list postDecoder
  
postDecoder: D.Decoder Post
postDecoder =
  D.map4 Post
  (D.field "userId" D.int)
  (D.field "id" D.int)
  (D.field "title" D.string)
  (D.field "body" D.string)

-- UPDATE
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> 
      (model, Cmd.none)
    PostsLoaded (Ok posts) ->
      let
        total = List.length posts
        page = if total > 0 then 1 else 0
      in
        ({model | posts = posts, page = page, postsCount = total, pagesCount = (pageCount model.perPage total), error = "", loading = False}, Cmd.none)
    PostsLoaded (Err err) -> 
      updateHttpErr model err
    ShowPage page -> 
      ({model | page = page}, Cmd.none)
    SetPageSize pageSize -> 
      case (String.toInt pageSize) of
        Just x -> 
          let
            pagesCount = pageCount x model.postsCount
            page = if model.page > pagesCount then pagesCount else model.page
          in
            ({model | perPage = x, page = page, pagesCount = pagesCount}, Cmd.none)
        Nothing -> (model, Cmd.none)
    SetQuery q ->
      if (q |> String.trim |> String.isEmpty) then 
        ({model | query = Nothing}, Cmd.none)
      else
        ({model | query = Just q}, Cmd.none)
    LoadPosts ->
      case model.query of 
        Nothing ->
          reloadPosts {model | query = Nothing} (loadPosts postsApiUrl)
        Just q ->
          reloadPosts {model | query = Just q} (loadPostsWithQuery postsApiUrl q)
    ShowDetail post ->
      ({model | selectedPost = Just post}, Cmd.none)
    CloseModal -> ({model | selectedPost = Nothing}, Cmd.none)
          
reloadPosts: Model -> Cmd Msg -> (Model, Cmd Msg)
reloadPosts model cmd =
  ({model | posts = [], page = 0, postsCount = -1, pagesCount = 0, error = "", loading = True}, cmd)
    
updateHttpErr: Model -> Http.Error -> (Model, Cmd Msg)
updateHttpErr model err =
  let
    errMsg = 
      case err of
        Http.BadUrl s -> s
        Http.Timeout -> "TimeOut"
        Http.NetworkError -> "Network Error"
        Http.BadStatus status -> (String.fromInt status)
        Http.BadBody bb -> bb
  in
    ({model | error = errMsg, posts = [], page = 0, postsCount = -1, pagesCount = 0}, Cmd.none)
    
-- SUBSCRIPTIONS
subscriptions: Model -> Sub Msg
subscriptions _ = Sub.none
    
-- VIEW
view: Model -> Html Msg
view model =
  div [] 
  [
    showModal model.selectedPost
    ,div [] [h1 [] [text "Posts List"]]
    , div [] 
    [
      table []
      [
        tr []
        [
          td [style "width" "70%"] [h5 [] [text (" posts." |> (++) (String.fromInt model.postsCount))], span [style "color" "red"] [text model.error]]
          ,td [] [div [style "width" "200px"] [searchInput model.query, button [onClick LoadPosts] [text "Go!"]]]
        ]
      ]
      , hr [] []
      , if model.loading then (text "Loading ...")
        else if model.postsCount == 0 then (text "Empty List")
        else postsTable (postsPage model.perPage model.page model.posts) model.perPage model.page model.pagesCount
    ]
  ]

showModal: Maybe Post -> Html Msg
showModal mpost =
  case mpost of
    Just post -> modal post
    Nothing -> div [] []
    
postsPage: Int -> Int -> (List Post) -> (List Post)
postsPage perPage page posts =
  let
    drop = (page - 1) * page
  in
    posts |> List.drop drop |> List.take perPage
  
pagingText: Int -> Int -> Int -> String
pagingText perPage page pagesCount = 
  "page " ++ (String.fromInt page) ++ " of " ++ (String.fromInt pagesCount)
  
pageCount: Int -> Int -> Int
pageCount perPage total =
  let
    x = toFloat perPage
    y = toFloat total
  in
    ceiling (y/x)

postsTable: List Post -> Int -> Int -> Int -> Html Msg
postsTable posts perPage page pagesCount =
  div []
  [
    table []
    [
      tr []
      [
        td [style "width" "70%"] [p [style "font-style" "italic", style "color" "gray"] [text (pagingText perPage page pagesCount)]]
      ]
    ]
    , pagingButtons perPage page pagesCount
    ,table [style "border" "1px solid black", style "width" "100%"]
      [
        thead [] 
        [
          tr []
          [
            th [] [text "User ID"]
            , th [] [text "ID"]
            , th [] [text "Title"]
          ]
        ]
        , tbody [] (posts |> List.take perPage |> List.map viewPostRaw)
      ]
    , p [] [text "End of page"]
  ]
  
viewPostRaw: Post -> Html Msg
viewPostRaw post =
    tr [] 
    [
      td [] [text (String.fromInt post.userId)]
      , td [] [text (String.fromInt post.id)]
      , td [style "cursor" "pointer", onClick (ShowDetail post)] [text post.title]
    ]
  
searchInput: Maybe String -> Html Msg
searchInput query =
  input [value (Maybe.withDefault "" query)
        , onInput SetQuery, placeholder "filter by userId"
        , style "width" "60%"] []
  
pagingButtons: Int -> Int -> Int -> Html Msg
pagingButtons perPage page pagesCount =
  div [style "margin-bottom" "10px", style "height" "20px"] 
  [
    div [style "float" "left"] ((List.range 1 pagesCount) |> List.map (pagingButton page))
    , div [style "float" "right", style "text-align" "right", style "width" "100px"] [pageSizeChoices perPage]
  ]
  
onChange: (String -> msg) -> Attribute msg
onChange  tagged =
  on "change" (D.map tagged targetValue)
  
pageSizeChoices: Int -> Html Msg
pageSizeChoices perPage =
  let
    makeOption c i = 
      option [value (String.fromInt i), selected (c == i)] 
              [text ("Page " ++ (String.fromInt i))]
  in
    select [onChange SetPageSize] ([5, 10, 20, 50, 100] |> List.map (makeOption perPage))
  
pagingButton: Int -> Int -> Html Msg
pagingButton curPage page =
  let
    st = if curPage == page then (style "font-size" "1.5em") else (style "" "")
  in
    a [onClick (ShowPage page), st, style "padding-right" "10px", style "color" "blue", style "cursor" "pointer"] 
    [text (String.fromInt page)]
     
  
 
 -- MODAL
modalMaskAttrs : List (Attribute msg)
modalMaskAttrs =
    [ ("background-color", "rgba(0,0,0,0.3)")
    , ("position", "fixed")
    , ("top", "0")
    , ("left", "0")
    , ("width", "100%")
    , ("height", "100%")
    ] |> List.map (\(a, b) -> style a b)
 
modalAttrs : List (Attribute msg)
modalAttrs =
    [ ("background-color", "rgba(255,255,255,1.0)")
    , ("position", "absolute")
    , ("top", "50%")
    , ("left", "50%")
    , ("height", "auto")
    , ("max-height", "80%")
    , ("width", "700px")
    , ("max-width", "95%")
    , ("padding", "10px")
    , ("border-radius", "3px")
    , ("box-shadow", "1px 1px 5px rgba(0,0,0,0.5)")
    , ("transform", "translate(-50%, -50%)")
    ] |> List.map (\(a, b) -> style a b)

modalCloseBtnAttrs: List (Attribute Msg)
modalCloseBtnAttrs =
  [
    onClick CloseModal
  ] ++
  ([("cursor", "pointer")
    , ("position", "relative")
    , ("top", "-62px")
    , ("left", "540px")
  ] |> List.map (\(a, b) -> style a b))
  
modal : Post -> Html Msg
modal post =
  div []
    [
      div modalMaskAttrs
        [ div modalAttrs
          [ 
            h3 [] [ text post.title ]
            , span modalCloseBtnAttrs [text "X"]
            , hr [] []
            , pre [] [text post.content ]
          ]
        ]
    ]