module Main exposing (..)

import Http
import Json.Decode exposing (Decoder, field, string, map2, map3, map4, map5)

import Browser
import Browser.Navigation exposing (..)
import Url
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Url.Parser as P exposing ((</>))
import Http exposing (multipartBody)
import Http exposing (stringPart)

main : Program () Model Msg
main =
  Browser.application { 
    init = init, 
    subscriptions = (\_ -> Sub.none), 
    update = update, 
    view = view,
    onUrlRequest = LinkClinked,
    onUrlChange = UrlChanged
    }

type alias Category = {
  id: String,
  name: String
  }

type alias Post = {
  id: String,
  category: Category,
  title: String,
  subtext: String
  }

type Route
  = Home
  | CategoriesView
  | CategoryView String
  | PostView String
  | LoginView
  | LogoutView
  | CreatePostView

routeParser : P.Parser (Route -> a) a
routeParser =
  P.oneOf
    [ P.map Home P.top
    , P.map CategoriesView (P.s "categories")
    , P.map CategoryView (P.s "category" </> P.string)
    , P.map PostView (P.s "post" </> P.string)
    , P.map LoginView (P.s "login")
    , P.map LogoutView (P.s "logout")
    , P.map CreatePostView (P.s "create")
    ]

processPostListing : Decoder (List Post)
processPostListing = 
  Json.Decode.list (
    map5(\a->\b->\c->\d->\e-> Post a (Category b c) d e) (field "id" string) (field "catid" string) (field "category" string) (field "title" string) (field "subtext" string)
  )

processCategoryListing : Decoder (List Category)
processCategoryListing =
  Json.Decode.list (map2 Category (field "id" string) (field "name" string))

processPost : Decoder (String, String, String)
processPost = 
  map3 (\a -> \b -> \c -> (a,b,c)) (field "title" string) (field "content" string) (field "category" string)

processCategory : Decoder (String, List Post)
processCategory =
  map2 (\a -> \b -> (a,b)) (field "category" string) (field "posts" processPostListing)

processLogin : Decoder (String)
processLogin =
  (field "username" string)
messageOfRoute : Maybe Route -> Cmd Msg
messageOfRoute r =
  case r of
    Just route -> 
      case route of
        Home -> Http.get {url = "/v1/posts", expect = Http.expectJson GotPosts processPostListing}
        CategoriesView -> Http.get {url = "/v1/categories", expect = Http.expectJson GotCategories processCategoryListing}
        PostView p -> Http.get {url = "/v1/post/" ++ p, expect = Http.expectJson GotPost processPost}
        CategoryView c -> Http.get {url = "/v1/category/" ++ c, expect = Http.expectJson GotCategory processCategory}
        _ -> Cmd.none
    Nothing -> Cmd.none

type alias User = {
  username: String
  }
type alias Model = {
  header: String,
  content: String,
  footer: String,
  pinnedPosts: List Post,
  posts: List Post,
  route: Maybe Route,
  key: Browser.Navigation.Key,
  errMessage: Maybe (String),
  username: String,
  password: String,
  user: Maybe User
  }

type Msg
  = LinkClinked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotPosts (Result Http.Error (List Post))
  | GotCategories (Result Http.Error (List Category))
  | GotPost (Result Http.Error (String, String, String))
  | GotCategory (Result Http.Error (String, List Post))
  | LoginResult (Result Http.Error String)
  | Username String
  | Password String
  | Login

init : () -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ url key =
  let r = P.parse routeParser url in
  ({
      header = "", content = "", footer = "", pinnedPosts = [], 
      posts = [], route = r, key = key, errMessage = Nothing,
      username = "", password = "", user = Nothing
    },
    messageOfRoute r
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClinked req ->
      case req of
        Browser.Internal url -> (model, Browser.Navigation.pushUrl model.key (Url.toString url))
        Browser.External url -> (model, Browser.Navigation.load url)
    UrlChanged req ->
      (
        let r = P.parse routeParser req in
        ({model | route = r}, messageOfRoute r)
      )
    GotPosts (Ok l) -> ({model | errMessage = Nothing, posts = l}, Cmd.none)
    GotPosts (Err _) -> ({model | errMessage = Just "Failed to load posts"}, Cmd.none)
    GotPost (Ok (title, content, category)) -> ({model | errMessage = Nothing, header = title, content = content, footer = category}, Cmd.none)
    GotCategory (Ok (title, posts)) -> ({model | errMessage = Nothing, header = title, posts = posts}, Cmd.none)
    Username u -> ({model | username = u}, Cmd.none)
    Password p -> ({model | password = p}, Cmd.none)
    Login -> (model, handleLogin model)
    _ -> ({model | errMessage = Just "Something has gone horribly wrong."}, Cmd.none)

handleLogin : Model -> Cmd Msg
handleLogin model =
  Http.post {url = "/v1/login", body = multipartBody [stringPart "username" model.username, stringPart "password" model.password], expect = Http.expectJson LoginResult processLogin}

type alias Document msg = {
  title: String,
  body: List (Html msg)
  }

view : Model -> Document Msg
view model =
  {title = "XNOEBLOG", body = [htmlView model]}

htmlView : Model -> Html Msg
htmlView model =
  div [
    style "flex-direction" "column",
    style "margin" "0",
    style "padding" "0"
  ] [
    div [
      style "border-bottom" "2px solid black",
      style "height" "1.5em",
      style "padding" "5px"
    ] ([
      a [href "/"] [text "Home"],
      text " - "
    ] ++
      case model.user of 
        Nothing -> [a [href "/login"] [text "Login"]]
        Just u -> [a [href "/logout"] [text "Logout"], text " - ", a [href "/create"] [text "Create Post"]]
    ),
    renderModel model
  ]
renderModel : Model -> Html Msg
renderModel model =
  case (model.errMessage) of
    Just e -> h1 [] [text (e)]
    Nothing ->
      case model.route of
        Just route -> case route of
          Home -> div [] [h1 [] [text "Welcome to my blog."],cardListing model]
          PostView _ -> div [] [h1 [] [text (model.header)], p [] [text(model.content)], h3 [] [text(model.footer ++ " ")]]
          CategoryView _ -> div [] [h1 [] [text (model.header)], cardListing model]
          LoginView -> loginForm model
          _ -> h1 [] [ text ("Not found."), a [ href "/"] [text("Return home")] ]
        _ -> h1 [] [ text ("Not found."), a [ href "/"] [text("Return home")] ]
cardListing : Model -> Html Msg
cardListing model =
  div [
    style "display" "grid",
    style "grid-template-columns" "repeat(auto-fill, minmax(16rem, 1fr))"
    ] (List.map renderPost model.posts)

renderPost : Post -> Html Msg
renderPost post =
  div [
    style "max-width" "32rem",
    style "height" "auto",
    style "box-sizing" "border-box",
    style "padding" "15px"
  ] [
    div [
      style "flex-direction" "column",
      style "border-radius" "5px",
      style "display" "flex",
      style "height" "100%",
      style "border" "2px solid black",
      style "padding" "5px"
      ] [
        header [
          style "min-height" "75px",
          style "display" "flex",
          style "flex-direction" "column",
          style "align-items" "center",
          style "box-shadow" "none",
          style "box-sizing" "border-box",
          style "font-size" "20px",
          style "font-weight" "bold",
          style "border-radius" "10px 10px 0 0"
        ] [
          a [style "text-align" "center", href ("/post/" ++ post.id)] [text (post.title)]
        ],
        div [
          style "flex-grow" "1",
          style "padding" "10px"
        ] [
          text (post.subtext)
        ],
        footer [
          style "min-height" "50px",
          style "display" "flex",
          style "flex-direction" "row",
          style "justify-content" "space-around",
          style "align-items" "center",
          style "box-shadow" "none",
          style "box-sizing" "border-box",
          style "padding" "10px",
          style "border-radius" "0 0 10px 10px"
        ] [
          text ("Category: "), a [href ("/category/" ++ post.category.id)] [text (post.category.name)]
        ]
      ]
  ]

loginForm : Model -> Html Msg
loginForm model =
  div [] [
    viewInput "text" "username" "Username" model.username Username,
    viewInput "password" "password" "Password" model.password Password,
    button [onClick Login] [text "Login"]
  ]

viewInput : String -> String -> String -> String -> (String -> msg) -> Html msg
viewInput t n p v toMsg =
  input [ name n, type_ t, placeholder p, value v, onInput toMsg ] []