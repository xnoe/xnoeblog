module Main exposing (..)

import Http
import Json.Decode exposing (Decoder, field, string, map2, map3, map4)

import Browser
import Browser.Navigation exposing (..)
import Url
import Html exposing (..)
import Html.Attributes exposing (..)

import Url.Parser as P exposing ((</>))

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

type alias Post = {
  id: String,
  category: String,
  title: String,
  subtext: String
  }

type alias Category = {
  id: String,
  name: String
  }

type alias Model = {
  header: String,
  content: String,
  footer: String,
  pinnedPosts: List Post,
  posts: List Post,
  route: Maybe Route,
  key: Browser.Navigation.Key,
  errMessage: Maybe (String)
  }

type Route
  = Home
  | CategoriesView
  | CategoryView String
  | PostView String
  | Login
  | Logout
  | CreatePost

routeParser : P.Parser (Route -> a) a
routeParser =
  P.oneOf
    [ P.map Home P.top
    , P.map CategoriesView (P.s "categories")
    , P.map CategoryView (P.s "category" </> P.string)
    , P.map PostView (P.s "post" </> P.string)
    , P.map Login (P.s "login")
    , P.map Logout (P.s "logout")
    , P.map CreatePost (P.s "create")
    ]

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

init : () -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ url key =
  let r = P.parse routeParser url in
  ({
      header = "", content = "", footer = "", pinnedPosts = [], 
      posts = [], route = r, key = key, errMessage = Just "Loading Posts..."
    },
    messageOfRoute r
  )

type Msg
  = LinkClinked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotPosts (Result Http.Error (List Post))
  | GotCategories (Result Http.Error (List Category))
  | GotPost (Result Http.Error (String, String, String))
  | GotCategory (Result Http.Error (String, List Post))


processPostListing : Decoder (List Post)
processPostListing = 
  Json.Decode.list (
    map4 Post (field "id" string) (field "category" string) (field "title" string) (field "subtext" string) 
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
    _ -> ({model | errMessage = Just "Something has gone horribly wrong."}, Cmd.none)

type alias Document msg = {
  title: String,
  body: List (Html msg)
  }

view : Model -> Document Msg
view model =
  {title = "Xnopyt.com", body = [htmlView model]}

htmlView : Model -> Html Msg
htmlView model =
  renderModel model

renderModel : Model -> Html Msg
renderModel model =
  case (model.errMessage) of
    Just e -> h1 [] [text (e)]
    Nothing ->
      case model.route of
        Just route -> case route of
          Home -> cardListing model
          PostView _ -> div [] [h1 [] [text (model.header)], p [] [text(model.content)], h3 [] [text(model.footer), a [href "/"] [text "Go Home"]]]
          _ -> h1 [] [ text ("Not found."), a [ href "/"] [text("Return home")] ]
        _ -> h1 [] [ text ("Not found."), a [ href "/"] [text("Return home")] ]

renderPost : Post -> Html Msg
renderPost post =
  div [
    style "width" "25%",
    style "height" "auto",
    style "box-sizing" "border-box",
    style "padding" "15px"
  ] [
    div [
      style "flex-direction" "column",
      style "border-radius" "5px",
      style "display" "flex",
      style "height" "100%"
      ] [
        header [
          style "background-color" "#404040",
          style "color" "white",
          style "height" "75px",
          style "display" "flex",
          style "flex-direction" "column",
          style "align-items" "center",
          style "box-shadow" "none",
          style "box-sizing" "border-box",
          style "font-size" "20px",
          style "font-weight" "bold",
          style "border-radius" "10px 10px 0 0"
        ] [
          a [href ("/post/" ++ post.id)] [text (post.title)]
        ],
        div [
          style "color" "white",
          style "flex-grow" "1",
          style "background-color" "#505050",
          style "padding" "10px"
        ] [
          text (post.subtext)
        ],
        footer [
          style "background-color" "#404040",
          style "color" "white",
          style "height" "50px",
          style "display" "flex",
          style "flex-direction" "row",
          style "justify-content" "space-around",
          style "box-shadow" "none",
          style "box-sizing" "border-box",
          style "padding" "10px",
          style "border-radius" "0 0 10px 10px"
        ] [
          text (post.category)
        ]
      ]
  ]

cardListing : Model -> Html Msg
cardListing model =
  div [
    style "display" "flex",
    style "flex-flow" "row wrap"
    ] (List.map renderPost model.posts)