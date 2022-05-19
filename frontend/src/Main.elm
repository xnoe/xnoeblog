module Main exposing (..)

import Http
import Json.Decode exposing (Decoder, field, string, map3)

import Browser
import Browser.Navigation exposing (..)
import Url
import Html exposing (..)
import Html.Attributes exposing (..)

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

type alias GridItem = {
  category: String,
  title: String,
  subtext: String
  }

type alias Model = {
  header: Maybe (Html Msg),
  sidebar: Maybe (Html Msg),
  pinnedPosts: List GridItem,
  posts: List GridItem,
  url: Url.Url,
  key: Browser.Navigation.Key,
  errMessage: Maybe (String)
  }

init : () -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ url key =
  ({
      header = Nothing, sidebar = Nothing, pinnedPosts = [], posts = [],
      url = url, key = key, errMessage = Nothing
    },
    Http.get {url = "v1/posts", expect = Http.expectJson GotPosts processPosts}
  )

type Msg
  = LinkClinked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotPosts (Result Http.Error (List GridItem))


processPosts : Decoder (List GridItem)
processPosts = 
  Json.Decode.list (
    map3 GridItem (field "category" string) (field "title" string) (field "subtext" string)
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
        { model | url = req }
        , Cmd.none
      )
    GotPosts (Ok l) -> ({model | posts = l}, Cmd.none)
    GotPosts (Err _) -> ({model | errMessage = Just "Failed to load posts!"}, Cmd.none)

type alias Document msg = {
  title: String,
  body: List (Html msg)
  }

renderGridItem : GridItem -> Html Msg
renderGridItem griditem =
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
          text (griditem.title)
        ],
        div [
          style "color" "white",
          style "flex-grow" "1",
          style "background-color" "#505050",
          style "padding" "10px"
        ] [
          text (griditem.subtext)
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
          text (griditem.category)
        ]
      ]
  ]

renderModel : Model -> Html Msg
renderModel model =
  div [
    style "display" "flex",
    style "flex-flow" "row wrap"
    ] 
    (case (model.errMessage) of
      Nothing -> (List.map renderGridItem model.posts)
      Just e -> [h1 [] [text (e)]]
    )


view : Model -> Document Msg
view model =
  {title = "Xnopyt.com", body = [htmlView model]}

htmlView : Model -> Html Msg
htmlView model =
  renderModel model