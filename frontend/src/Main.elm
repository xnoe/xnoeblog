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
  subtext: String,
  content: String
  }

type Route
  = Home
  | CategoriesView
  | CategoryPageView (String, String)
  | PostView String
  | PostPageView String
  | LoginView
  | LogoutView
  | CreatePostView
  | EditPostView

routeParser : P.Parser (Route -> a) a
routeParser =
  P.oneOf
    [ P.map (PostPageView "1") P.top
    , P.map PostPageView (P.s "post" </> P.s "page" </> P.string)
    , P.map CategoriesView (P.s "categories")
    , P.map (\a -> \b -> CategoryPageView (b,a)) (P.s "category" </> P.string </> P.s "page" </> P.string)
    , P.map (\a -> CategoryPageView ("1",a)) (P.s "category" </> P.string)
    , P.map PostView (P.s "post" </> P.string)
    , P.map LoginView (P.s "login")
    , P.map LogoutView (P.s "logout")
    , P.map CreatePostView (P.s "create")
    ]

processPostList : Decoder (List Post)
processPostList = 
  Json.Decode.list (
    map5(\a->\b->\c->\d->\e-> Post a (Category b c) d e "") (field "id" string) (field "catid" string) (field "category" string) (field "title" string) (field "subtext" string)
  )

processPostListing : Decoder (Maybe Int, List Post)
processPostListing =
  map2 (\a -> \b -> (String.toInt a,b)) (field "total" string) (field "posts" processPostList)

processCategoryList : Decoder (List Category)
processCategoryList =
  Json.Decode.list (map2 Category (field "id" string) (field "name" string))

processCategoryListing : Decoder (Maybe Int, List Category)
processCategoryListing =
  map2 (\a -> \b -> (String.toInt a, b)) (field "total" string) (field "categories" processCategoryList)

processPost : Decoder {title: String, content: String, subtext: String, category: Category}
processPost = 
  map5 (\a -> \b -> \c -> \d -> \e -> {title=a,content=b,subtext=e,category=(Category c d)}) (field "title" string) (field "content" string) (field "catid" string) (field "category" string) (field "subtext" string)

processCategory : Decoder (Maybe Int, String, List Post)
processCategory =
  map3 (\a -> \b -> \c -> (String.toInt a,b,c)) (field "total" string) (field "category" string) (field "posts" processPostList)

processLogout : Decoder (Bool)
processLogout = 
  Json.Decode.map (\a -> case a of 
    "true" -> True
    _ -> False) (field "success" string)

processDeletePost : Decoder (Bool)
processDeletePost = 
  Json.Decode.map (\a -> case a of 
    "true" -> True
    _ -> False) (field "success" string)

processLogin : Decoder (Maybe User)
processLogin =
  map2 (\a->\b->case a of 
    "true" -> Just (User b)
    _ -> Nothing) (field "success" string) (field "username" string)

processCreatePost : Decoder (Maybe String)
processCreatePost =
  map2 (\a->\b->case a of 
    "true" -> Just b
    _ -> Nothing) (field "success" string) (field "postid" string)

processEditPost : Decoder (Maybe String)
processEditPost =
  map2 (\a->\b->case a of 
    "true" -> Just b
    _ -> Nothing) (field "success" string) (field "postid" string)

processWhoami : Decoder (Maybe String)
processWhoami =
  map2 (\a->\b->case a of 
    "true" -> Just b
    _ -> Nothing) (field "success" string) (field "username" string)

messageOfRoute : Model -> Cmd Msg
messageOfRoute model =
  case model.route of
    Just route -> 
      case route of
        PostPageView _ -> Http.get {url = "/v1/posts?page=" ++ (String.fromInt model.page), expect = Http.expectJson GotPosts processPostListing}
        CategoriesView -> Http.get {url = "/v1/categories?page=" ++ (String.fromInt model.page), expect = Http.expectJson GotCategories processCategoryListing}
        PostView p -> Http.get {url = "/v1/post/" ++ p, expect = Http.expectJson GotPost processPost}
        CategoryPageView (_,c) -> Http.get {url = "/v1/category/" ++ c ++ "?page=" ++ (String.fromInt model.page), expect = Http.expectJson GotCategory processCategory}
        _ -> Cmd.none
    Nothing -> Cmd.none

type alias User = {
  username: String
  }
type alias Model = {
  header: String,
  body: String,
  footer: String,
  pinnedPosts: List Post,
  posts: List Post,
  route: Maybe Route,
  key: Browser.Navigation.Key,
  errMessage: Maybe (String),
  username: String,
  password: String,
  user: Maybe User,
  post: Post,
  page: Int,
  totalPages: Int
  }

type Msg
  = LinkClinked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotPosts (Result Http.Error ((Maybe Int, List Post)))
  | GotCategories (Result Http.Error ((Maybe Int, List Category)))
  | GotPost (Result Http.Error {title: String, content: String, subtext: String, category: Category})
  | GotCategory (Result Http.Error (Maybe Int, String, List Post))
  | LoginResult (Result Http.Error (Maybe User))
  | UsernameUpdate String
  | PasswordUpdate String
  | Login
  | Logout
  | LogoutResult (Result Http.Error Bool)
  | TitleUpdate String
  | ContentUpdate String
  | SubtextUpdate String
  | CategoryUpdate String
  | CreatePost
  | CreatePostResult (Result Http.Error (Maybe String))
  | DeletePostResult (Result Http.Error Bool)
  | EditPostResult (Result Http.Error (Maybe String))
  | GotWhoami (Result Http.Error (Maybe String))
  | GotoEditPost
  | GotoDeletePost
  | EditPost
  | DeletePost

init : () -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ url key =
  let r = P.parse routeParser url in
  let p = (case r of
        Just (PostPageView n) -> case (String.toInt n) of
          Just m -> (m-1)
          Nothing -> 0
        Just (CategoryPageView (n,_)) -> case (String.toInt n) of
          Just m -> (m-1)
          Nothing -> 0
        _ -> 0) in
  ({
      header = "", body = "", footer = "", pinnedPosts = [], 
      posts = [], route = r, key = key, errMessage = Nothing,
      username = "", password = "", user = Nothing,
      post = Post "" (Category "" "") "" "" "", page = p,
      totalPages = 1
    },
    Http.get {url = "/v1/whoami", expect = Http.expectJson GotWhoami processWhoami}
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
        let imodel = case r of
              Just CreatePostView -> {model | post = Post "" (Category "" "") "" "" ""}
              Just (PostView p) -> let post = model.post in {model | post = {post | id = p}}
              Just Home -> {model | page=0}
              Just CategoriesView -> {model | page=0}
              Just (PostPageView page) -> case String.toInt page of
                Just p -> {model | page = p-1}
                _ -> model
              Just (CategoryPageView (page,_)) -> case String.toInt page of
                Just p -> {model | page = p-1}
                _ -> model
              _ -> model
        in
        let m = {imodel | route = r} in (m, messageOfRoute m)
      )
    GotPosts (Ok (Just t, l)) -> ({model | errMessage = Nothing, posts = l, totalPages = ceiling ((toFloat t) / 20)}, Cmd.none)
    GotPosts (Err _) -> ({model | errMessage = Just "Failed to load posts"}, Cmd.none)
    GotPost (Ok {title, content, subtext, category}) -> let post = model.post in ({model | post = {post | title = title, content = content, category = category, subtext = subtext}}, Cmd.none)
    GotCategory (Ok (Just t, title, posts)) -> ({model | errMessage = Nothing, header = title, posts = posts, totalPages = ceiling ((toFloat t) / 20)}, Cmd.none)
    Login -> (model, handleLogin model)
    LoginResult (Ok (Just _ as u)) -> ({model | user = u}, load "/")
    LoginResult (Ok (Nothing)) -> ({model | errMessage = Just "Invalid username or password."}, Cmd.none)
    Logout -> (model, Http.get {url = "/v1/logout", expect = Http.expectJson LogoutResult processLogout})
    LogoutResult _ -> (model, load "/")
    UsernameUpdate u -> ({model | username = u}, Cmd.none)
    PasswordUpdate p -> ({model | password = p}, Cmd.none)
    TitleUpdate t -> let post = model.post in ({model | post = {post | title = t}}, Cmd.none)
    SubtextUpdate s -> let post = model.post in ({model | post = {post | subtext = s}}, Cmd.none)
    ContentUpdate c -> let post = model.post in ({model | post = {post | content = c}}, Cmd.none)
    CategoryUpdate c -> let post = model.post in let category = model.post.category in ({model | post = {post | category = {category | name = c}}}, Cmd.none)
    CreatePost -> (model, handleCreatePost model)
    CreatePostResult (Ok (Just postid)) -> (model, load ("/post/" ++ postid))
    CreatePostResult (Ok Nothing) -> ({model | errMessage = Just "Failed to create post"}, Cmd.none)
    DeletePostResult (Ok True) -> (model, load "/")
    DeletePostResult (Ok False) -> ({model | errMessage = Just "Failed to delete post"}, Cmd.none)
    GotWhoami (Ok (Just u)) -> let m = {model | user = Just (User u)} in (m, messageOfRoute m)
    GotWhoami _ -> (model, messageOfRoute model)
    GotoEditPost -> ({model | route = Just EditPostView}, Cmd.none)
    EditPost -> (model, handleEditPost model)
    EditPostResult (Ok (Just postid)) -> (model, load ("/post/" ++ postid))
    DeletePost -> (model, handleDeletePost model)
    _ -> ({model | errMessage = Just "Something has gone horribly wrong."}, Cmd.none)

handleLogin : Model -> Cmd Msg
handleLogin model =
  Http.post {
    url = "/v1/login", 
    body = multipartBody [stringPart "username" model.username, stringPart "password" model.password], 
    expect = Http.expectJson LoginResult processLogin
  }

handleCreatePost : Model -> Cmd Msg
handleCreatePost model =
  case model.user of 
    Nothing -> Cmd.none
    Just user ->
      Http.post {
        url = "/v1/createPost",
        body = multipartBody [
          stringPart "username" user.username,
          stringPart "title" model.post.title,
          stringPart "subtext" model.post.subtext,
          stringPart "content" model.post.content,
          stringPart "category" model.post.category.name
        ],
        expect = Http.expectJson CreatePostResult processCreatePost
      }

handleEditPost : Model -> Cmd Msg
handleEditPost model =
  case model.user of 
    Nothing -> Cmd.none
    Just user ->
      Http.post {
        url = "/v1/editPost",
        body = multipartBody [
          stringPart "username" user.username,
          stringPart "id" model.post.id,
          stringPart "title" model.post.title,
          stringPart "subtext" model.post.subtext,
          stringPart "content" model.post.content,
          stringPart "category" model.post.category.name
        ],
        expect = Http.expectJson EditPostResult processEditPost
      }

handleDeletePost : Model -> Cmd Msg
handleDeletePost model =
  case model.user of 
    Nothing -> Cmd.none
    Just user ->
      Http.post {
        url = "/v1/deletePost",
        body = multipartBody [
          stringPart "username" user.username,
          stringPart "id" model.post.id
        ],
        expect = Http.expectJson DeletePostResult processDeletePost
      }

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
    style "padding" "0",
    style "font-family" "sans-serif"
  ] [
    div [
      style "border-bottom" "2px solid black",
      style "height" "1.5em",
      style "padding" "5px"
    ] ([
      xa [href "/"] [text "Home"],
      text " - "
    ] ++
      case model.user of 
        Nothing -> [xa [href "/login"] [text "Login"]]
        Just u -> [xa [onClick Logout] [text "Logout"], text " - ", xa [href "/create"] [text "Create Post"]]
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
          PostPageView _ -> div [style "padding" "5px"] [h1 [] [text "Welcome to my blog."],cardListing model "/post"]
          PostView _ -> div [] ([h1 [] [text (model.post.title)], p [] [text(model.post.content)], h3 [] [text(model.post.category.name ++ " ")]] ++ case model.user of
            Nothing -> []
            Just u -> [xa [href "", onClick DeletePost] [text "Delete Post"], text " - ", xa [href "", onClick GotoEditPost] [text "Edit Post"]])
          CategoryPageView (_,c) -> div [style "padding" "5px"] [h1 [] [text (model.header)], cardListing model ("/category/" ++ c)]
          LoginView -> loginForm model
          CreatePostView -> createPostForm model CreatePost "Create Post"
          EditPostView -> createPostForm model EditPost "Edit Post"
          _ -> h1 [] [ text ("Not found."), xa [ href "/"] [text("Return home")] ]
        _ -> h1 [] [ text ("Not found."), xa [ href "/"] [text("Return home")] ]
cardListing : Model -> String -> Html Msg
cardListing model prefix =
  div [
    style "display" "flex",
    style "flex-direction" "column",
    style "align-items" "center"
  ] [
    div [
      style "width" "100%",
      style "display" "grid",
      style "grid-template-columns" "repeat(auto-fill, minmax(16rem, 1fr))"
    ] (List.map renderPost model.posts),
    viewPaginator model prefix
  ]

viewPaginator : Model -> String -> Html Msg
viewPaginator model prefix =
  p [] (List.indexedMap (\i -> \x -> if i /= model.page then 
    span [] [xa [href (prefix ++ "/page/" ++ (String.fromInt (i+1)))] [text (String.fromInt (i+1))], text (if i == (model.totalPages-1) then "" else ",")]
  else
    span [] [span [] [text (String.fromInt (i+1))], text (if i == (model.totalPages-1) then "" else ",")]
    ) (List.range 1 model.totalPages))

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
          xa [style "text-align" "center", href ("/post/" ++ post.id)] [text (post.title)]
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
          text ("Category: "), xa [href ("/category/" ++ post.category.id)] [text (post.category.name)]
        ]
      ]
  ]

createPostForm : Model -> Msg -> String -> Html Msg
createPostForm model msg buttonText =
  div [
    style "display" "flex",
    style "flex-direction" "column"
  ] [
    viewInput "text" "title" "Title" model.post.title TitleUpdate,
    viewInput "text" "subtext" "Subtext" model.post.subtext SubtextUpdate,
    viewTextarea "content" "Content" model.post.content ContentUpdate,
    viewInput "text" "category" "Blog Post" model.post.category.name CategoryUpdate,
    button [onClick msg] [text buttonText]
  ]

loginForm : Model -> Html Msg
loginForm model =
  div [
    style "display" "flex",
    style "flex-direction" "column"
  ] [
    viewInput "text" "username" "Username" model.username UsernameUpdate,
    viewInput "password" "password" "Password" model.password PasswordUpdate,
    button [onClick Login] [text "Login"]
  ]

viewInput : String -> String -> String -> String -> (String -> msg) -> Html msg
viewInput t n p v toMsg =
  input [ name n, type_ t, placeholder p, value v, onInput toMsg ] []

viewTextarea : String -> String -> String -> (String -> msg) -> Html msg
viewTextarea n p v toMsg =
  textarea [ name n, placeholder p, value v, onInput toMsg ] []

xa : List (Attribute msg) -> List (Html msg) -> Html msg
xa x y =
  a (x ++ [
    style "text-decoration" "none",
    style "color" "#aa66ff"
  ]) y