module Main where

import Happstack.Server
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Text.JSON
import Data.ByteString.Char8 as C (pack)
import Data.ByteString.Lazy.Char8 as L (pack)
import Data.ByteString as B (unpack, ByteString)
import Text.Printf
import System.Random (newStdGen, randomRs)

import Crypto.Hash.SHA256 as SHA256

instance ToMessage JSValue where
  toContentType _ = C.pack "application/json"
  toMessage = \j -> L.pack $ encode j

config = Conf {
  port = 80,
  validator = Nothing,
  logAccess = Just logMAccess,
  timeout = 30,
  threadGroup = Nothing
}

getCategory :: IConnection t => t -> Int -> IO String
getCategory c category = do
  getCategory <- prepare c "SELECT name FROM categories WHERE catid = ?;"
  execute getCategory [toSql category]
  result <- fetchAllRows getCategory
  case result of
    (r:[]):[] -> return $ fromSql r;
    _ -> fail "More than one category with same ID!"

getPostListing :: IConnection t => t -> Int -> IO (Maybe JSValue)
getPostListing c page = do
  getPosts <- prepare c "SELECT * FROM posts ORDER BY postid DESC LIMIT 20 OFFSET (? * 20);"
  execute getPosts [toSql (page)]
  result <- fetchAllRows getPosts

  posts <- sequence $
    map (\(pid:cid:t:subtext:content:[]) -> do 
    category <- getCategory c $ fromSql cid
    let title = ("title", toJSString $ fromSql t)
    let sub = ("subtext", toJSString $ fromSql subtext)
    let cat = ("category", toJSString category)
    let id = ("id", toJSString $ show $ (fromSql pid::Int))
    let catid = ("catid", toJSString $ fromSql cid)
    return $ showJSON $ toJSObject [id, title, sub, cat, catid]
    ) result
  return $ Just $ showJSON $ JSArray posts

getCategoryListing :: IConnection t => t -> Int -> IO (Maybe JSValue)
getCategoryListing c page = do
  getCategories <- prepare c "SELECT * FROM categories ORDER BY catid DESC LIMIT 20 OFFSET (? * 20);"
  execute getCategories [toSql (page)]
  result <- fetchAllRows getCategories

  categories <- sequence $
    map (\(cid:n:[]) -> do
    let id = ("id", toJSString $ show $ (fromSql cid::Int))
    let name = ("name", toJSString $ fromSql n)
    return $ showJSON $ toJSObject [id, name]
    ) result
  return $ Just $ showJSON $ JSArray categories

getPostsInCategory :: IConnection t => t -> Int -> Int -> IO (Maybe JSValue)
getPostsInCategory c cat page = do
  getPosts <- prepare c "SELECT * FROM posts WHERE catid = ? ORDER BY postid DESC LIMIT 20 OFFSET (? * 20);"
  execute getPosts [toSql cat, toSql page]
  result <- fetchAllRows getPosts

  category <- getCategory c cat

  posts <- sequence $
    map (\(pid:cid:t:subtext:content:[]) -> do
    let title = ("title", toJSString $ fromSql t)
    let sub = ("subtext", toJSString $ fromSql subtext)
    let id = ("id", toJSString $ show $ (fromSql pid::Int))
    let cat = ("category", toJSString category)
    let catid = ("catid", toJSString $ fromSql cid)
    return $ showJSON $ toJSObject [id, cat, title, sub, catid]
    ) result
  
  return $ Just $ showJSON $ toJSObject [("category", showJSON $ toJSString category), ("posts", showJSON $ JSArray posts)]

getPostContent :: IConnection t => t -> Int -> IO (Maybe JSValue)
getPostContent c pid = do
  getPost <- prepare c "SELECT title, catid, content FROM posts WHERE postid = ?;"
  execute getPost [toSql (pid)]
  result <- fetchAllRows getPost

  case result of
    (t:cid:con:[]):[] -> do
      category <- getCategory c $ fromSql cid
      let title = ("title", toJSString $ fromSql t)
      let content = ("content", toJSString $ fromSql con)
      let cat = ("category", toJSString category)
      let catid = ("catid", toJSString $ fromSql cid)
      return $ Just $ showJSON $ toJSObject [title, content, cat, catid];
    _ -> fail "Post does not exist!"

hexOfBS :: B.ByteString -> String
hexOfBS = concatMap (printf "%02x") . B.unpack

random256 :: IO String
random256 = do
  g <- newStdGen
  return $ take 256 (randomRs ('a', 'z') g)

attemptLogin :: IConnection t => t -> String -> String -> IO (Maybe String)
attemptLogin c user pass = do
  getRows <- prepare c "SELECT * FROM users WHERE username = ?"
  execute getRows [toSql user]
  result <- fetchAllRows getRows

  case result of
    (userid:username:password:authtoken:[]):[] -> do
      let passwordHashBS = SHA256.hash (C.pack pass)
      let passwordHash = hexOfBS passwordHashBS
      if passwordHash == (fromSql password) then do
        at <- random256
        let atHashBS = SHA256.hash (C.pack at)
        let atHash = hexOfBS atHashBS
        updateAuthToken <- prepare c "UPDATE users SET authtoken = ? WHERE userid = ?"
        execute updateAuthToken [toSql atHash, toSql userid]
        commit c
        return $ Just at
      else
        return Nothing
    _ -> return Nothing

handleLogin :: IConnection t => t -> ServerPart Response
handleLogin c = do 
  methodM POST
  username <- look "username"
  password <- look "password"
  loginSuccess <- lift $ attemptLogin c username password
  case loginSuccess of
    Just authtoken -> do 
      addCookie Session (mkCookie "token" authtoken)
      ok $ toResponse $ showJSON $ toJSObject [("success", toJSString "true"), ("username", toJSString username)]
    Nothing -> ok $ toResponse $ showJSON $ toJSObject [("success", toJSString "false"), ("username", toJSString "")]

validateCookie :: IConnection t => t -> String -> String -> IO Bool
validateCookie c a u = do
  getAuthToken <- prepare c "SELECT authtoken from users WHERE username = ?;"
  execute getAuthToken [toSql u]
  result <- fetchAllRows getAuthToken
  case result of
    (at:[]):[] -> return $ (fromSql at) == (hexOfBS $ SHA256.hash $ C.pack a)
    _ -> return False

attemptCreatePost :: IConnection t => t -> String -> String -> String -> String -> String -> String -> IO Bool
attemptCreatePost c authtoken username title subtext content category = do
  cookieValid <- validateCookie c authtoken username
  case cookieValid of
    False -> return False
    True -> do
      -- First we should probably validate that the category actually exists
      getCategory <- prepare c "SELECT * FROM categories WHERE catid = ?;"
      execute getCategory [toSql category]
      result <- fetchAllRows getCategory
      case result of
        [] -> do return False
        _ -> do
          -- If we're here, we know the category does actually exist
          addPost <- prepare c "INSERT INTO posts(catid, title, subtext, content) VALUES(?, ?, ?, ?);"
          execute addPost [toSql category, toSql title, toSql subtext, toSql content]
          commit c
          return True

handleCreatePost :: IConnection t => t -> ServerPart Response
handleCreatePost c = do
  methodM POST
  authtoken <- lookCookieValue "token"
  username <- look "username"
  title <- look "title"
  subtext <- look "subtext"
  content <- look "content"
  category <- look "category"

  success <- lift $ attemptCreatePost c authtoken username title subtext content category
  case success of 
    True -> ok $ toResponse $ showJSON $ toJSObject [("success", toJSString "true")]
    False -> ok $ toResponse $ showJSON $ toJSObject [("success", toJSString "false")]

main :: IO ()
main = do
  c <- connectPostgreSQL "host=database user=blog password=root dbname=blog"

  createPostsTable <- prepare c "CREATE TABLE IF NOT EXISTS posts(postid serial primary key, catid int, title varchar(128), subtext varchar(128), content text);"
  createCategoriesTable <- prepare c "CREATE TABLE IF NOT EXISTS categories(catid serial primary key, name varchar(128));"
  createUsersTable <- prepare c "CREATE TABLE IF NOT EXISTS users(userid serial primary key, username varchar(128), password varchar(128), authtoken varchar(128));"
  execute createPostsTable []
  execute createCategoriesTable []
  execute createUsersTable []
  commit c
  
  simpleHTTP config $ do
    decodeBody (defaultBodyPolicy "/tmp/" 0 4096 4096)
    msum [ dir "v1" $ dir "post" $ path $ \pid -> require (getPostContent c pid) $ \post -> ok $ toResponse post
         , dir "v1" $ dir "category" $ path $ \cid -> require (getPostsInCategory c cid 0) $ \cat -> ok $ toResponse cat
         , dir "v1" $ dir "posts" $ require (getPostListing c 0) $ \posts -> ok $ toResponse posts
         , dir "v1" $ dir "categories" $ require (getCategoryListing c 0) $ \cats -> ok $ toResponse cats
         , dir "v1" $ dir "login" $ handleLogin c
         , dir "v1" $ dir "createPost" $ handleCreatePost c
         , notFound $ toResponse "Endpoint does not exist"
         ]
