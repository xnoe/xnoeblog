module Main where

import Happstack.Server
import Control.Monad
import Control.Exception
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Text.JSON
import Data.ByteString.Char8 as C (pack)
import Data.ByteString.Lazy.Char8 as L (pack)

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
    return $ showJSON $ toJSObject [title, sub, cat]
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
    return $ showJSON $ toJSObject [title, sub]
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
      return $ Just $ showJSON $ toJSObject [title, content, cat];
    _ -> fail "Post does not exist!"

main :: IO ()
main = do
  c <- connectPostgreSQL "host=database user=blog password=root dbname=blog"

  createPostsTable <- prepare c "CREATE TABLE IF NOT EXISTS posts(postid serial primary key, catid int, title varchar(128), subtext varchar(128), content text);"
  createCategoriesTable <- prepare c "CREATE TABLE IF NOT EXISTS categories(catid serial primary key, name varchar(128));"
  execute createPostsTable []
  execute createCategoriesTable []
  commit c
  
  simpleHTTP config $ 
    msum [ dir "v1" $ dir "post" $ path $ \pid -> require (getPostContent c pid) $ \post -> ok $ toResponse post
         , dir "v1" $ dir "category" $ path $ \cid -> require (getPostsInCategory c cid 0) $ \cat -> ok $ toResponse cat
         , dir "v1" $ dir "posts" $ require (getPostListing c 0) $ \posts -> ok $ toResponse posts
         , dir "v1" $ dir "categories" $ require (getCategoryListing c 0) $ \cats -> ok $ toResponse cats
         , notFound $ toResponse "Endpoint does not exist"
         ]
