module Main where

import Happstack.Server
import Control.Monad
import Control.Exception
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Text.JSON

config = Conf {
  port = 80,
  validator = Nothing,
  logAccess = Just logMAccess,
  timeout = 30,
  threadGroup = Nothing
}

getPostListing c = do
  select <- prepare c "SELECT * FROM posts;"
  execute select []
  result <- fetchAllRows select
  let posts = JSArray $ (map (\(t:s:c:[]) -> let (title, subtext, category) = (fromSql t, fromSql s, fromSql c) in showJSON $ toJSObject [("title", toJSString title), ("subtext", toJSString subtext), ("category", toJSString category)]) result) in
    return $ Just $ encode posts

main :: IO ()
main = do
  c <- connectPostgreSQL "host=database user=blog password=root dbname=blog"
  simpleHTTP config $ 
    msum [ dir "v1" $ path $ \s -> case s of {"posts" -> require (getPostListing c) $ \posts -> ok posts ; _ -> notFound "Endpoint does not exist"}
         , notFound "Endpoint does not exist"
         ]
