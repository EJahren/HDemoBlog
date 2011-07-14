{-# LANGUAGE FlexibleContexts #-}
module Main where

import Pages
import Constants

import Happstack.Server
import Happstack.Server.Auth
import Control.Monad
import Data.Monoid
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad.IO.Class
import qualified Text.Blaze as B


main = do
  con <- connectSqlite3 "blog.db" 
  g <- prepare con "select ROWID,title,post from posts;" 
  ins <- prepare con "INSERT INTO posts values (?,?);"
  rm  <- prepare con "delete from posts where ROWID=?;"
  let insertP a b = execute ins [toSql a, toSql b]
  let remv i = execute rm [toSql i]
  let get = getPosts g 
  simpleHTTP nullConf $ decodeBody myPolicy >> msum 
       [dir "style.css" $ serveFile (asContentType "text/css") "style.css",
        dir "new" $ myAuth inputHandler,
        dir "remove" . myAuth $handleRemove remv,
        dir "send" . myAuth $ newPostHandler insertP,
        mainHandler get
        ]

getPosts :: Statement -> IO [(Int,B.Html,B.Html)]
getPosts query = do
  execute query []
  rows <- fetchAllRows query
  return (map (\[a,b,c] -> (fromSql a ,convert b,convert c)) rows) 
    where convert s = B.toHtml (fromSql s :: String)

myAuth :: ServerPart a -> ServerPart a  
myAuth = basicAuth "127.0.0.1" accounts 

newPostHandler :: (String -> String -> IO a) -> ServerPart Response
newPostHandler f = do
  methodM POST
  title <- look "title"
  post  <- look "post"
  liftIO (f title post)
  seeOther domain . toResponse $ "The post is added!"

inputHandler :: ServerPart Response
inputHandler = ok . toResponse $ inputPage

mainHandler :: IO [(Int,B.Html,B.Html)] -> ServerPart Response
mainHandler f = do
  posts <- liftIO f
  ok . toResponse $ mainPage posts

lookId :: RqData Int
lookId = lookRead "id"

handleRemove remv = do
  r <- getDataFn lookId
  case r of
    (Left e) ->
        badRequest . toResponse . unlines $ e
    (Right i) -> do
        liftIO (remv i)
        seeOther domain . toResponse $ "The post is deleted!"

initDB = do
  con <- connectSqlite3 "blog.db"
  runRaw con "CREATE TABLE posts (title TEXT,post TEXT);"
  commit con
