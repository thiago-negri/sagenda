{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Web (webServer) where

import qualified Web.Scotty as Scotty
import Network.HTTP.Types (status404)
import Hasql.Connection (Connection)

import Sagenda.Data.User ()
import Sagenda.Error ()
import Sagenda.Service (getAllUsers, getUserByName)
import Sagenda.Database (connectDatabase)
import Sagenda.Auth (authMiddleware)

getUsersH :: Connection -> Scotty.ActionM ()
getUsersH connection = do
    users <- liftIO $ getAllUsers connection
    Scotty.json users

getUserByNameH :: Connection -> Text -> Scotty.ActionM ()
getUserByNameH connection name = do
    user <- liftIO $ getUserByName connection name
    maybe notFound Scotty.json user
    where notFound = Scotty.status status404

routes :: Connection -> Scotty.ScottyM ()
routes connection = do
    Scotty.middleware $ authMiddleware connection
    Scotty.get "/users" $ getUsersH connection
    Scotty.get "/users/:name" $ do
        name <- Scotty.captureParam "name"
        getUserByNameH connection name

port :: Int
port = 3000

webServer :: IO ()
webServer = do
    connection' <- connectDatabase
    case connection' of
        Left e -> putStrLn $ "ERROR: Can't connect to database: " ++ show e
        Right connection -> Scotty.scotty port $ routes connection
