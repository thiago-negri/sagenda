{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Web (webServer) where

import qualified Web.Scotty as Scotty
import Network.HTTP.Types (status404)

import Sagenda.Data.User ()
import Sagenda.Error ()
import Sagenda.Service (getAllUsers, getUserByName)
import Sagenda.Database (connectDatabase)
import Sagenda.Auth (authMiddleware)
import qualified Data.Vault.Lazy as V
import Sagenda.Context (SagendaContext (connection, SagendaContext, userKey))
import Network.Wai (Request(vault))
import Data.Text.Lazy (pack)
import Data.Text (unpack)
import Sagenda.Debug (debugLog)

getUsersH :: SagendaContext -> Scotty.ActionM ()
getUsersH c = do
    users <- liftIO $ getAllUsers (connection c)
    Scotty.json users

getUserByNameH :: SagendaContext -> Text -> Scotty.ActionM ()
getUserByNameH c name = do
    user <- liftIO $ getUserByName (connection c) name
    maybe notFound Scotty.json user
    where notFound = Scotty.status status404

routes :: SagendaContext -> Scotty.ScottyM ()
routes c = do
    Scotty.middleware $ authMiddleware c
    Scotty.get "/users" $ getUsersH c
    Scotty.get "/users/:name" $ do
        name <- Scotty.captureParam "name"
        getUserByNameH c name
    Scotty.get "/me" $ do
        req <- Scotty.request
        case V.lookup (userKey c) (vault req) of
            Nothing -> Scotty.text "NOTHING"
            Just (uid, name) -> Scotty.text . pack $ "Id: " ++ show uid ++ " Name:" ++ unpack name

port :: Int
port = 3000

webServer :: IO ()
webServer = do
    debugLog "+---------+"
    debugLog "| SAGENDA |"
    debugLog "+---------+"

    debugLog "Creating vault keys"
    uKey <- V.newKey :: IO (V.Key (Int32, Text))

    debugLog "Connecting to database"
    connection' <- connectDatabase
    case connection' of
        Left e -> debugLog $ "ERROR: Can't connect to database: " ++ show e
        Right conn -> do
            debugLog "Starting web server"
            Scotty.scotty port $ routes (SagendaContext uKey conn)
            debugLog "Done"
