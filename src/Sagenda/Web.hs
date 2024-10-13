{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Web (webServer) where

import qualified Web.Scotty as Scotty
import Network.HTTP.Types (status404, status500)

import Sagenda.Data.User (userId, userName, userToPublic)
import Sagenda.Error (AppError)
import Sagenda.Database (withDatabase)
import Sagenda.Auth (authMiddleware)
import qualified Data.Vault.Lazy as V
import Sagenda.Context (SagendaContext (database, userKey), newContext)
import Network.Wai (Request(vault))
import Data.Text.Lazy (pack)
import Data.Text (unpack)
import Sagenda.Debug (debugLog)
import Sagenda.Database.Session (selectAllUsers, selectUserByName)

logErrorAnd500 :: AppError -> Scotty.ActionM ()
logErrorAnd500 e = do
    liftIO . debugLog $ show e
    Scotty.status status500
    Scotty.text "Internal Server Error"

notFound :: Scotty.ActionM ()
notFound = do
    Scotty.status status404
    Scotty.text "Not Found"

getUsersH :: SagendaContext -> Scotty.ActionM ()
getUsersH c = do
    users <- liftIO . runExceptT . withDatabase pool $ selectAllUsers
    either logErrorAnd500 (Scotty.json . map userToPublic) users
    where pool = database c

getUserByNameH :: SagendaContext -> Text -> Scotty.ActionM ()
getUserByNameH c name = do
    user <- liftIO . runExceptT . withDatabase pool $ selectUserByName name
    either logErrorAnd500 (maybe notFound (Scotty.json . userToPublic)) user
    where pool = database c

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
            Just user -> Scotty.text . pack $ "Id: " ++ show (userId user) ++ " Name:" ++ unpack (userName user)

port :: Int
port = 3000

webServer :: IO ()
webServer = do
    debugLog "+---------+"
    debugLog "| SAGENDA |"
    debugLog "+---------+"

    contextE <- runExceptT newContext
    case contextE of
        Left e -> do
            debugLog $ "Erro creating context: " ++ show e
        Right context -> do
            debugLog "Starting web server"
            Scotty.scotty port $ routes context
            debugLog "Done"
