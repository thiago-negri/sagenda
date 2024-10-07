{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Web (webServer) where

import qualified Web.Scotty as Scotty
--import Network.HTTP.Types (status404)
import Hasql.Connection (Connection)

import Sagenda.Service (getAllUsers)
import Sagenda.Database (connectDatabase)


getUsers :: Connection -> Scotty.ActionM ()
getUsers connection = do
    users <- liftIO $ getAllUsers connection
    Scotty.json users

--
--getUserByName :: String -> ActionM ()
--getUserByName = maybe notFound json . userByName
--    where notFound = status status404

routes :: Connection -> Scotty.ScottyM ()
routes connection = do
    Scotty.get "/users" $ getUsers connection
--    get "/users/:name" $ do
--        name <- captureParam "name"
--        getUserByName name

port :: Int
port = 3000

webServer :: IO ()
webServer = do
    connection <- connectDatabase
    Scotty.scotty port $ routes connection
