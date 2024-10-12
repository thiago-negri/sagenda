{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Database (connectDatabase) where

import qualified Data.ByteString.UTF8 as BSU
import Hasql.Connection (Settings, settings, Connection, acquire)

import Sagenda.Error (AppError (MissingEnvVar, DatabaseConnectionError))

connectDatabase :: IO (Either AppError Connection)
connectDatabase = do
    connectionSettings <- connectionSettingsFromEnv
    either (return . Left) acquire' connectionSettings

connectionSettingsFromEnv :: IO (Either AppError Settings)
connectionSettingsFromEnv = do
    host' <- lookEnv "DB_HOST"
    port' <- readEnv "DB_PORT"
    user' <- lookEnv "DB_USER"
    password' <- lookEnv "DB_PASSWORD"
    database' <- lookEnv "DB_DATABASE"
    return $ unpackSettingsFromEnv host' port' user' password' database'

lookEnv :: String -> IO (Either AppError ByteString)
lookEnv varName = do
    val' <- lookupEnv varName
    let val = BSU.fromString <$> val'
    maybe err (return . Right) val
    where err = return $ Left (MissingEnvVar varName)

readEnv :: Read a => String -> IO (Either AppError a)
readEnv varName = do
    val' <- lookupEnv varName
    let val = readMaybe =<< val'
    maybe err (return . Right) val
    where err = return $ Left (MissingEnvVar varName)

unpackSettingsFromEnv :: Either AppError ByteString ->
                         Either AppError Word16 ->
                         Either AppError ByteString ->
                         Either AppError ByteString ->
                         Either AppError ByteString ->
                         Either AppError Settings
unpackSettingsFromEnv host' port' user' password' database' =
    settings <$> host' <*> port' <*> user' <*> password' <*> database'

acquire' :: Settings -> IO (Either AppError Connection)
acquire' s = do
    c' <- acquire s
    return $ case c' of
        Left e -> Left $ DatabaseConnectionError e
        Right c -> Right c
