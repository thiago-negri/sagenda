module Sagenda.Database (connectDatabase) where

import qualified Data.ByteString.UTF8 as BSU
import Hasql.Connection (Settings, settings, Connection, acquire)

import Sagenda.Error (AppError (MissingEnvVar, DatabaseConnectionError))

connectDatabase :: IO (Either AppError Connection)
connectDatabase = do
    connectionSettings <- connectionSettingsEnv
    either (return . Left) acquire' connectionSettings

connectionSettingsEnv :: IO (Either AppError Settings)
connectionSettingsEnv = do
    host' <- getEnvBS "DB_HOST"
    port' <- readEnv "DB_PORT"
    user' <- getEnvBS "DB_USER"
    password' <- getEnvBS "DB_PASSWORD"
    database' <- getEnvBS "DB_DATABASE"
    return $ unpackSettings host' port' user' password' database'

getEnvBS :: String -> IO (Either AppError ByteString)
getEnvBS = lookEnv (BSU.fromString <$>)

readEnv :: Read a => String -> IO (Either AppError a)
readEnv = lookEnv (readMaybe =<<)

lookEnv :: (Maybe String -> Maybe a) -> String -> IO (Either AppError a)
lookEnv f e = lookupEnv e >>= maybe err (return . Right) . f
    where err = return $ Left (MissingEnvVar e)

unpackSettings :: Either AppError ByteString ->
                  Either AppError Word16 ->
                  Either AppError ByteString ->
                  Either AppError ByteString ->
                  Either AppError ByteString ->
                  Either AppError Settings
unpackSettings host' port' user' password' database' =
    settings <$> host' <*> port' <*> user' <*> password' <*> database'

acquire' :: Settings -> IO (Either AppError Connection)
acquire' s = mapLeft DatabaseConnectionError <$> acquire s
