module Sagenda.Database (
    newDatabaseConnectionPool,
    DatabasePool,
    withDatabase
) where

import qualified Data.ByteString.UTF8 as BSU

import Data.Pool (
        Pool, 
        defaultPoolConfig, 
        newPool, 
        putResource, 
        takeResource, 
        destroyResource
    )
import qualified Hasql.Connection as HC

import Sagenda.Error (
        AppError (MissingEnvVar, DatabaseConnectionError, InvalidEnvVar)
    )
import Control.Error (note, failWithM)

type DatabasePool = Pool (Either AppError HC.Connection)

newDatabaseConnectionPool :: ExceptT AppError IO DatabasePool
newDatabaseConnectionPool = do
    connectionSettings <- connectionSettingsEnv
    lift . newPool $ defaultPoolConfig
                            (acquire' connectionSettings)
                            release'
                            idleTimeInSeconds
                            poolMaxResources
    where idleTimeInSeconds = fifteenMinutes
          fifteenMinutes = 15 * 60
          -- poolMaxResources must not be smaller than numStripes
          poolMaxResources = max numCapabilities 40

withDatabase :: DatabasePool -> 
                (HC.Connection -> ExceptT AppError IO a) -> 
                ExceptT AppError IO a
withDatabase pool f = ExceptT $ do
    (connect, lpool) <- takeResource pool
    case connect of
        Left e -> do
            destroyResource pool lpool connect
            return (Left e)
        Right conn -> do
            v <- runExceptT $ f conn
            putResource lpool connect
            return v

connectionSettingsEnv :: ExceptT AppError IO HC.Settings
connectionSettingsEnv = do
    host <- getEnvBS "DB_HOST"
    port <- readEnv "DB_PORT"
    user <- getEnvBS "DB_USER"
    password <- getEnvBS "DB_PASSWORD"
    database <- getEnvBS "DB_DATABASE"
    return $ HC.settings host port user password database

getEnvBS :: String -> ExceptT AppError IO ByteString
getEnvBS = lookEnv (return . BSU.fromString)

readEnv :: Read a => String -> ExceptT AppError IO a
readEnv e = lookEnv (except . note (InvalidEnvVar e) . readMaybe) e

lookEnv :: (String -> ExceptT AppError IO a) -> String -> ExceptT AppError IO a
lookEnv f e = do
    v <- failWithM (MissingEnvVar e) $ lookupEnv e
    f v

acquire' :: HC.Settings -> IO (Either AppError HC.Connection)
acquire' s = mapLeft DatabaseConnectionError <$> HC.acquire s

release' :: Either AppError HC.Connection -> IO ()
release' = either mempty HC.release
