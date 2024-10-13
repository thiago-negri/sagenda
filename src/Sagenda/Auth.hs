{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Auth (authMiddleware) where

import Network.Wai (
        Request (requestHeaders, vault),
        Middleware,
        responseBuilder,
        Response
    )
import Network.HTTP.Types (status403, status500)
import Network.HTTP.Types.Header (Header)
import Data.Text (pack, encodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.UTF8 as BSU8
import qualified Data.Vault.Lazy as V
import Control.Monad.Trans.Maybe (hoistMaybe)
import Control.Monad.Trans.Except (throwE)
import Crypto.BCrypt (validatePassword)

import Sagenda.Context (SagendaContext (connection, userKey))
import Sagenda.Debug (debugLog)
import Sagenda.Error (AppError (AuthError))
import Sagenda.Database.Session (selectUserByName)
import Sagenda.Data.User (userPassword)

authMiddleware :: SagendaContext -> Middleware
authMiddleware c wapp req send = do
    req' <- runExceptT $ do
        (name, password) <- maybeToExceptT (AuthError "Invalid authorization") $
                            hoistMaybe . basicAuth $ requestHeaders req
        let name' = pack name
            password' = BSC8.pack password
        user <- unpackMaybe (AuthError "Invalid credentials") $
                selectUserByName conn name'
        let actualPassword = encodeUtf8 $ userPassword user
            correctPassword = validatePassword actualPassword password'
            vault' = V.insert userK user $ vault req
        maybeToExceptT (AuthError "Invalid credentials") $
            hoistMaybe . ifMaybe correctPassword $ req { vault = vault' }
    either handleAppError (`wapp` send) req'
    where userK = userKey c
          conn = connection c
          handleAppError (AuthError e) = do
            debugLog $ show (AuthError e)
            send forbidden
          handleAppError e = do
            debugLog $ show e
            send internalServerError

basicAuth :: [Header] -> Maybe (String, String)
basicAuth headers = do
    (_, header) <- find ((== "authorization") . fst) headers
    let isBasic = "Basic " `BS.isPrefixOf` header
    authValue' <- ifMaybe isBasic $ BS64.decode $ BS.drop 6 header
    authValue <- BSU8.toString <$> rightToMaybe authValue'
    ix <- elemIndex ':' authValue
    let name = take ix authValue
        password = drop (ix + 1) authValue
    return (name, password)

ifMaybe :: Bool -> a -> Maybe a
ifMaybe False _ = Nothing
ifMaybe True a = Just a

unpackMaybe :: Monad m => e -> ExceptT e m (Maybe a) -> ExceptT e m a
unpackMaybe e f = f >>= maybe (throwE e) return

forbidden :: Response
forbidden = responseBuilder status403 [] "Forbidden"

internalServerError :: Response
internalServerError = responseBuilder status500 [] "Internal Server Error"
