{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Auth (authMiddleware) where

import Network.Wai (
        Request (requestHeaders, vault),
        Middleware,
        responseBuilder,
        Response
    )
import Network.HTTP.Types (status403)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.UTF8 as BSU8
import Sagenda.Service (authUser)
import Data.Text (pack)
import qualified Data.Vault.Lazy as V
import Sagenda.Context (SagendaContext (connection, userKey))
import Network.HTTP.Types.Header (Header)
import Sagenda.Debug (debugLog)
import Control.Monad.Trans.Maybe (hoistMaybe)

authMiddleware :: SagendaContext -> Middleware
authMiddleware c wapp req send = do
    req' <- runExceptT $ do
        (name, password) <- maybeToExceptT "Invalid authorization header" $
                            hoistMaybe . basicAuth $ requestHeaders req
        let name' = pack name
            password' = pack password
            conn = connection c
        user <- maybeToExceptT "Invalid credentials" $
                authUser conn name' password'
        let userK = userKey c
            vault' = V.insert userK user (vault req)
        return $ req { vault = vault' }
    either (\e -> debugLog e >> send forbidden)
           (`wapp` send)
           req'

forbidden :: Response
forbidden = responseBuilder status403 [] "Forbidden"

ifMaybe :: Bool -> a -> Maybe a
ifMaybe False _ = Nothing
ifMaybe True a = Just a

basicAuth :: [Header] -> Maybe (String, String)
basicAuth headers = do
    (_, header) <- find ((== "authorization") . fst) headers
    let isBasic = "Basic " `BS.isPrefixOf` header
    authValue' <- ifMaybe isBasic (BS64.decode $ BS.drop 6 header)
    authValue <- BSU8.toString <$> rightToMaybe authValue'
    ix <- elemIndex ':' authValue
    let name = take ix authValue
        password = drop (ix + 1) authValue
    return (name, password)
