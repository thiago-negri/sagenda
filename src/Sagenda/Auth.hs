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

authMiddleware :: SagendaContext -> Middleware
authMiddleware c wapp req send = do
    let auth = basicAuth req
    case auth of
        Nothing -> send forbidden
        Just (name, password) -> do
            let name' = pack name
                password' = pack password
            user <- authUser conn name' password'
            case user of
                Nothing -> send forbidden
                Just (_, False) -> send forbidden
                Just (v, True) -> 
                    let vault' = V.insert userK (v, name') (vault req)
                        req' = req { vault = vault' }
                     in wapp req' send
    where conn = connection c
          userK = userKey c

forbidden :: Response
forbidden = responseBuilder status403 [] "Forbidden"

ifMaybe :: Bool -> a -> Maybe a
ifMaybe False _ = Nothing
ifMaybe True a = Just a

basicAuth :: Request -> Maybe (String, String)
basicAuth req = do
    (_, header) <- find ((== "authorization") . fst) $ requestHeaders req
    let isBasic = "Basic " `BS.isPrefixOf` header
    authValue' <- ifMaybe isBasic (BS64.decode $ BS.drop 6 header)
    authValue <- BSU8.toString <$> rightToMaybe authValue'
    ix <- elemIndex ':' authValue
    let name = take ix authValue
        password = drop (ix + 1) authValue
    return (name, password)
