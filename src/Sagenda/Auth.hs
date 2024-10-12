{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Auth (authMiddleware) where

import Network.Wai (
        Request (requestHeaders),
        Middleware,
        responseBuilder,
        Response
    )
import Network.HTTP.Types (status403)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.UTF8 as BSU8
import Hasql.Connection (Connection)
import Sagenda.Service (authUser)
import Data.Text (pack)

authMiddleware :: Connection -> Middleware
authMiddleware conn wapp req send = do
    let auth = basicAuth req
    case auth of
        Nothing -> send forbidden
        Just (name, password) -> do
            user <- authUser conn (pack name) (pack password)
            case user of
                Nothing -> send forbidden
                Just (_, False) -> send forbidden
                Just (_, True) -> wapp req send

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
    let (name, password) = splitAt ix authValue
    return (name, password)
