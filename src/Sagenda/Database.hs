{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Database (connectDatabase) where

import qualified Hasql.Connection as Connection

connectionSettings :: Connection.Settings
connectionSettings = Connection.settings host port user password database
    where host = "localhost"
          port = 5432
          user = "root"
          password = "!Letm3in!"
          database = "agenda"

connectDatabase :: IO Connection.Connection
connectDatabase = do
    -- FIXME(tnegri): Handle error
    Right connection <- Connection.acquire connectionSettings
    return connection
