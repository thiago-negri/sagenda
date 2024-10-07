{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Database (connectDatabase) where

import Hasql.Connection (Settings, settings, Connection, acquire)

connectionSettings :: Settings
connectionSettings = settings host port user password database
    where host = "localhost"
          port = 5432
          user = "root"
          password = "!Letm3in!"
          database = "agenda"

connectDatabase :: IO Connection
connectDatabase = do
    -- FIXME(tnegri): Handle error
    Right connection <- acquire connectionSettings
    return connection
