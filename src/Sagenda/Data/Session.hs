module Sagenda.Data.Session (allUsers, authenticateUser) where

import Hasql.Session (Session, statement)

import qualified Sagenda.Data.Statement as Statement

allUsers :: Session (Vector (Int32, Text))
allUsers = statement () Statement.allUsers

authenticateUser :: Text -> Text -> Session (Maybe (Int32, Bool))
authenticateUser name password =
    statement (name, password) Statement.authenticateUser