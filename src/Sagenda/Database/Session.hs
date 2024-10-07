module Sagenda.Database.Session (
    allUsers,
    authenticateUser
) where

import Hasql.Session (Session, statement)

import Sagenda.Data.User (User)
import Sagenda.Database.Statement (allUsersStatement, authenticateUserStatement)

allUsers :: Session (Vector User)
allUsers = statement () allUsersStatement

authenticateUser :: Text -> Text -> Session (Maybe (Int32, Bool))
authenticateUser name password =
    statement (name, password) authenticateUserStatement
