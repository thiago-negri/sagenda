{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}

module Sagenda.Database.Statement (
    selectAllUsersS,
    selectUserByNameS
) where

import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, vectorStatement)

import Sagenda.Data.User (User (User))

selectAllUsersS :: Statement () (Vector User)
selectAllUsersS = dimap
    id
    (fmap userRow)
    [vectorStatement|
            SELECT id :: int4, name :: text, password :: text
                FROM users
        |]

selectUserByNameS :: Statement Text (Maybe User)
selectUserByNameS = dimap
    id
    (fmap userRow)
    [maybeStatement|
            SELECT id :: int4, name :: text, password :: text
                FROM users
                WHERE users.name = $1 :: text
        |]

userRow :: (Int32, Text, Text) -> User
userRow (uid, name, password) = User uid name password
