{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}

module Sagenda.Database.Statement (
    selectUserIdByNameAndPasswordS,
    selectAllUsersS,
    selectUserByNameS
) where

import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, vectorStatement)

import Sagenda.Data.User (User (PublicUser))

selectUserIdByNameAndPasswordS :: Statement (Text, Text) (Maybe Int32)
selectUserIdByNameAndPasswordS = [maybeStatement|
        SELECT id :: int4
            FROM users
            WHERE name = $1 :: text
              AND password = $2 :: text
    |]

selectAllUsersS :: Statement () (Vector User)
selectAllUsersS = dimap
    id
    (fmap $ uncurry PublicUser)
    [vectorStatement|
            SELECT id :: int4, name :: text
                FROM users
        |]

selectUserByNameS :: Statement Text (Maybe User)
selectUserByNameS = dimap
    id
    (fmap $ uncurry PublicUser)
    [maybeStatement|
            SELECT id :: int4, name :: text
                FROM users
                WHERE users.name = $1 :: text
        |]