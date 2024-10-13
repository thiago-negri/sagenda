{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}

module Sagenda.Database.Statement (
    authenticateUserStatement,
    allUsersStatement,
    findUserByNameStatement
) where

import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, vectorStatement)

import Sagenda.Data.User (User (PublicUser))

authenticateUserStatement :: Statement (Text, Text) (Maybe Int32)
authenticateUserStatement = [maybeStatement|
        SELECT id :: int4
            FROM users
            WHERE name = $1 :: text
              AND password = $2 :: text
    |]

allUsersStatement :: Statement () (Vector User)
allUsersStatement = dimap
    id
    (fmap $ uncurry PublicUser)
    [vectorStatement|
            SELECT id :: int4, name :: text
                FROM users
        |]

findUserByNameStatement :: Statement Text (Maybe User)
findUserByNameStatement = dimap
    id
    (fmap $ uncurry PublicUser)
    [maybeStatement|
            SELECT id :: int4, name :: text
                FROM users
                WHERE users.name = $1 :: text
        |]