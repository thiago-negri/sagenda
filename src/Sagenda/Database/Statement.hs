{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}

module Sagenda.Database.Statement (
    authenticateUserStatement,
    allUsersStatement
) where

import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, vectorStatement)

import Sagenda.Data.User (User (PublicUser))

authenticateUserStatement :: Statement (Text, Text) (Maybe (Int32, Bool))
authenticateUserStatement = [maybeStatement|
        SELECT id :: int4,
            (password = $2 :: text) :: bool
            FROM users
            WHERE name = $1 :: text
    |]

allUsersStatement :: Statement () (Vector User)
allUsersStatement = dimap
    id
    (fmap $ uncurry PublicUser)
    [vectorStatement|
            SELECT id :: int4, name :: text
                FROM users
        |]
