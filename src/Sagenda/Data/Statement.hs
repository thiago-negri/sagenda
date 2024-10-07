{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Sagenda.Data.Statement where

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

authenticateUser :: Statement (Text, Text) (Maybe (Int32, Bool))
authenticateUser = [TH.maybeStatement|
    SELECT id :: int4,
           (password = $2 :: text) :: bool
        FROM users
        WHERE name = $1 :: text
    |]

allUsers :: Statement () (Vector (Int32, Text))
allUsers = [TH.vectorStatement|
    SELECT id :: int4, name :: text
        FROM users
    |]
