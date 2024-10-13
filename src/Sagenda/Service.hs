module Sagenda.Service (getAllUsers, getUserByName, authUser) where

import Hasql.Connection (Connection)
import Hasql.Session (run)

import Sagenda.Database.Session (allUsers, findUserByName, authenticateUser)
import Sagenda.Data.User (User (PublicUser))
import Control.Monad.Trans.Maybe (hoistMaybe)

getAllUsers :: Connection -> IO [User]
getAllUsers connection = do
    -- FIXME(tnegri): Handle error
    Right rows <- run allUsers connection
    return $ toList rows

getUserByName :: Connection -> Text -> MaybeT IO User
getUserByName connection name = do
    -- FIXME(tnegri): Handle error
    Right row <- lift $ run (findUserByName name) connection
    hoistMaybe row

authUser :: Connection -> Text -> Text -> MaybeT IO User
authUser connection name password = do
    -- FIXME(tnegri): Handle error
    Right row <- lift $ run (authenticateUser name password) connection
    uid <- hoistMaybe row
    return $ PublicUser uid name
