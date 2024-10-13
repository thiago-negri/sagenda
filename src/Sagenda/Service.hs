module Sagenda.Service (getAllUsers, getUserByName, authUser) where

import Hasql.Connection (Connection)
import Hasql.Session (run, Session)

import Sagenda.Database.Session (allUsers, findUserByName, authenticateUser)
import Sagenda.Data.User (User (PublicUser))
import Sagenda.Error (AppError (DatabaseQueryError))

getAllUsers :: Connection -> ExceptT AppError IO [User]
getAllUsers connection = do
    rows <- run' connection allUsers
    return $ toList rows

getUserByName :: Connection -> Text -> ExceptT AppError IO (Maybe User)
getUserByName connection name = run' connection $ findUserByName name

authUser :: Connection -> Text -> Text -> ExceptT AppError IO (Maybe User)
authUser connection name password = do
    uid <- run' connection $ authenticateUser name password
    return $ uid >>= \uuid' -> Just (PublicUser uuid' name)

run' :: Connection -> Session a -> ExceptT AppError IO a
run' s c = withExceptT DatabaseQueryError . ExceptT $ run c s
