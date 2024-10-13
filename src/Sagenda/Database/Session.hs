module Sagenda.Database.Session (
    selectAllUsers,
    selectUserByNameAndPassword,
    selectUserByName
) where

import Hasql.Session (Session, statement, run)
import Hasql.Connection (Connection)

import Sagenda.Data.User (User (PublicUser))
import Sagenda.Database.Statement (
        selectAllUsersS,
        selectUserIdByNameAndPasswordS,
        selectUserByNameS
    )
import Sagenda.Error (AppError (DatabaseQueryError))

selectAllUsers :: Connection -> ExceptT AppError IO [User]
selectAllUsers connection = do
    rows <- run' connection stmt
    return $ toList rows
    where stmt = statement () selectAllUsersS

selectUserByNameAndPassword :: Connection -> Text -> Text -> ExceptT AppError IO (Maybe User)
selectUserByNameAndPassword connection name password = do
    uid <- run' connection stmt
    return $ uid >>= \uuid' -> Just (PublicUser uuid' name)
    where stmt = statement (name, password) selectUserIdByNameAndPasswordS

selectUserByName :: Connection -> Text -> ExceptT AppError IO (Maybe User)
selectUserByName connection name = run' connection stmt
    where stmt = statement name selectUserByNameS

run' :: Connection -> Session a -> ExceptT AppError IO a
run' s c = withExceptT DatabaseQueryError . ExceptT $ run c s
