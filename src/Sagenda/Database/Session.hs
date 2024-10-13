module Sagenda.Database.Session (
    selectAllUsers,
    selectUserByName
) where

import Hasql.Session (Session, statement, run)
import Hasql.Connection (Connection)

import Sagenda.Data.User (User)
import Sagenda.Database.Statement (
        selectAllUsersS,
        selectUserByNameS
    )
import Sagenda.Error (AppError (DatabaseQueryError))

selectAllUsers :: Connection -> ExceptT AppError IO [User]
selectAllUsers connection = do
    rows <- run' connection session
    return $ toList rows
    where session = statement () selectAllUsersS

selectUserByName :: Connection -> Text -> ExceptT AppError IO (Maybe User)
selectUserByName connection name = run' connection session
    where session = statement name selectUserByNameS

run' :: Connection -> Session a -> ExceptT AppError IO a
run' c s = withExceptT DatabaseQueryError . ExceptT $ run s c
