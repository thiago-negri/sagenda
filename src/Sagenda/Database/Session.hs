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
    rows <- run' session connection
    return $ toList rows
    where session = statement () selectAllUsersS

selectUserByName :: Text -> Connection -> ExceptT AppError IO (Maybe User)
selectUserByName name = run' session
    where session = statement name selectUserByNameS

run' :: Session a -> Connection -> ExceptT AppError IO a
run' s c = withExceptT DatabaseQueryError . ExceptT $ run s c
