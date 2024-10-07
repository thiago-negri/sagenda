module Sagenda.Service (getAllUsers, getUserByName) where

import Hasql.Connection (Connection)
import Hasql.Session (run)

import Sagenda.Database.Session (allUsers, findUserByName)
import Sagenda.Data.User (User)
    
getAllUsers :: Connection -> IO [User]
getAllUsers connection = do
    -- FIXME(tnegri): Handle error
    Right rows <- run allUsers connection
    return $ toList rows

getUserByName :: Connection -> Text -> IO (Maybe User)
getUserByName connection name = do
    -- FIXME(tnegri): Handle error
    Right row <- run (findUserByName name) connection
    return row
