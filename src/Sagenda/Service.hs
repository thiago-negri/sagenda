module Sagenda.Service (getAllUsers) where

import Hasql.Connection (Connection)
import Hasql.Session (run)

import Sagenda.Database.Session (allUsers)
import Sagenda.Data.User (User)
    
getAllUsers :: Connection -> IO [User]
getAllUsers connection = do
    -- FIXME(tnegri): Handle error
    Right rows <- run allUsers connection
    return $ toList rows
