module Sagenda.Context (
        SagendaContext (SagendaContext),
        newContext,
        userKey,
        database
    ) where

import qualified Data.Vault.Lazy as V
import Sagenda.Data.User (User)
import Sagenda.Database (DatabasePool, newDatabaseConnectionPool)
import Sagenda.Error (AppError)
import Sagenda.Debug (debugLog)

data SagendaContext = SagendaContext {
        userKey :: V.Key User,
        database :: DatabasePool
    }

newContext :: ExceptT AppError IO SagendaContext
newContext = do
    lift $ debugLog "Creating vault keys"
    uKey <- lift (V.newKey :: IO (V.Key User))

    lift $ debugLog "Creating database connection pool"
    pool <- newDatabaseConnectionPool
    
    lift $ debugLog ""
    return $ SagendaContext uKey pool
