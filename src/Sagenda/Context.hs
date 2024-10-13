module Sagenda.Context (SagendaContext(..)) where

import Hasql.Connection (Connection)
import qualified Data.Vault.Lazy as V
import Sagenda.Data.User (User)

data SagendaContext = SagendaContext {
        userKey :: V.Key User,
        connection :: Connection
    }
