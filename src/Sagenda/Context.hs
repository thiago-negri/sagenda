module Sagenda.Context (SagendaContext(..)) where

import Hasql.Connection (Connection)
import qualified Data.Vault.Lazy as V

data SagendaContext = SagendaContext {
        userKey :: V.Key (Int32, Text),
        connection :: Connection
    }
