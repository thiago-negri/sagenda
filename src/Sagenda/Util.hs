module Sagenda.Util (
    require
) where

import Control.Monad.Trans.Except (throwE)

require :: Monad m => e -> ExceptT e m (Maybe a) -> ExceptT e m a
require e f = f >>= maybe (throwE e) return
