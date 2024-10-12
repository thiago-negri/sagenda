module Sagenda.Error (
    AppError(..)
) where

import Hasql.Connection (ConnectionError)

data AppError = MissingEnvVar String
              | DatabaseConnectionError ConnectionError

instance Show AppError where
    show (MissingEnvVar e) = "missing env var: " ++ show e
    show (DatabaseConnectionError e) = "connection error: " ++ show e
