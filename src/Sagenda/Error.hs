module Sagenda.Error (
    AppError(..)
) where

import Hasql.Connection (ConnectionError)
import Hasql.Session (QueryError)

data AppError = MissingEnvVar String
              | DatabaseConnectionError ConnectionError
              | DatabaseQueryError QueryError
              | AuthError String

instance Show AppError where
    show (MissingEnvVar e) = "missing env var: " ++ show e
    show (DatabaseConnectionError e) = "connection error: " ++ show e
    show (DatabaseQueryError e) = "database query error: " ++ show e
    show (AuthError e) = "auth error: " ++ show e
