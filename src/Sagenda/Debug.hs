module Sagenda.Debug (debugLog) where

import System.IO

debugLog :: String -> IO ()
debugLog t = putStrLn t >> hFlush stdout
