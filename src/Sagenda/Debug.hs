module Sagenda.Debug (debugLog) where

import System.IO

debugLog :: String -> IO ()
debugLog e = putStrLn e >> hFlush stdout
