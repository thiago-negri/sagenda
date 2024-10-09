module Main (main) where

import Sagenda.Web (webServer)

main :: IO ()
main = do
    putStrLn "SAGENDA UP"
    webServer
