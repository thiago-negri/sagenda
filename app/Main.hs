module Main (main) where

import Sagenda.Web (webServer)

main :: IO ()
main = do
    webServer
