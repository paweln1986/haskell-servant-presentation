module Main where

import Server
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp    (run)

main :: IO ()
main = do
  putStrLn "Starting server on port 8081"
  generateJsClients
  run 8081 $ logStdoutDev application
