{-# LANGUAGE OverloadedStrings #-}
module Main where

import Server (generateJsClients, application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Middleware)
import Network.Wai.Metrics (registerWaiMetrics, metrics)
import System.Metrics (registerGcMetrics, newStore)
import System.Remote.Monitoring (serverMetricStore, forkServerWith)

ekg :: Int -> IO Middleware
ekg port = do
    s <- newStore
    registerGcMetrics s
    wai <- registerWaiMetrics s
    _ <- forkServerWith s "localhost" port
    pure (metrics wai)

main :: IO ()
main = do
  putStrLn "Starting server on port 8081"
  generateJsClients
  waiMetrics <- ekg 8000
  run 8081 $ waiMetrics $ logStdoutDev application
