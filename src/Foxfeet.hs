module Foxfeet (main) where

import Foxfeet.Feed (discover)
import Foxfeet.Opt (opts)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative (execParser)

main :: IO ()
main = do
  opt <- execParser opts
  manager <- newTlsManager
  discover manager opt
