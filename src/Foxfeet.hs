module Foxfeet (main) where

import Foxfeet.Feed.Discover (discover)
import Foxfeet.Feed.Preview (preview)
import Foxfeet.Opt (Command(..), opts)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative (execParser)

main :: IO ()
main = do
  command <- execParser opts
  manager <- newTlsManager
  case command of
    Discover opt ->
      discover manager opt
    Preview opt ->
      preview manager opt
