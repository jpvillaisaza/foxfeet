module Foxfeet (main) where

import Foxfeet.Feed (discover, preview)
import Foxfeet.Opt (Opt(..), opts)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative (execParser)

main :: IO ()
main = do
  opt <- execParser opts
  manager <- newTlsManager
  if optPreview opt
    then preview manager (optUrl opt)
    else discover manager opt
