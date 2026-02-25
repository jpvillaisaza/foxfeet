module Foxfeet (main) where

import Foxfeet.Feed.Discover (discover)
import Foxfeet.Feed.Preview (preview)
import Foxfeet.Opt
  ( Command (..)
  , DiscoverOptions (..)
  , PreviewOptions (..)
  , Options (..)
  , optionsParserInfo
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative (execParser)

main :: IO ()
main = do
  options <- execParser optionsParserInfo
  manager <- newTlsManager
  case optionsCommand options of
    Discover (DiscoverOptions check guess url) ->
      discover manager url check guess
    Preview (PreviewOptions url) ->
      preview manager url
