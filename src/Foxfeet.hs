module Foxfeet (main) where

import Foxfeet.Feed.Discover (discover)
import Foxfeet.Feed.Preview (preview)
import Foxfeet.Opt (Command(..), DiscoverOpt (..), PreviewOpt (..), commandParserInfo)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative (execParser)

main :: IO ()
main = do
  command <- execParser commandParserInfo
  manager <- newTlsManager
  case command of
    Discover (DiscoverOpt check guess url) ->
      discover manager url check guess
    Preview (PreviewOpt url) ->
      preview manager url
