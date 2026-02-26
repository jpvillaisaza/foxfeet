module Foxfeet (main) where

import Data.Foldable (traverse_)
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import Foxfeet.Feed.Discover (discoverFeeds, renderFeed)
import Foxfeet.Feed.Preview (previewFeed, renderItem)
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
    Discover (DiscoverOptions check guess url) -> do
      feeds <- discoverFeeds manager url check guess
      traverse_ (Text.Lazy.IO.putStr . renderFeed) feeds
    Preview (PreviewOptions url) -> do
      items <- previewFeed manager url
      traverse_ (Text.Lazy.IO.putStr . renderItem) items
