module Foxfeet (main) where

import Data.Foldable (traverse_)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (getArgs)
import Text.HTML.TagSoup

main :: IO ()
main = do
  args <- getArgs
  case args of
    [url] -> do
      manager <- newTlsManager
      request <- parseRequest url
      response <- httpLbs request manager
      let body = responseBody response
      let bodyT = decodeUtf8 body
      let tags = parseTags bodyT
      let h = getFeeds tags
      traverse_ printFeed h
    _ ->
      undefined

getFeeds :: [Tag Text] -> [Feed]
getFeeds =
  fmap toFeed
    . filter isFeed
    . filter (isTagOpenName (pack "link"))
    . takeWhile (not . isTagCloseName (pack "head"))
    . dropWhile (not . isTagOpenName (pack "head"))

isFeed :: Tag Text -> Bool
isFeed tag =
  fromAttrib (pack "rel") tag == pack "alternate"
    && fromAttrib (pack "type") tag `elem` types

types :: [Text]
types = fmap pack
  [ "application/rss+xml",
  "application/atom+xml",
  "application/feed+json",
  "application/json"
  ]

toFeed :: Tag Text -> Feed
toFeed tag =
  Feed
    { feedHref = fromAttrib (pack "href") tag
    , feedTitle = parseTitle tag
    , feedType = fromAttrib (pack "type") tag
    }

parseTitle :: Tag Text -> Maybe Text
parseTitle tag =
  case fromAttrib (pack "title") tag of
    t | t == pack "" ->
      Nothing
    t ->
      Just t

printFeed :: Feed -> IO ()
printFeed feed = do
  TIO.putStrLn (feedHref feed)
  TIO.putStrLn (feedType feed)
  traverse_ TIO.putStrLn (feedTitle feed)

data Feed = Feed
  { feedHref :: Text
  , feedTitle :: Maybe Text
  , feedType :: Text
  }
