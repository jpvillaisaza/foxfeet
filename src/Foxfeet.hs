module Foxfeet (main) where

import Control.Monad (filterM)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (traverse_)
import Data.List (find)
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Encoding (decodeUtf8)
import Foxfeet.Opt
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative (execParser)
import Text.HTML.TagSoup

main :: IO ()
main = do
  opt <- execParser opts
  manager <- newTlsManager
  request <- parseRequest (optUrl opt)
  response <- httpLbs request manager
  let body = responseBody response
  let bodyT = decodeUtf8 body
  let tags = parseTags bodyT
  let h = getFeeds tags
  feeds <-
    if optCheck opt
      then filterM (checkFeed manager) h
      else pure h
  traverse_ printFeed feeds

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

checkFeed :: Manager -> Feed -> IO Bool
checkFeed manager feed = do
  request <- parseRequest (unpack (feedHref feed))
  response <- httpLbs request manager
  let body = responseBody response
  case unpack (feedType feed) of
    t | t == "application/json" || t == "application/feed+json"-> do
      case decode body of
        Just (Object o) ->
          case KeyMap.lookup (fromString "version") o of
            Just (String ver) ->
              pure (Text.isPrefixOf (Text.pack "https://jsonfeed.org/version/") ver)
            _ ->
              pure False
        _ ->
          pure False
    _ -> do
      let bodyT = decodeUtf8 body
      let tags = parseTags bodyT
      case find (\t -> isTagOpenName (pack "rss") t || isTagOpenName (pack "feed") t) tags of
        Just _ ->
          pure True
        Nothing ->
          pure False
