module Foxfeet.Feed.Discover where

import Control.Monad (filterM)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import Foxfeet.Feed.Preview (previewFeed)
import Foxfeet.Http (addUserAgent)
import Network.HTTP.Client
import Network.URI
import Text.HTML.TagSoup

data Feed = Feed
  { feedTitle :: Maybe Text
  , feedUrl :: Text
  , feedType :: Text
  }
  deriving (Eq, Show)

discoverFeeds :: Manager -> URI -> Bool -> Bool -> IO [Feed]
discoverFeeds manager url check guess = do
  request <- parseRequest (show url)
  response <- httpLbs (addUserAgent request) manager
  let body = responseBody response
  let h = parseFeeds url (decodeUtf8 body)
  feeds <-
    if check
      then filterM (checkFeed manager) h
      else pure h
  if guess && null feeds
    then guessFeeds manager url
    else pure feeds

parseFeeds :: URI -> Text -> [Feed]
parseFeeds url =
  mapMaybe (parseFeed url)
    . filter (isTagOpenName (pack "link"))
    . takeWhile (not . isTagCloseName (pack "head"))
    . dropWhile (not . isTagOpenName (pack "head"))
    . parseTags

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

parseFeed :: URI -> Tag Text -> Maybe Feed
parseFeed url tag =
  if isFeed tag
    then
      case parseURIReference (unpack href) of
        Just hrefuri ->
          Just Feed
            { feedUrl = pack (show (relativeTo hrefuri url))
            , feedTitle = parseTitle tag
            , feedType = fromAttrib (pack "type") tag
            }
        Nothing ->
          Nothing
    else Nothing
  where
    href =
      fromAttrib (pack "href") tag

parseTitle :: Tag Text -> Maybe Text
parseTitle tag =
  case fromAttrib (pack "title") tag of
    t | t == pack "" ->
      Nothing
    t ->
      Just t

checkFeed :: Manager -> Feed -> IO Bool
checkFeed manager feed =
  case parseURI (unpack (feedUrl feed)) of
    Just url -> do
      items <- previewFeed manager url
      pure (items /= mempty)
    Nothing ->
      pure False

guessFeeds :: Manager -> URI -> IO [Feed]
guessFeeds manager base =
  filterM (checkFeed manager) (catMaybes feeds)
  where
    feeds =
      [ case parseURIReference "rss.xml" of
          Just p ->
            Just (Feed Nothing (pack (show (relativeTo p base))) (pack "application/rss+xml"))
          Nothing ->
            Nothing
      , case parseURIReference "atom.xml" of
          Just p ->
            Just (Feed Nothing (pack (show (relativeTo p base))) (pack "application/atom+xml"))
          Nothing ->
            Nothing
      , case parseURIReference "feed" of
          Just p ->
            Just (Feed Nothing (pack (show (relativeTo p base))) (pack "application/rss+xml"))
          Nothing ->
            Nothing
      , case parseURIReference "rss" of
          Just p ->
            Just (Feed Nothing (pack (show (relativeTo p base))) (pack "application/rss+xml"))
          Nothing ->
            Nothing
      ]

renderFeed :: Feed -> Text
renderFeed feed =
  Text.Lazy.unlines
    [ pack "- url: " <> feedUrl feed
    , pack "  type: " <> feedType feed
    ]
