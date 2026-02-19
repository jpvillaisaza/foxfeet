module Foxfeet.Feed where

import Control.Monad (filterM, when)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (traverse_)
import Data.List (find)
import Data.Maybe (catMaybes, mapMaybe)
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Version (showVersion)
import Foxfeet.Opt
import Network.HTTP.Client
import Network.HTTP.Types (hUserAgent)
import Network.URI
import Paths_foxfeet (version)
import Text.HTML.TagSoup

addUserAgent :: Request -> Request
addUserAgent request =
  let
    t = (hUserAgent, ByteString.pack ("foxfeet/" <> showVersion version))
  in
    request { requestHeaders = t : requestHeaders request }

discover :: Manager -> Opt -> IO ()
discover manager opt = do
  request <- parseRequest (show (optUrl opt))
  response <- httpLbs (addUserAgent request) manager
  let body = responseBody response
  let h = extractFeeds (optUrl opt) (decodeUtf8 body)
  feeds <-
    if optCheck opt
      then filterM (checkFeed manager) h
      else pure h
  traverse_ printFeed feeds
  when (optGuess opt && null feeds) $ do
    guessed <- guess manager (optUrl opt)
    traverse_ printFeed guessed

extractFeeds :: URI -> Text -> [Feed]
extractFeeds url =
  getFeeds url . parseTags

getFeeds :: URI -> [Tag Text] -> [Feed]
getFeeds url =
  mapMaybe (parseFeed url)
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

parseFeed :: URI -> Tag Text -> Maybe Feed
parseFeed url tag =
  if isFeed tag
    then
      case parseURIReference (unpack href) of
        Just hrefuri ->
          Just Feed
            { feedHref = pack (show (relativeTo hrefuri url))
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

printFeed :: Feed -> IO ()
printFeed feed = do
  TIO.putStrLn (pack "- " <> feedHref feed <> pack " (" <> feedType feed <> pack ")")

data Feed = Feed
  { feedHref :: Text
  , feedTitle :: Maybe Text
  , feedType :: Text
  }
  deriving (Eq, Show)

checkFeed :: Manager -> Feed -> IO Bool
checkFeed manager feed = do
  request <- parseRequest (unpack (feedHref feed))
  response <- httpLbs (addUserAgent request) manager
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

guess :: Manager -> URI -> IO [Feed]
guess manager base =
  filterM (checkFeed manager) (catMaybes feeds)
  where
    feeds =
      [ case parseURIReference "rss.xml" of
          Just p ->
            Just (Feed (pack (show (relativeTo p base))) Nothing (pack "application/rss+xml"))
          Nothing ->
            Nothing
      , case parseURIReference "atom.xml" of
          Just p ->
            Just (Feed (pack (show (relativeTo p base))) Nothing (pack "application/atom+xml"))
          Nothing ->
            Nothing
      , case parseURIReference "feed" of
          Just p ->
            Just (Feed (pack (show (relativeTo p base))) Nothing (pack "application/rss+xml"))
          Nothing ->
            Nothing
      , case parseURIReference "rss" of
          Just p ->
            Just (Feed (pack (show (relativeTo p base))) Nothing (pack "application/rss+xml"))
          Nothing ->
            Nothing
      ]
