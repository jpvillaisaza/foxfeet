module Foxfeet.Feed where

import Control.Applicative
import Control.Monad (filterM)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Lazy (ByteString)
import Data.List (find)
import Data.Maybe (catMaybes, mapMaybe)
import Data.String (IsString (..))
import Data.Text.Lazy (Text, fromStrict, pack, unpack)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Vector (toList)
import Data.Version (showVersion)
import Network.HTTP.Client
import Network.HTTP.Types (hUserAgent)
import Network.URI
import Paths_foxfeet (version)
import Text.HTML.TagSoup

data Feed = Feed
  { feedFormat :: Format
  , feedTitle :: Maybe Text
  , feedUrl :: Text
  , feedType :: Text
  }
  deriving (Eq, Show)

renderFeed :: Feed -> Text
renderFeed feed =
  Text.Lazy.unlines
    [ pack "- url: " <> feedUrl feed
    , pack "  type: " <> prettyFormat (feedFormat feed)
    ]

data Format
  = Atom
  | Json
  | Rss
  deriving (Eq, Show)

parseFormat :: Text -> Maybe Format
parseFormat =
  flip lookup
    [ (pack "application/atom+xml", Atom)
    , (pack "application/feed+json", Json)
    , (pack "application/json", Json)
    , (pack "application/rss+xml", Rss)
    ]

prettyFormat :: IsString string => Format -> string
prettyFormat format =
  case format of
    Atom ->
      fromString "Atom"
    Json ->
      fromString "JSON"
    Rss ->
      fromString "RSS"

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

parseFeed :: URI -> Tag Text -> Maybe Feed
parseFeed url tag =
  case mFormat of
    Just format | isFeed tag ->
      case parseURIReference (unpack href) of
        Just hrefuri ->
          Just Feed
            { feedFormat = format
            , feedUrl = pack (show (relativeTo hrefuri url))
            , feedTitle = parseTitle tag
            , feedType = fromAttrib (pack "type") tag
            }
        Nothing ->
          Nothing
    _ ->
      Nothing
  where
    href =
      fromAttrib (pack "href") tag
    mFormat =
      parseFormat (fromAttrib (pack "type") tag)

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
            Just (Feed Rss Nothing (pack (show (relativeTo p base))) (pack "application/rss+xml"))
          Nothing ->
            Nothing
      , case parseURIReference "atom.xml" of
          Just p ->
            Just (Feed Atom Nothing (pack (show (relativeTo p base))) (pack "application/atom+xml"))
          Nothing ->
            Nothing
      , case parseURIReference "feed" of
          Just p ->
            Just (Feed Rss Nothing (pack (show (relativeTo p base))) (pack "application/rss+xml"))
          Nothing ->
            Nothing
      , case parseURIReference "rss" of
          Just p ->
            Just (Feed Rss Nothing (pack (show (relativeTo p base))) (pack "application/rss+xml"))
          Nothing ->
            Nothing
      ]

data Item =
  Item
    { itemTitle :: Maybe Text
    , itemUrl :: Text
    , itemPubDate :: Maybe Text
    }
  deriving (Eq, Show)

previewFeed :: Manager -> URI -> IO [Item]
previewFeed manager url = do
  request <- parseRequest (show url)
  response <- httpLbs (addUserAgent request) manager
  let items = parseItems (responseBody response)
  pure items

parseItems :: ByteString -> [Item]
parseItems body =
  parseItems1 tags
    <|> parseItems2 tags
    <|> parseItems3 body
  where
    tags =
      parseTags (decodeUtf8 body)

parseItems1 :: [Tag Text] -> [Item]
parseItems1 =
  concatMap (mapMaybe parseItem . sections (~== "<item>"))
    . sections (~== "<rss>")

parseItems2 :: [Tag Text] -> [Item]
parseItems2 =
  concatMap (mapMaybe parseItem2 . sections (~== "<entry>"))
    . sections (~== "<feed>")

findBetween :: Text -> [Tag Text] -> Maybe [Tag Text]
findBetween _ [] = Nothing
findBetween name (x:xs)
  | isTagOpenName name x =
      let ys = takeWhile (not . isTagCloseName name) xs
      in Just ys
  | otherwise = findBetween name xs

parseItem :: [Tag Text] -> Maybe Item
parseItem tags =
  case findBetween (pack "link") tags of
    Just ts ->
      Just Item
        { itemTitle = fmap innerText (findBetween (pack "title") tags)
        , itemUrl = innerText ts
        , itemPubDate = fmap innerText (findBetween (pack "pubDate") tags)
        }
    Nothing ->
      Nothing

parseItem2 :: [Tag Text] -> Maybe Item
parseItem2 tags =
  case find (isTagOpenName (pack "link")) tags of
    Just tag ->
      Just Item
        { itemTitle = fmap innerText (findBetween (pack "title") tags)
        , itemUrl = fromAttrib (pack "href") tag
        , itemPubDate = fmap innerText (findBetween (pack "pubDate") tags)
        }
    Nothing ->
      Nothing

parseItems3 :: ByteString -> [Item]
parseItems3 body =
  case decode body of
    Just (Object o) ->
      case KeyMap.lookup (fromString "items") o of
        Just (Array items) ->
          mapMaybe parseItem3 (toList items)
        _ ->
          []
    _ ->
      []

parseItem3 :: Value -> Maybe Item
parseItem3 (Object o) = do
  String t <- KeyMap.lookup (fromString "title") o
  String l <- KeyMap.lookup (fromString "url") o <|> KeyMap.lookup (fromString "external_url") o
  let d = case KeyMap.lookup (fromString "date_published") o of
            Just (String x) -> Just x
            _ -> Nothing
  pure $ Item (Just (fromStrict t)) (fromStrict l) (fmap fromStrict d)
parseItem3 _ = Nothing

renderItem :: Item -> Text
renderItem item =
  case itemTitle item of
    Just title ->
      Text.Lazy.unlines
        [ pack "- title: " <> title
        , pack "  url: " <> itemUrl item
        ]
    Nothing ->
      Text.Lazy.unlines
        [ pack "- url: " <> itemUrl item
        ]

addUserAgent :: Request -> Request
addUserAgent request =
  let
    t = (hUserAgent, ByteString.pack ("foxfeet/" <> showVersion version))
  in
    request { requestHeaders = t : requestHeaders request }
