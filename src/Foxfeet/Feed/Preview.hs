module Foxfeet.Feed.Preview where

import Control.Applicative
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Data.Text.Lazy (Text, fromStrict, pack)
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Vector (toList)
import Data.Version (showVersion)
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

data Item =
  Item
    { itemTitle :: Maybe Text
    , itemUrl :: Text
    , itemPubDate :: Maybe Text
    }
  deriving (Eq, Show)

preview :: Manager -> URI -> IO ()
preview manager url = do
  request <- parseRequest (show url)
  response <- httpLbs (addUserAgent request) manager
  let items = extractItems (responseBody response)
  traverse_ printItem items

extractItems :: ByteString -> [Item]
extractItems body =
  extractItems1 tags
    <|> extractItems2 tags
    <|> extractItems3 body
  where
    tags =
      parseTags (decodeUtf8 body)

extractItems1 :: [Tag Text] -> [Item]
extractItems1 =
  concatMap (mapMaybe parseItem . sections (~== "<item>"))
    . sections (~== "<rss>")

extractItems2 :: [Tag Text] -> [Item]
extractItems2 =
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

printItem :: Item -> IO ()
printItem item =
  case itemTitle item of
    Just title -> do
      TIO.putStrLn (pack "- title: " <> title)
      TIO.putStrLn (pack "  url: " <> itemUrl item)
    Nothing ->
      TIO.putStrLn (pack "- url: " <> itemUrl item)

extractItems3 :: ByteString -> [Item]
extractItems3 body =
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
