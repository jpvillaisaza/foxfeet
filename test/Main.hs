module Main (main) where

-- base
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)

-- foxfeet
import Foxfeet.Feed
import Paths_foxfeet

-- hspec
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import qualified Test.Hspec as Hspec

-- http-client-tls
import Network.HTTP.Client.TLS (newTlsManager)

-- network-uri
import Network.URI (URI (..), URIAuth (..), nullURI)

-- text
import qualified Data.Text.Lazy as Text

-- wai-app-static
import Network.Wai.Application.Static

-- warp
import Network.Wai.Handler.Warp

main :: IO ()
main =
  hspec (Hspec.parallel spec)

spec :: Spec
spec = do
  Hspec.aroundAll withStaticServer $ do
    describe "discoverFeeds" $ do
      it "no feeds" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/pages/no-feeds" }
        feeds <- discoverFeeds manager url False False
        feeds `shouldBe` mempty
      it "accepts several" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/pages"}
        feeds <- discoverFeeds manager url False False
        length feeds `shouldBe` 3
    describe "previewFeed" $ do
      it "" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/" }
        items <- previewFeed manager url
        items `shouldBe` mempty
      it "Atom" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/feeds/atom.xml" }
        items <- previewFeed manager url
        length items `shouldBe` 3
      it "JSON" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/feeds/feed.json" }
        items <- previewFeed manager url
        length items `shouldBe` 3
      it "RSS" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/feeds/rss.xml" }
        items <- previewFeed manager url
        length items `shouldBe` 3
  describe "parseFeeds" $ do
    it "no feeds" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \</html>"
      parseFeeds nullURI (Text.pack html)
        `shouldBe` []
    it "alternate feed missing values" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\">\
          \  </head>\
          \</html>"
      parseFeeds nullURI (Text.pack html)
        `shouldBe` []
    it "RSS" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\" type=\"application/rss+xml\" href=\"/rss\">\
          \  </head>\
          \</html>"
      parseFeeds nullURI (Text.pack html)
        `shouldBe` [Feed Rss Nothing (Text.pack "/rss") (Text.pack "application/rss+xml")]
    it "Atom" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\" type=\"application/atom+xml\" href=\"/atom\">\
          \  </head>\
          \</html>"
      parseFeeds nullURI (Text.pack html)
        `shouldBe` [Feed Atom Nothing (Text.pack "/atom") (Text.pack "application/atom+xml")]
    it "JSON" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\" type=\"application/feed+json\" href=\"/json\">\
          \  </head>\
          \</html>"
      parseFeeds nullURI (Text.pack html)
        `shouldBe` [Feed Json Nothing (Text.pack "/json") (Text.pack "application/feed+json")]
    it "skips feed in body" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head></head>\
          \  <body>\
          \    <link rel=\"alternate\" type=\"application/rss+xml\" href=\"/rss\">\
          \  </body>\
          \</html>"
      parseFeeds nullURI (Text.pack html)
        `shouldBe` []
    it "skips feed in pre" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head></head>\
          \  <body>\
          \    <pre>\
          \      <link rel=\"alternate\" type=\"application/rss+xml\" href=\"/rss\">\
          \    </pre>\
          \  </body>\
          \</html>"
      parseFeeds nullURI (Text.pack html)
        `shouldBe` []
    it "accepts title" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\" type=\"application/rss+xml\" href=\"/rss\" title=\"a\">\
          \  </head>\
          \</html>"
      parseFeeds nullURI (Text.pack html)
        `shouldBe` [Feed Rss (Just (Text.pack "a")) (Text.pack "/rss") (Text.pack "application/rss+xml")]
    it "accepts several" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\" type=\"application/rss+xml\" href=\"/rss\">\
          \    <link rel=\"alternate\" type=\"application/atom+xml\" href=\"/atom\">\
          \    <link rel=\"alternate\" type=\"application/feed+json\" href=\"/json\">\
          \  </head>\
          \</html>"
      parseFeeds nullURI (Text.pack html)
        `shouldBe`
        [ Feed Rss Nothing (Text.pack "/rss") (Text.pack "application/rss+xml")
        , Feed Atom Nothing (Text.pack "/atom") (Text.pack "application/atom+xml")
        , Feed Json Nothing (Text.pack "/json") (Text.pack "application/feed+json")
        ]

withStaticServer :: (URI -> IO ()) -> IO ()
withStaticServer f =
  bracket start killThread (const (f url))
  where
    start = do
      settings <- fmap defaultFileServerSettings (getDataFileName "test/data")
      forkIO (run 8000 (staticApp settings))
    url =
      URI "http:" (Just (URIAuth "" "localhost" ":8000")) "" "" ""
