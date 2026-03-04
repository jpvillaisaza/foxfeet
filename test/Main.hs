{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- base
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)

-- foxfeet
import Foxfeet.Feed
import Paths_foxfeet

-- hspec
import Test.Hspec (Spec, context, describe, hspec, it, shouldBe)
import qualified Test.Hspec as Hspec

-- http-client-tls
import Network.HTTP.Client.TLS (newTlsManager)

-- network-uri
import Network.URI (URI (..), URIAuth (..), nullURI)

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
        let url = baseUrl { uriPath = "/no-feeds.html" }
        feeds <- discoverFeeds manager url False False
        feeds `shouldBe` mempty
      it "accepts several" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/"}
        feeds <- discoverFeeds manager url False False
        length feeds `shouldBe` 4
      it "checks" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/"}
        feeds <- discoverFeeds manager url True False
        length feeds `shouldBe` 3
      it "guesses" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/no-feeds.html"}
        feeds <- discoverFeeds manager url False True
        length feeds `shouldBe` 2
    describe "previewFeed" $ do
      it "no feed" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/" }
        items <- previewFeed manager url
        items `shouldBe` mempty
      it "Atom" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/atom.xml" }
        items <- previewFeed manager url
        length items `shouldBe` 3
      it "JSON" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/feed.json" }
        items <- previewFeed manager url
        length items `shouldBe` 3
      it "RSS" $ \baseUrl -> do
        manager <- newTlsManager
        let url = baseUrl { uriPath = "/rss.xml" }
        items <- previewFeed manager url
        length items `shouldBe` 3
  describe "parseFeeds" $ do
    it "no feeds" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \</html>"
      parseFeeds nullURI html
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
      parseFeeds nullURI html
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
      parseFeeds nullURI html
        `shouldBe` [Feed Rss Nothing "/rss" "application/rss+xml"]
    it "Atom" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\" type=\"application/atom+xml\" href=\"/atom\">\
          \  </head>\
          \</html>"
      parseFeeds nullURI html
        `shouldBe` [Feed Atom Nothing "/atom" "application/atom+xml"]
    it "JSON" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\" type=\"application/feed+json\" href=\"/json\">\
          \  </head>\
          \</html>"
      parseFeeds nullURI html
        `shouldBe` [Feed Json Nothing "/json" "application/feed+json"]
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
      parseFeeds nullURI html
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
      parseFeeds nullURI html
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
      parseFeeds nullURI html
        `shouldBe` [Feed Rss (Just "a") "/rss" "application/rss+xml"]
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
      parseFeeds nullURI html
        `shouldBe`
        [ Feed Rss Nothing "/rss" "application/rss+xml"
        , Feed Atom Nothing "/atom" "application/atom+xml"
        , Feed Json Nothing "/json" "application/feed+json"
        ]
  describe "parseItems" $ do
    context "Atom" $ do
      it "no items" $ do
        let
          xml =
            "<feed>\
            \</feed>"
        parseItems xml
          `shouldBe` []
      it "items" $ do
        let
          xml =
            "<feed>\
            \<entry>\
            \<title>Item 1</title>\
            \<link href=\"link 1\"/>\
            \</entry>\
            \</feed>"
        let items = parseItems xml
        length items `shouldBe` 1
    context "JSON" $ do
      it "no items" $ do
        let
          json =
            "{\
            \}"
        parseItems json
          `shouldBe` []
      it "items" $ do
        let
          json =
            "{\
            \\"version\": \"https://jsonfeed.org/version/1.1\",\
            \\"items\": [\
            \{\
            \\"id\": \"1\",\
            \\"title\": \"Item 1\",\
            \\"url\": \"link 1\"\
            \}\
            \]\
            \}"
        let items = parseItems json
        length items `shouldBe` 1
    context "RSS" $ do
      it "no items" $ do
        let
          xml =
            "<rss>\
            \<channel>\
            \</channel>\
            \</rss>"
        parseItems xml
          `shouldBe` []
      it "items" $ do
        let
          xml =
            "<rss>\
            \<channel>\
            \<item>\
            \<title>Item 1</title>\
            \<link>Link 1</link>\
            \</item>\
            \</channel>\
            \</rss>"
        let items = parseItems xml
        length items `shouldBe` 1

withStaticServer :: (URI -> IO ()) -> IO ()
withStaticServer f =
  bracket start killThread (const (f url))
  where
    start = do
      settings <- fmap defaultFileServerSettings (getDataFileName "test/data")
      forkIO (run 8000 (staticApp settings))
    url =
      URI "http:" (Just (URIAuth "" "localhost" ":8000")) "" "" ""
