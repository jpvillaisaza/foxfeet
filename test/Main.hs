module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import qualified Data.Text.Lazy as Text
import Foxfeet.Feed
import Network.HTTP.Client.TLS (newTlsManager)
import Network.URI (URI (..), URIAuth (..), nullURI)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import qualified Test.Hspec as Hspec

main :: IO ()
main =
  hspec (Hspec.parallel spec)

spec :: Spec
spec = do
  Hspec.aroundAll withStaticServer $
    describe "discoverFeeds" $ do
      it "" $ \url -> do
        manager <- newTlsManager
        feeds <- discoverFeeds manager url False False
        feeds `shouldBe` mempty
      it "" $ \url -> do
        manager <- newTlsManager
        feeds <- discoverFeeds manager url False False
        feeds `shouldBe` mempty
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
    start =
      forkIO $
        run 8000
          (staticApp (defaultFileServerSettings "static"))
    url =
      URI "http:" (Just (URIAuth "" "localhost" ":8000")) "" "" ""
