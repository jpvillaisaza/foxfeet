module Foxfeet.FeedSpec where

import qualified Data.Text.Lazy as Text
import Foxfeet.Feed
import Network.URI (nullURI)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "extractFeeds" $ do
    it "no feeds" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \</html>"
      extractFeeds nullURI (Text.pack html)
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
      extractFeeds nullURI (Text.pack html)
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
      extractFeeds nullURI (Text.pack html)
        `shouldBe` [Feed (Text.pack "/rss") Nothing (Text.pack "application/rss+xml")]
    it "Atom" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\" type=\"application/atom+xml\" href=\"/atom\">\
          \  </head>\
          \</html>"
      extractFeeds nullURI (Text.pack html)
        `shouldBe` [Feed (Text.pack "/atom") Nothing (Text.pack "application/atom+xml")]
    it "JSON" $ do
      let
        html =
          "<!doctype html>\
          \<html>\
          \  <head>\
          \    <link rel=\"alternate\" type=\"application/feed+json\" href=\"/json\">\
          \  </head>\
          \</html>"
      extractFeeds nullURI (Text.pack html)
        `shouldBe` [Feed (Text.pack "/json") Nothing (Text.pack "application/feed+json")]
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
      extractFeeds nullURI (Text.pack html)
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
      extractFeeds nullURI (Text.pack html)
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
      extractFeeds nullURI (Text.pack html)
        `shouldBe` [Feed (Text.pack "/rss") (Just (Text.pack "a")) (Text.pack "application/rss+xml")]
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
      extractFeeds nullURI (Text.pack html)
        `shouldBe`
        [ Feed (Text.pack "/rss") Nothing (Text.pack "application/rss+xml")
        , Feed (Text.pack "/atom") Nothing (Text.pack "application/atom+xml")
        , Feed (Text.pack "/json") Nothing (Text.pack "application/feed+json")
        ]
