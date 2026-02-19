module Main (main) where

import qualified Foxfeet.FeedSpec as Foxfeet.Feed
import Test.Hspec (hspec, parallel)

main :: IO ()
main =
  hspec
    (parallel Foxfeet.Feed.spec)
