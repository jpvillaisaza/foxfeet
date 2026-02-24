module Main (main) where

import qualified Foxfeet.Feed.DiscoverSpec as Foxfeet.Feed.Discover
import Test.Hspec (hspec, parallel)

main :: IO ()
main =
  hspec
    (parallel Foxfeet.Feed.Discover.spec)
