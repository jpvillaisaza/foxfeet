module Foxfeet.Opt
  ( Opt (..)
  , opts
  ) where

import Data.Foldable (fold)
import Data.Version (showVersion)
import Options.Applicative
import Paths_foxfeet (version)

data Opt = Opt
  { optCheck :: Bool
  , optGuess :: Bool
  , optUrl :: String
  }

opt :: Parser Opt
opt =
  Opt
    <$> check
    <*> guess
    <*> pUrl

check :: Parser Bool
check =
  let
    mods =
      [ long "check"
      , help mempty
      ]
  in
    switch (fold mods)

guess :: Parser Bool
guess =
  let
    mods =
      [ long "guess"
      , help mempty
      ]
  in
    switch (fold mods)

pUrl :: Parser String
pUrl =
  pUrl1 <|> pUrl2

pUrl1 :: Parser String
pUrl1 =
  let
    mods =
      [ long "url"
      , metavar "URL"
      , help mempty
      ]
  in
    strOption (fold mods)

pUrl2 :: Parser String
pUrl2 =
  let
    mods =
      [ metavar "URL"
      , help mempty
      ]
  in
    argument str (fold mods)

opts :: ParserInfo Opt
opts =
  let
    ppp =
      opt
        <**> helper
        <**> simpleVersioner (showVersion version)
    mods =
      [ fullDesc
      , header mempty
      , progDesc mempty
      , footer mempty
      ]
  in
    info ppp (fold mods)
