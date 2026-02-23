module Foxfeet.Opt
  ( Opt (..)
  , opts
  ) where

import Data.Foldable (fold)
import Data.Version (showVersion)
import Network.URI
import Options.Applicative
import Paths_foxfeet (version)

data Opt = Opt
  { optCheck :: Bool
  , optGuess :: Bool
  , optPreview :: Bool
  , optUrl :: URI
  }

opt :: Parser Opt
opt =
  Opt
    <$> check
    <*> guess
    <*> previewP
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

previewP :: Parser Bool
previewP =
  let
    mods =
      [ long "preview"
      , help "preview"
      ]
  in
    switch (fold mods)

pUrl :: Parser URI
pUrl =
  pUrl1 <|> pUrl2

pUrl1 :: Parser URI
pUrl1 =
  let
    mods =
      [ long "url"
      , metavar "URL"
      , help mempty
      ]
  in
    option (maybeReader parseURI) (fold mods)

pUrl2 :: Parser URI
pUrl2 =
  let
    mods =
      [ metavar "URL"
      , help mempty
      ]
  in
    argument (maybeReader parseURI) (fold mods)

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
