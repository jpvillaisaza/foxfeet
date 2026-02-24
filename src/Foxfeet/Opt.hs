module Foxfeet.Opt
  ( Command (..)
  , DiscoverOpt (..)
  , PreviewOpt (..)
  , opts
  ) where

import Data.Foldable (fold)
import Data.Version (showVersion)
import Network.URI
import Options.Applicative
import Paths_foxfeet (version)

data Command
  = Discover DiscoverOpt
  | Preview PreviewOpt

data DiscoverOpt = DiscoverOpt
  { discoverOptCheck :: Bool
  , discoverOptGuess :: Bool
  , discoverOptUrl :: URI
  }

discoverOpt :: Parser DiscoverOpt
discoverOpt =
  DiscoverOpt
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

opts :: ParserInfo Command
opts =
  let
    cmds =
      [ command "discover" (info (fmap Discover discoverOpt) (progDesc "discover desc"))
      , command "preview" (info (fmap Preview previewOpt) (progDesc "preview desc"))
      ]
    opt =
      hsubparser (fold cmds)
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

data PreviewOpt = PreviewOpt
  { previewOptUrl :: URI
  }

previewOpt :: Parser PreviewOpt
previewOpt =
  PreviewOpt
    <$> pUrl
