module Foxfeet.Opt
  ( Command (..)
  , DiscoverOpt (..)
  , PreviewOpt (..)
  , commandParserInfo
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

discoverOptParser :: Parser DiscoverOpt
discoverOptParser =
  DiscoverOpt
    <$> checkParser
    <*> guessParser
    <*> urlParser

checkParser :: Parser Bool
checkParser =
  let
    mods =
      [ long "check"
      , help "the help for check"
      ]
  in
    switch (fold mods)

guessParser :: Parser Bool
guessParser =
  let
    mods =
      [ long "guess"
      , help "the help for guess"
      ]
  in
    switch (fold mods)

urlParser :: Parser URI
urlParser =
  pUrl1 <|> pUrl2

pUrl1 :: Parser URI
pUrl1 =
  let
    mods =
      [ long "url"
      , metavar "URL"
      , help "the help for url"
      ]
  in
    option (maybeReader parseURI) (fold mods)

pUrl2 :: Parser URI
pUrl2 =
  let
    mods =
      [ metavar "URL"
      , help "the other help for URL"
      ]
  in
    argument (maybeReader parseURI) (fold mods)

commandParserInfo :: ParserInfo Command
commandParserInfo =
  let
    cmds =
      [ command "discover" (info (fmap Discover discoverOptParser) (progDesc "discover desc"))
      , command "preview" (info (fmap Preview previewOptParser) (progDesc "preview desc"))
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

previewOptParser :: Parser PreviewOpt
previewOptParser =
  PreviewOpt
    <$> urlParser
