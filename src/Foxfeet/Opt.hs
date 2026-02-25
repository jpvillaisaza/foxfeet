module Foxfeet.Opt
  ( Options (..)
  , optionsParserInfo
  , Command (..)
  , DiscoverOptions (..)
  , PreviewOptions (..)
  ) where

import Control.Applicative ((<**>))
import Data.Foldable (fold)
import Data.Version (showVersion)
import Network.URI (URI, parseURI)
import qualified Options.Applicative as Options
import Paths_foxfeet (version)

newtype Options = Options
  { optionsCommand :: Command
  }

optionsParserInfo :: Options.ParserInfo Options
optionsParserInfo =
  let
    optionsParser =
      fmap Options commandParser
        <**> Options.helper
        <**> Options.simpleVersioner (showVersion version)
    mods =
      [ Options.fullDesc
      , Options.header mempty
      , Options.progDesc mempty
      , Options.footer mempty
      ]
  in
    Options.info optionsParser (fold mods)

data Command
  = Discover DiscoverOptions
  | Preview PreviewOptions

commandParser :: Options.Parser Command
commandParser =
  let
    discoverParserInfo =
      let
        mods =
          [ Options.progDesc "Discover feeds"
          ]
      in
        Options.info (fmap Discover discoverOptionsParser) (fold mods)
    previewParserInfo =
      let
        mods =
          [ Options.progDesc "Preview a feed"
          ]
      in
        Options.info (fmap Preview previewOptionsParser) (fold mods)
    cmds =
      [ Options.command "discover" discoverParserInfo
      , Options.command "preview" previewParserInfo
      ]
  in
    Options.hsubparser (fold cmds)

data DiscoverOptions = DiscoverOptions
  { discoverOptionsCheck :: Bool
  , discoverOptionsGuess :: Bool
  , discoverOptionsUrl :: URI
  }

discoverOptionsParser :: Options.Parser DiscoverOptions
discoverOptionsParser =
  DiscoverOptions
    <$> checkParser
    <*> guessParser
    <*> mkUrlParser "URL" "URL to discover"

checkParser :: Options.Parser Bool
checkParser =
  let
    mods =
      [ Options.long "check"
      , Options.help "the help for check"
      ]
  in
    Options.switch (fold mods)

guessParser :: Options.Parser Bool
guessParser =
  let
    mods =
      [ Options.long "guess"
      , Options.help "the help for guess"
      ]
  in
    Options.switch (fold mods)

mkUrlParser :: String -> String -> Options.Parser URI
mkUrlParser mv h =
  let
    mods =
      [ Options.metavar mv
      , Options.help h
      ]
  in
    Options.argument (Options.maybeReader parseURI) (fold mods)

newtype PreviewOptions = PreviewOptions
  { previewOptionsUrl :: URI
  }

previewOptionsParser :: Options.Parser PreviewOptions
previewOptionsParser =
  PreviewOptions
    <$> mkUrlParser "FEED_URL" "Feed URL to preview"
