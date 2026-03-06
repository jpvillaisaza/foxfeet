module Foxfeet.Main
  ( main
  , Options (..)
  , Command (..)
  , DiscoverOptions (..)
  , PreviewOptions (..)
  ) where

-- base
import Control.Applicative ((<**>), optional)
import Data.Foldable (fold)
import Data.Version (showVersion)

-- foxfeet
import Foxfeet.Feed
import Paths_foxfeet (version)

-- http-client-tls
import Network.HTTP.Client.TLS (newTlsManager)

-- network-uri
import Network.URI (URI, parseURI)

-- optparse-applicative
import Options.Applicative (execParser)
import qualified Options.Applicative as Options

-- text
import qualified Data.Text.Lazy.IO as Text.Lazy.IO

main :: IO ()
main = do
  options <- execParser optionsParserInfo
  manager <- newTlsManager
  case optionsCommand options of
    Discover (DiscoverOptions check guess json url) -> do
      feeds <- discoverFeeds manager url check guess
      Text.Lazy.IO.putStrLn (renderFeeds json feeds)
    Preview (PreviewOptions json mLimit url) -> do
      items <- fmap (maybe id take mLimit) (previewFeed manager url)
      Text.Lazy.IO.putStrLn (renderItems json items)

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
  , discoverOptionsJson :: Bool
  , discoverOptionsUrl :: URI
  }

discoverOptionsParser :: Options.Parser DiscoverOptions
discoverOptionsParser =
  DiscoverOptions
    <$> checkParser
    <*> guessParser
    <*> jsonParser
    <*> mkUrlParser "URL" "URL to discover feeds"

checkParser :: Options.Parser Bool
checkParser =
  let
    mods =
      [ Options.long "check"
      , Options.help "Validate feeds"
      ]
  in
    Options.switch (fold mods)

guessParser :: Options.Parser Bool
guessParser =
  let
    mods =
      [ Options.long "guess"
      , Options.help "Guess feeds"
      ]
  in
    Options.switch (fold mods)

jsonParser :: Options.Parser Bool
jsonParser =
  let
    mods =
      [ Options.long "json"
      , Options.help "JSON output"
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

data PreviewOptions = PreviewOptions
  { previewOptionsJson :: Bool
  , previewOptionsLimit :: Maybe Int
  , previewOptionsUrl :: URI
  }

previewOptionsParser :: Options.Parser PreviewOptions
previewOptionsParser =
  PreviewOptions
    <$> jsonParser
    <*> optional limitParser
    <*> mkUrlParser "FEED_URL" "Feed URL to preview"

limitParser :: Options.Parser Int
limitParser =
  let
    mods =
      [ Options.long "limit"
      , Options.help "Limit"
      ]
  in
    Options.option Options.auto (fold mods)
