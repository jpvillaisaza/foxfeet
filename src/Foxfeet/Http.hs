module Foxfeet.Http
  ( addUserAgent
  ) where

import qualified Data.ByteString.Char8 as ByteString
import Data.Version (showVersion)
import Network.HTTP.Client (Request (requestHeaders))
import Network.HTTP.Types (hUserAgent)
import Paths_foxfeet (version)

addUserAgent :: Request -> Request
addUserAgent request =
  let
    t = (hUserAgent, ByteString.pack ("foxfeet/" <> showVersion version))
  in
    request { requestHeaders = t : requestHeaders request }
