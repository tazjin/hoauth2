{- |A simple OAuth2 Haskell binding. This module exports convenience functions based on http-conduit that make it
    very easy to get up and running (check the examples folder in the repository for details), however you can also
    import "Network.OAuth.OAuth2.Internal" directly and work with your own implementation.
-}
module Network.OAuth.OAuth2 (
  -- ^ Types
    Internal.OAuth2(..)
  , Internal.AccessToken(..)
  , Internal.OAuth2Result

  -- ^ Retrieve access token
  , Http.requestAccessToken
  , Http.refreshAccessToken

  -- ^ Simple authenticated requests
  , Http.getReqAuth
  , Http.postReqAuth
  , Http.authenticatedRequest

  -- ^ Utilities
  , Http.getResponseBody

  ) where

import           Network.OAuth.OAuth2.HttpClient as Http
import           Network.OAuth.OAuth2.Internal   as Internal
