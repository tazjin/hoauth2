{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-

This is basically very manual test. Check following link for details.

Google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer

Google OAuth 2.0 playround: https://developers.google.com/oauthplayground/

-}

module Main where

import           Keys                          (googleKey)
import           Network.OAuth.OAuth2

import           Control.Applicative           ((<$>), (<*>))
import           Control.Monad                 (mzero)
import           Data.Aeson                    (FromJSON, Value (Object),
                                                parseJSON, (.:), (.:?))
import           Data.Aeson.TH                 (deriveJSON)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.Text                     (Text)
import           Network.HTTP.Types
import           Prelude                       hiding (id)
import qualified Prelude                       as P (id)
import           System.Environment            (getArgs)

--------------------------------------------------

data Token = Token { issued_to      :: Text
                   , audience       :: Text
                   , user_id        :: Maybe Text
                   , scope          :: Text
                   , expires_in     :: Integer
                   , email          :: Maybe Text
                   , verified_email :: Maybe Bool
                   , access_type    :: Text
                   } deriving (Show)

instance FromJSON Token where
    parseJSON (Object o) = Token
                           <$> o .:  "issued_to"
                           <*> o .:  "audience"
                           <*> o .:? "user_id"
                           <*> o .:  "scope"
                           <*> o .:  "expires_in"
                           <*> o .:? "email"
                           <*> o .:? "verified_email"
                           <*> o .:  "access_type"
    parseJSON _ = mzero

data User = User { id          :: Text
                 , name        :: Text
                 , given_name  :: Text
                 , family_name :: Text
                 , link        :: Text
                 , picture     :: Text
                 , gender      :: Text
                 , birthday    :: Text
                 , locale      :: Text
                 } deriving (Show)

$(deriveJSON P.id ''User)

--------------------------------------------------

main :: IO ()
main = do
    xs <- getArgs
    case xs of
--        ["offline"] -> offlineCase
        _ -> normalCase

offlineCase :: IO ()
offlineCase = do
    print $ authorizationUrl googleKey `appendQueryParam'` googleScopeEmail `appendQueryParam'` googleAccessOffline
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    accessToken <- requestAccessToken googleKey code
    case accessToken of
        Left e  -> putStr "Authentication failed with " >> print e
        Right t -> do validateToken t >>= print
                      case refreshToken t of
                          Nothing -> putStrLn "Failed to fetch refresh token"
                          Just tk -> do
                              refreshed <- refreshAccessToken googleKey code
                              case refreshed of
                                  Left e  -> putStr "Authentication failed with " >> print e
                                  Right t -> do validateToken t >>= print

normalCase :: IO ()
normalCase = do
    -- Prepare the authorization URL and prompt the user to authenticate
    print $ authorizationUrl googleKey `appendQueryParam'` googleScopeUserInfo
    putStrLn "Visit this URL and paste the code here: "
    code <- fmap BS.pack getLine
    -- Request the actual access token
    accessToken <- requestAccessToken googleKey code
    putStr "AccessToken: " >> print accessToken
    -- Perform some operations with the token
    case accessToken of
        Left e  -> putStr "Authentication failed with " >> print e
        Right t -> do validateToken t >>= print
                      userinfo t >>= print

--------------------------------------------------
-- Google API

-- | This is special for google Gain read-only access to the user's email address.
googleScopeEmail :: QueryParams
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

-- | Gain read-only access to basic profile information, including a
googleScopeUserInfo :: QueryParams
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

-- | Access offline
googleAccessOffline :: QueryParams
googleAccessOffline = [("access_type", "offline")
                      ,("approval_prompt", "force")]

-- | Token Validation
validateToken :: AccessToken -> IO (Maybe BL.ByteString)
validateToken accessToken = do
  req <- parseUrl "https://www.googleapis.com/oauth2/v1/tokeninfo"
  rsp <- getReqAuth accessToken req
  return $ getResponseBody rsp

-- | fetch user email.
--   for more information, please check the playround site.
--
userinfo :: AccessToken -> IO (Maybe BL.ByteString)
userinfo accessToken = do
  req <- parseUrl "https://www.googleapis.com/oauth2/v2/userinfo"
  rsp <- getReqAuth accessToken req
  return $ getResponseBody rsp
