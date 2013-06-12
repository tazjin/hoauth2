{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | A simple http client to request OAuth2 tokens and several utils.

module Network.OAuth.OAuth2.HttpClient where

import           Control.Exception             (catch)
import           Control.Monad.Trans.Resource  (ResourceT)
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           Network.OAuth.OAuth2.Internal

--------------------------------------------------
-- * Retrieve access token
--------------------------------------------------

-- | Requests an access token according to the provider details specified
--   in the supplied 'OAuth2'
requestAccessToken :: OAuth2          -- ^ OAuth context
                   -> BS.ByteString   -- ^ Authentication code gained after authorization
                   -> IO (OAuth2Result AccessToken) -- ^ Access Token
requestAccessToken oa code = doJSONPostRequest (accessTokenUrl oa code)


-- | Requests a refresh token
refreshAccessToken :: OAuth2          -- ^ OAuth context
                   -> BS.ByteString   -- ^ refresh token gained after authorization
                   -> IO (OAuth2Result AccessToken)
refreshAccessToken oa rtoken = doJSONPostRequest (refreshAccessTokenUrl oa rtoken)

--------------------------------------------------
-- * Utilities for response parsing
--------------------------------------------------

handleResponse :: Response BSL.ByteString -> OAuth2Result BSL.ByteString
handleResponse rsp =
    if (statusCode $ responseStatus rsp) == 200
        then Right $ responseBody rsp
        else Left $ BSL.append "Gaining token failed: " (responseBody rsp)

-- |Parses a @OAuth2Result BSL.ByteString@ into @FromJSON a => a@
parseResponse :: FromJSON a
              => OAuth2Result BSL.ByteString
              -> OAuth2Result a
parseResponse rsp =
    either (Left) -- Return Left if error
           (maybe (Left "Could not decode JSON") (Right) . decode) -- Decode JSON
           rsp

-- |Adds request headers for UserAgent \"hoauth2\" and Accept \"application/json\"
setUseragentAccept :: Request m -> Request m
setUseragentAccept req = req { requestHeaders = [ (hUserAgent, "hoauth2")
                                                , (hAccept, "application/json") ]
                             }

--------------------------------------------------
-- * Simple authenticated requests
--------------------------------------------------

-- |Creates the Header to be sent along with authenticated requests.
oauth2BearerHeader :: AccessToken -- ^ Token to embed in the header
                   -> Request m   -- ^ Request to add the header to
                   -> Request m   -- ^ Final request
oauth2BearerHeader (AccessToken t _) r =
    r { requestHeaders = (hAuthorization, BS.append "Bearer " t) : requestHeaders r}

-- |Sets the HTTP method to use
setMethod :: StdMethod -> Request m -> Request m
setMethod m req = req { method = renderStdMethod m }

-- |Sends a HTTP request including the Authorization header with the specified
--  access token.
authenticatedRequest :: AccessToken             -- ^ Authentication token to use
                     -> StdMethod               -- ^ Method to use
                     -> Request (ResourceT IO)  -- ^ Request to perform
                     -> IO (OAuth2Result (Response BSL.ByteString))
authenticatedRequest token m r =
    catch (fmap Right $ withManager $ httpLbs request)                  -- If Response goes through, return it
          (\e -> return $ Left $ BSL.pack $ show (e :: HttpException))  -- If we get an Exception, show it and return
  where
    request = oauth2BearerHeader token $ setMethod m r { checkStatus = \_ _ _ -> Nothing }

-- |If you're only interested in the response body you can use this to extract
--  it from the result of authenticatedRequest
getResponseBody :: OAuth2Result (Response BSL.ByteString) -> Maybe BSL.ByteString
getResponseBody (Left _)  = Nothing
getResponseBody (Right r) = Just $ responseBody r

-- |This is 'authenticatedRequest' specialized to GET
getReqAuth :: AccessToken -> Request (ResourceT IO) -> IO (OAuth2Result (Response BSL.ByteString))
getReqAuth t = authenticatedRequest t GET

-- |This is 'authenticatedRequest' specialized to POST
postReqAuth :: AccessToken -> Request (ResourceT IO) -> IO (OAuth2Result (Response BSL.ByteString))
postReqAuth t = authenticatedRequest t POST

-- |This is 'authenticatedRequest' specialized to POST
deleteReqAuth :: AccessToken -> Request (ResourceT IO) -> IO (OAuth2Result (Response BSL.ByteString))
deleteReqAuth t = authenticatedRequest t DELETE

-- |This is 'authenticatedRequest' specialized to POST
putReqAuth :: AccessToken -> Request (ResourceT IO) -> IO (OAuth2Result (Response BSL.ByteString))
putReqAuth t = authenticatedRequest t PUT

--------------------------------------------------
-- * Simple HTTP requests
--------------------------------------------------

-- | Conduct post request and return response as JSON.
doJSONPostRequest :: FromJSON a
                  => (URI, PostBody)      -- ^ The URI and request body for fetching token.
                  -> IO (OAuth2Result a)  -- ^ Response as ByteString
doJSONPostRequest (uri, body) = do
    rsp <- fmap handleResponse $ doPostRequestWithReq (BS.unpack uri) body
    case rsp of
      Left err -> return $ Left err
      Right a -> case decode a of
                    Nothing -> return $ Left "JSON: Decoding error"
                    Just r  -> return $ Right r

doPostRequestWithReq ::  String                               -- ^ URL
                    -> [(BS.ByteString, BS.ByteString)]  -- ^ Data to Post Body
                    -> IO (Response BSL.ByteString)      -- ^ Response
doPostRequestWithReq url body = do
    req <- parseUrl url
    let req' = setUseragentAccept req
    withManager $ httpLbs (urlEncodedBody body req')
