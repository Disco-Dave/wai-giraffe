module Wai.Giraffe.Core.HttpContext
  ( WaiSend
  , WaiInfo
  , PathPart
  , HandlerInfo
  , HttpContext
  , make
  , getRequest
  , getResponse
  , sendResponse
  , modifyHeaders
  , addHeader
  , addHeaders
  , consumePathInfo
  )
where

import Control.Arrow ( (>>>) )
import Data.Function ( (&) )
import Data.Functor ( (<&>) )
import Data.Text ( Text )

import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai


-- | A `Wai.Application` uses a continuation-passing style to send
-- a response. This type represents the function they use to send
-- the response.
type WaiSend = Wai.Response -> IO Wai.ResponseReceived

-- | Represents a single part of a URI path. 
-- For example say we have an url http://localhost.com/a/b?q=1/c/d
-- then the path parts would be a, b, c, and d.
type PathPart = Text

-- | Contains all of the Wai information that we will need.
data WaiInfo = WaiInfo
  { waiRequest :: !Wai.Request
  , waiSend :: !WaiSend
  , waiResponse :: !(Maybe Wai.ResponseReceived)
  }

-- | Contains information from the handlers.
data HandlerInfo = HandlerInfo
  { handlerRemainingPathInfo :: ![PathPart]
  , handlerResponseHeaders :: ![Http.Header]
  }

-- | Contains the context of a http request and response chain.
data HttpContext a = HttpContext
  { httpWaiInfo :: !WaiInfo
  , httpHandlerInfo :: !HandlerInfo
  , httpData :: !a
  }

instance Functor HttpContext where
  fmap f context = 
    context { httpData = f (context & httpData) }

make :: Wai.Request -> WaiSend -> a -> HttpContext a
make request send initialData =
  let 
    waiInfo = 
        WaiInfo
          { waiRequest = request
          , waiSend = send
          , waiResponse = Nothing
          }
    handlerInfo =
      HandlerInfo
        { handlerRemainingPathInfo = Wai.pathInfo request
        , handlerResponseHeaders = []
        }
    in
      HttpContext
        { httpWaiInfo = waiInfo
        , httpHandlerInfo = handlerInfo
        , httpData = initialData
        }

getRequest :: HttpContext a -> Wai.Request
getRequest = httpWaiInfo >>> waiRequest

getResponse :: HttpContext a -> Maybe Wai.ResponseReceived
getResponse = httpWaiInfo >>> waiResponse

sendResponse :: Wai.Response -> HttpContext a -> IO (HttpContext a)
sendResponse response context =
  case context & httpWaiInfo & waiResponse of
    Just _ ->
      -- A response was already sent and we don't want to
      -- send another. So just return the context unmodified.
      pure context
    Nothing ->
      let
        send = context & httpWaiInfo & waiSend
        updateContext r = 
          let newWaiInfo = (context & httpWaiInfo) { waiResponse = Just r }
          in  context { httpWaiInfo = newWaiInfo}
      in
        send response <&> updateContext
        
modifyHeaders :: ([Http.Header] -> [Http.Header]) -> HttpContext a -> HttpContext a
modifyHeaders f context = 
  let 
    oldHeaders = 
      context & httpHandlerInfo & handlerResponseHeaders
    newHandlerInfo = 
      (context & httpHandlerInfo) { handlerResponseHeaders = f oldHeaders }
  in
    context { httpHandlerInfo = newHandlerInfo }

addHeaders :: [Http.Header] -> HttpContext a -> HttpContext a
addHeaders headers = modifyHeaders (<> headers)

addHeader :: Http.Header -> HttpContext a -> HttpContext a
addHeader header = addHeaders [header]

consumePathInfo :: HttpContext a -> (Maybe PathPart, HttpContext a)
consumePathInfo context =
  case context & httpHandlerInfo & handlerRemainingPathInfo of
    [] ->
      (Nothing, context)
    (part : remainingParts) ->
      let
        newHandlerInfo = 
          (context & httpHandlerInfo) { handlerRemainingPathInfo = remainingParts }
      in 
        (Just part, context { httpHandlerInfo = newHandlerInfo }) 
