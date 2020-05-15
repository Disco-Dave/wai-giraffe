{-# LANGUAGE OverloadedStrings #-}

module Wai.Giraffe.Core
( -- * HttpContext
  HttpContext
, getData
, setData
, modifyData
, waiRequest
, setStatus
, addHeader
, addHeaders
, writeResponse

  -- * HttpHandler
, HttpFuncResult
, HttpFunc

  -- * Combinators
, (>=>)
, choose

  -- * Routing
, httpMethod
, get
, post
, Wai.Giraffe.Core.head
, put
, delete
, trace
, connect
, options
, patch
, route
, header
, headerAndValue

 -- * Responses
, text

-- * Application
, toApplication
)
where

import           Control.Applicative            ( empty )
import           Control.Monad                  ( (>=>) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Foldable                  ( asum , foldl')
import           Data.Maybe                     ( fromMaybe)
import           Data.Text                      ( Text )

import qualified Data.Text.Lazy                as LText
import qualified Data.Text.Lazy.Encoding       as ELText
import qualified Network.HTTP.Types            as Http
import qualified Network.Wai                   as Wai



data HttpContext a = HttpContext
  { httpRequest :: !Wai.Request
  , httpPathInfo :: ![Text]
  , httpSend :: !(Wai.Response -> IO Wai.ResponseReceived)
  , httpResponse :: !(Maybe Wai.ResponseReceived)
  , httpHeaders :: ![Http.Header]
  , httpStatus :: !(Maybe Http.Status)
  , httpData :: !a
  }

type HttpFuncResult a = MaybeT IO (HttpContext a)
type HttpFunc a b = HttpContext a -> HttpFuncResult b

instance Functor HttpContext where
  fmap f ctx = ctx { httpData = f (httpData ctx) }

getData :: HttpContext a -> a
getData = httpData

setData :: a -> HttpContext b -> HttpContext a
setData = (<$)

modifyData :: (a -> b) -> HttpContext a -> HttpContext b
modifyData = fmap

waiRequest :: HttpContext a -> Wai.Request
waiRequest = httpRequest

makeHttpContext
  :: a
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> HttpContext a
makeHttpContext initialData request send = HttpContext
  { httpRequest  = request
  , httpPathInfo = Wai.pathInfo request
  , httpSend     = send
  , httpResponse = Nothing
  , httpHeaders  = []
  , httpStatus   = Nothing
  , httpData     = initialData
  }

setStatus :: Http.Status -> HttpContext a -> HttpContext a
setStatus status ctx = ctx { httpStatus = Just status }

addHeader :: Http.Header -> HttpContext a -> HttpContext a
addHeader header ctx = ctx { httpHeaders = header : httpHeaders ctx }

addHeaders :: Foldable t => t Http.Header -> HttpContext a -> HttpContext a
addHeaders headers ctx = ctx { httpHeaders = newHeaders }
  where newHeaders = foldl' (flip (:)) (httpHeaders ctx) headers


writeResponse :: Wai.Response -> HttpFunc a a
writeResponse response ctx = do
  responseRecieved <- liftIO $ httpSend ctx response
  pure $ ctx { httpResponse = Just responseRecieved }

text :: Text -> HttpFunc a a
text textContent ctx =
  let newCtx   = addHeader ("Content-Type", "text/plain; charset=UTF-8") ctx
      headers  = httpHeaders newCtx
      status   = fromMaybe Http.status200 (httpStatus newCtx)
      bytes    = ELText.encodeUtf8 $ LText.fromStrict textContent
      response = Wai.responseLBS status headers bytes
  in  writeResponse response newCtx






choose :: [HttpFunc a b] -> HttpFunc a b
choose funcs ctx = asum $ fmap ($ ctx) funcs




httpMethod :: Http.Method -> HttpFunc a a
httpMethod method ctx | method == getMethod ctx = pure ctx
                      | otherwise               = empty
  where getMethod = Wai.requestMethod . waiRequest


get :: HttpFunc a a
get = httpMethod Http.methodGet

post :: HttpFunc a a
post = httpMethod Http.methodPost

head :: HttpFunc a a
head = httpMethod Http.methodHead

put :: HttpFunc a a
put = httpMethod Http.methodPut

delete :: HttpFunc a a
delete = httpMethod Http.methodDelete

trace :: HttpFunc a a
trace = httpMethod Http.methodTrace

connect :: HttpFunc a a
connect = httpMethod Http.methodConnect

options :: HttpFunc a a
options = httpMethod Http.methodOptions

patch :: HttpFunc a a
patch = httpMethod Http.methodPatch

route :: Text -> HttpFunc a a
route part ctx = case httpPathInfo ctx of
  (p : parts) | p == part -> pure $ ctx { httpPathInfo = parts }
  _                       -> empty

header :: Http.HeaderName -> HttpFunc a a
header headerName ctx =
  let headers = Wai.requestHeaders $ waiRequest ctx
  in  if any ((== headerName) . fst) headers then pure ctx else empty


headerAndValue :: Http.Header -> HttpFunc a a
headerAndValue header ctx =
  let headers = Wai.requestHeaders $ waiRequest ctx
  in  if header `elem` headers then pure ctx else empty

toApplication :: a -> HttpFunc a b -> Wai.Application
toApplication initialData handler request send = do
  let initialCtx = makeHttpContext initialData request send
      result     = handler initialCtx
  response <- (>>= httpResponse) <$> runMaybeT result
  maybe (send $ Wai.responseLBS Http.status404 [] mempty) pure response
