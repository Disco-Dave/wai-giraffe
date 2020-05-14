{-# LANGUAGE OverloadedStrings #-}

module Wai.Giraffe.Core
  ( -- * HttpContext
    HttpContext
  , waiRequest
  , setStatus
  , addHeader
  , addHeaders
  , writeResponse
  , writeText

    -- * HttpHandler
  , HttpFuncResult
  , HttpFunc
  , HttpHandler

    -- * Combinators
  , compose
  , (>==>)
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
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Foldable                  ( asum , foldl')
import           Data.Maybe                     ( fromMaybe)
import           Data.Text                      ( Text )

import qualified Data.Text.Lazy                as LText
import qualified Data.Text.Lazy.Encoding       as ELText
import qualified Network.HTTP.Types            as Http
import qualified Network.Wai                   as Wai



data HttpContext = HttpContext
  { httpRequest :: !Wai.Request
  , httpPathInfo :: ![Text]
  , httpSend :: !(Wai.Response -> IO Wai.ResponseReceived)
  , httpResponse :: !(Maybe Wai.ResponseReceived)
  , httpHeaders :: ![Http.Header]
  , httpStatus :: !(Maybe Http.Status)
  }

waiRequest :: HttpContext -> Wai.Request
waiRequest = httpRequest

makeHttpContext
  :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> HttpContext
makeHttpContext request send = HttpContext
  { httpRequest  = request
  , httpPathInfo = Wai.pathInfo request
  , httpSend     = send
  , httpResponse = Nothing
  , httpHeaders  = []
  , httpStatus   = Nothing
  }

setStatus :: Http.Status -> HttpContext -> HttpContext
setStatus status ctx = ctx { httpStatus = Just status }

addHeader :: Http.Header -> HttpContext -> HttpContext
addHeader header ctx = ctx { httpHeaders = header : httpHeaders ctx }

addHeaders :: Foldable t => t Http.Header -> HttpContext -> HttpContext
addHeaders headers ctx = ctx { httpHeaders = newHeaders }
  where newHeaders = foldl' (flip (:)) (httpHeaders ctx) headers

writeResponse :: MonadIO m => Wai.Response -> HttpContext -> m HttpContext
writeResponse response ctx = do
  responseRecieved <- liftIO $ httpSend ctx response
  pure $ ctx { httpResponse = Just responseRecieved }

writeText :: MonadIO m => Text -> HttpContext -> m HttpContext
writeText textContent ctx =
  let newCtx   = addHeader ("Content-Type", "text/plain; charset=UTF-8") ctx
      headers  = httpHeaders newCtx
      status   = fromMaybe Http.status200 (httpStatus newCtx)
      bytes    = ELText.encodeUtf8 $ LText.fromStrict textContent
      response = Wai.responseLBS status headers bytes
  in  writeResponse response newCtx



type HttpFuncResult = MaybeT IO HttpContext
type HttpFunc = HttpContext -> HttpFuncResult
type HttpHandler = HttpFunc -> HttpFunc



compose :: HttpHandler -> HttpHandler -> HttpHandler
compose handler1 handler2 final =
  let func = handler1 $ handler2 final
  in  \ctx -> case httpResponse ctx of
        Just _  -> final ctx
        Nothing -> func ctx

(>==>) :: HttpHandler -> HttpHandler -> HttpHandler
(>==>) = compose
infixr 1 >==>

chooseHttpFunc :: [HttpFunc] -> HttpFunc
chooseHttpFunc funcs ctx = asum $ fmap ($ ctx) funcs

choose :: [HttpHandler] -> HttpHandler
choose handlers next = chooseHttpFunc (fmap ($ next) handlers)



httpMethod :: Http.Method -> HttpHandler
httpMethod method next ctx | method == getMethod ctx = next ctx
                           | otherwise               = empty
  where getMethod = Wai.requestMethod . waiRequest

get :: HttpHandler
get = httpMethod Http.methodGet

post :: HttpHandler
post = httpMethod Http.methodPost

head :: HttpHandler
head = httpMethod Http.methodHead

put :: HttpHandler
put = httpMethod Http.methodPut

delete :: HttpHandler
delete = httpMethod Http.methodDelete

trace :: HttpHandler
trace = httpMethod Http.methodTrace

connect :: HttpHandler
connect = httpMethod Http.methodConnect

options :: HttpHandler
options = httpMethod Http.methodOptions

patch :: HttpHandler
patch = httpMethod Http.methodPatch

route :: Text -> HttpHandler
route part next ctx = case httpPathInfo ctx of
  (p : parts) | p == part -> next (ctx { httpPathInfo = parts })
  _                       -> empty

header :: Http.HeaderName -> HttpHandler
header headerName next ctx =
  let headers = Wai.requestHeaders $ waiRequest ctx
  in  if any ((== headerName) . fst) headers then next ctx else empty

headerAndValue :: Http.Header -> HttpHandler
headerAndValue header next ctx =
  let headers = Wai.requestHeaders $ waiRequest ctx
  in  if header `elem` headers then next ctx else empty



text :: Text -> HttpHandler
text textContent next ctx = do
  newCtx <- writeText textContent ctx
  next newCtx



toApplication :: HttpHandler -> Wai.Application
toApplication handler request send = do
  let initialCtx = makeHttpContext request send
      result     = handler pure initialCtx
  response <- (>>= httpResponse) <$> runMaybeT result
  maybe (send $ Wai.responseLBS Http.status404 [] mempty) pure response
