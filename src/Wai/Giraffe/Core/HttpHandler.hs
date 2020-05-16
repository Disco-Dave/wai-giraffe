{-# LANGUAGE RankNTypes #-}

module Wai.Giraffe.Core.HttpHandler
  ( HttpResult
  , HttpHandler
  , (>=>)
  , compose
  , choose
  , toApplicationHoisted
  , toApplication
  )
where

import Wai.Giraffe.Core.GiraffeT
import Wai.Giraffe.Core.HttpContext (HttpContext)
import qualified Wai.Giraffe.Core.HttpContext as HttpContext

import Control.Monad ( (>=>) )
import Data.Foldable ( asum )

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Http


type HttpResult m a = GiraffeT m (HttpContext a)
type HttpHandler m a b = HttpContext a -> HttpResult m b

compose :: Monad m => HttpHandler m a b -> HttpHandler m b c -> HttpHandler m a c
compose = (>=>)

choose :: Monad m => [HttpHandler m a b] -> HttpHandler m a b
choose handlers context = asum $ fmap ($ context) handlers

toApplicationHoisted :: (forall r. m r -> IO r) -> a -> HttpHandler m a b -> Wai.Application
toApplicationHoisted nt initialData handler request send = do
  let initialCtx = HttpContext.make request send initialData 
      result     = nt . runGiraffeT $ handler initialCtx
  response <- (>>= HttpContext.getResponse) <$> result
  maybe (send $ Wai.responseLBS Http.status404 [] mempty) pure response

toApplication :: a -> HttpHandler IO a b -> Wai.Application
toApplication = toApplicationHoisted id
