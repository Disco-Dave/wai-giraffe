module Wai.Giraffe.Core.GiraffeT
  ( GiraffeT(..)
  , runGiraffeT
  )
where

import Control.Applicative ( Alternative(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe ( MaybeT(..) )

newtype GiraffeT m a =
  GiraffeT { unwrapGiraffeT :: MaybeT m a }

runGiraffeT :: GiraffeT m a -> m (Maybe a)
runGiraffeT = runMaybeT . unwrapGiraffeT

instance Functor m => Functor (GiraffeT m) where
  fmap f (GiraffeT m) = GiraffeT $ fmap f m

instance Monad m => Applicative (GiraffeT m) where
  pure v = GiraffeT $ pure v
  (GiraffeT gf) <*> (GiraffeT gm) = GiraffeT $ gf <*> gm

instance Monad m => Monad (GiraffeT m) where
  (GiraffeT gm) >>= f = GiraffeT $ gm >>= (unwrapGiraffeT . f)

instance MonadTrans GiraffeT where
  lift = GiraffeT . MaybeT . fmap Just

instance MonadIO m => MonadIO (GiraffeT m) where
  liftIO = lift . liftIO

instance Monad m => Alternative (GiraffeT m) where
  empty = GiraffeT empty
  (GiraffeT x) <|> (GiraffeT y) = GiraffeT $ x <|> y
