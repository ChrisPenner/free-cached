{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Control.Monad.Free.Cached where

import ToyMonad
import Control.Monad.Free
import qualified Control.Monad.Trans.Free      as TF
import Data.Dynamic
import Data.Functor.Foldable
import Control.Monad
import Control.Applicative
import Data.Bifunctor

iterAndCache
  :: forall r m f a
   . (Monoid r, Monad m, Functor f)
  => (forall x . f x -> f (r, x))
  -> (forall x . f (m x) -> m x)
  -> Free f a
  -> m (r, a)
iterAndCache pairer interp fr = iterM go ((mempty, ) <$> fr)
 where
  collapse :: Semigroup r => (r, m (r, a)) -> m (r, a)
  collapse (r, m) = first (r <>) <$> m
  go :: f (m (r, a)) -> m (r, a)
  go fmra =
    let paired :: f (m (r, a))
        paired = collapse <$> pairer fmra
    in  interp paired

iterFromCache
  :: forall f a r m
   . (Functor f, MonadPlus m)
  => (forall b . f b -> (r -> m b))
  -> [r]
  -> Free f a
  -> m a
iterFromCache interp cache fr = cata alg fr cache
 where
  alg :: TF.FreeF f b ([r] -> m b) -> ([r] -> m b)
  alg (TF.Pure b  ) []       = pure b
  -- If we still have cache values left something went funny, fail
  alg (TF.Pure _  ) _        = empty
  alg (TF.Free frr) (r : rs) = do
    f <- interp frr r
    f rs
  alg (TF.Free _) [] = empty

iterCachedOrRefresh
  :: forall f m a r
   . (Monad m, Functor f)
  => (forall x . f x -> (r -> Maybe x))
  -> (f (m a) -> m a)
  -> [r]
  -> Free f a
  -> m a
iterCachedOrRefresh cacheInterp interp cache fr =
  maybe (iterM interp fr) pure (iterFromCache cacheInterp cache fr)

runAndCacheToyMonad :: IO ([Dynamic], String)
runAndCacheToyMonad = iterAndCache toyMonadCacher toyMonadInterpIO prog

runFromCacheToyMonad :: [Dynamic] -> Free ToyMonad a -> Maybe a
runFromCacheToyMonad = iterFromCache toyMonadUseCache

runCachedOrInterpToyMonad :: [Dynamic] -> Free ToyMonad a -> IO a
runCachedOrInterpToyMonad cache =
  iterCachedOrRefresh toyMonadUseCache toyMonadInterpIO cache
