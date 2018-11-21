{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Simple where

import Control.Monad.Free
import qualified Control.Monad.Trans.Free      as TF
import Data.Functor.Foldable

runWithCache
  :: forall f a r
   . Functor f
  => (forall b . f b -> (r -> Maybe b))
  -> [r]
  -> Free f a
  -> Maybe a
runWithCache alg cache fr = cata alg' fr cache
 where
  alg' :: TF.FreeF f b ([r] -> Maybe b) -> ([r] -> Maybe b)
  alg' (TF.Pure b  ) _        = Just b
  alg' (TF.Free frr) (r : rs) = do
    f <- alg frr r
    f rs
  alg' (TF.Free _) [] = Nothing

buildCache
  :: forall m r b f
   . (Monad m, Functor f)
  => (forall a . f a -> m (r, a))
  -> Free f b
  -> m [r]
buildCache alg = cata alg'
 where
  alg' :: TF.FreeF f x (m [r]) -> m [r]
  alg' (TF.Pure _  ) = pure []
  alg' (TF.Free frb) = do
    (r, mr) <- alg frb
    rs      <- mr
    return (r : rs)

-- progCache :: IO [Dynamic]
-- progCache = buildCache myActionCacher prog
