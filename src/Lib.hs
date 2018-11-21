{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Data.Aeson
import Control.Monad.Free
import qualified Control.Monad.Trans.Free as TF
import Data.Functor.Foldable
import Data.Serialize
import Data.Dynamic
import Control.Comonad.Cofree

data MyActions r =
  ReadFile FilePath (String -> r)
    | WriteFile FilePath String r
    deriving Functor

writeFile' :: String -> String -> Free MyActions ()
writeFile' filepath contents = liftF $ WriteFile filepath contents ()

readFile' :: FilePath -> Free MyActions String
readFile' filepath = liftF $ ReadFile filepath id

prog :: Free MyActions String
prog = do
  f <- readFile' "text-file.txt"
  writeFile' "new-file.txt" (f <> "stuff")
  return (f <> " end!")

myActionCacher :: MyActions x -> IO (Dynamic, x)
myActionCacher (ReadFile path f) = do
  contents <- readFile path
  return (toDyn contents, f contents)
myActionCacher (WriteFile _ _ x) = pure (toDyn (), x)

-- tag :: Free MyActions x -> Cofree (TF.FreeF MyActions x) Dynamic
-- tag = 

class PatternFunctor f where
  data IndF
  indOf :: forall a. f a -> IndF

-- thingDyn :: Free MyActions () -> Free MyActions Dynamic
-- thingDyn (ReadFile path f) = 

-- cachedInterpreter
--   :: (Monad m, Functor f)
--   => (f (m a) -> m (Dynamic, a))
--   -> Free f a
--   -> m ([Dynamic], a)

progCache :: IO [Dynamic]
progCache = buildCache myActionCacher prog

myActionUseCache :: MyActions b -> Dynamic -> Maybe b
myActionUseCache (ReadFile filepath f) d = f <$> fromDynamic d
myActionUseCache (WriteFile _ _ b    ) _ = Just b

magicReducer :: (forall a . Typeable a => (a -> b) -> b) -> MyActions b -> b
magicReducer magic (ReadFile filepath f) = magic f
magicReducer _     (WriteFile _ _ b    ) = b

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
  alg' (TF.Pure b  ) rs       = Just b
  alg' (TF.Free frr) (r : rs) = do
    f <- alg frr r
    f rs
  alg' (TF.Free frr) [] = Nothing

buildCache
  :: forall m r b f
   . (Monad m, Functor f)
  => (forall a . f a -> m (r, a))
  -> Free f b
  -> m [r]
buildCache alg = cata alg'
 where
  alg' :: TF.FreeF f x (m [r]) -> m [r]
  alg' (TF.Pure b  ) = pure []
  alg' (TF.Free frb) = do
    (r, mr) <- alg frb
    rs      <- mr
    return (r : rs)
