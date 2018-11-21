{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Lib where

import           Control.Monad.Free
import           Data.Dynamic

data MyActions r =
  ReadFile FilePath (String -> r)
    | Print String r
    deriving Functor

print' :: String -> Free MyActions ()
print' txt = liftF $ Print txt ()

readFile' :: FilePath -> Free MyActions String
readFile' filepath = liftF $ ReadFile filepath id

prog :: Free MyActions String
prog = do
  print' "re-running!"
  f <- readFile' "text-file.txt"
  return (f <> " DONE")

-- myActionCacher :: MyActions x -> IO (Dynamic, x)
-- myActionCacher (ReadFile path f) = do
--   contents <- readFile path
--   return (toDyn contents, f contents)
-- myActionCacher (WriteFile _ _ x) = pure (toDyn (), x)

myActionInterpIO :: MyActions (IO x) -> IO x
myActionInterpIO (Print    txt  x) = print txt >> x
myActionInterpIO (ReadFile path f) = do
  contents <- readFile path
  f contents


dynCacheFunc :: Typeable a => (a -> b) -> (a -> ([Dynamic], b))
dynCacheFunc f a = ([toDyn a], f a)

dynCacheVal :: a -> ([Dynamic], a)
dynCacheVal a = ([toDyn ()], a)

myActionCacher :: MyActions a -> MyActions ([Dynamic], a)
myActionCacher (ReadFile path f) = ReadFile path (dynCacheFunc f)
myActionCacher (Print    txt  c) = Print txt (dynCacheVal c)

myActionUseCache :: MyActions b -> Dynamic -> Maybe b
myActionUseCache (ReadFile _ f) d = f <$> fromDynamic d
myActionUseCache (Print    _ b) _ = Just b
