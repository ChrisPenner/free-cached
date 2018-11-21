{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
module ToyMonad where

import           Control.Monad.Free
import           Data.Dynamic

data ToyMonad r =
  ReadFile FilePath (String -> r)
    | Print String r
    deriving Functor

print' :: String -> Free ToyMonad ()
print' txt = liftF $ Print txt ()

readFile' :: FilePath -> Free ToyMonad String
readFile' filepath = liftF $ ReadFile filepath id

prog :: Free ToyMonad String
prog = do
  print' "re-running!"
  f <- readFile' "text-file.txt"
  return (f <> " DONE")

toyMonadInterpIO :: ToyMonad (IO x) -> IO x
toyMonadInterpIO (Print    txt  x) = print txt >> x
toyMonadInterpIO (ReadFile path f) = do
  contents <- readFile path
  f contents


dynCacheFunc :: Typeable a => (a -> b) -> (a -> ([Dynamic], b))
dynCacheFunc f a = ([toDyn a], f a)

dynCacheVal :: a -> ([Dynamic], a)
dynCacheVal a = ([toDyn ()], a)

toyMonadCacher :: ToyMonad a -> ToyMonad ([Dynamic], a)
toyMonadCacher (ReadFile path f) = ReadFile path (dynCacheFunc f)
toyMonadCacher (Print    txt  c) = Print txt (dynCacheVal c)

toyMonadUseCache :: ToyMonad b -> Dynamic -> Maybe b
toyMonadUseCache (ReadFile _ f) d = f <$> fromDynamic d
toyMonadUseCache (Print    _ b) _ = Just b
