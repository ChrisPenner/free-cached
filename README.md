# free-cached

**WIP**

Given that you've written methods which can cache the result of interpreting
your free monad (helpers are provided), then you can run a free monad once and
cache the result until the free monad changes. This is useful for tests which
have effects and are slow to run.

Totally still a work in progress, see `src/Control.Monad.Free.Cached.hs` to
take a peek a the incomprehensible code behind this.

Note, this is still really broken, don't use it.

```haskell
> (cache, result) <- runAndCacheToyMonad myProgram
"The program ran so I'm printing an effect"
> result
"The result"
> runFromCacheToyMonad cache myProgram
Just "The result"
> runCachedOrInterpToyMonad cache myProgram 
"The result"
> runCachedOrInterpToyMonad [] myProgram 
"The program ran so I'm printing an effect"
"The result"
```
