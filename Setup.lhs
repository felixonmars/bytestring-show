#!/usr/bin/env runhaskell
> import Distribution.Simple
> import System.Cmd (system)

> main = defaultMainWithHooks (defaultUserHooks { runTests = tests })

> tests _ _ _ _ = system "runhaskell Tests/Properties.hs" >> return ()
