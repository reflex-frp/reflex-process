reflex-process
==============

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex-process.svg)](https://hackage.haskell.org/package/reflex-process) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/reflex-process/badge)](https://matrix.hackage.haskell.org/#/package/reflex-process) [![Travis CI](https://api.travis-ci.org/reflex-frp/reflex-process.svg?branch=develop)](https://travis-ci.org/reflex-frp/reflex-process) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/reflex-frp/reflex-process/blob/master/LICENSE)

Functional-reactive system processes
------------------------------------

Run and interact with system processes from within a [Reflex FRP](https://reflex-frp.org/) application.

Example
-------

The following example uses [reflex-vty](https://github.com/reflex-frp/reflex-vty) to run a terminal application that calls a shell command and displays the result:

```haskell
> {-# LANGUAGE OverloadedStrings #-}
> import Reflex
> import Reflex.Process
> import Reflex.Vty
>
> import Control.Monad ((<=<))
> import Data.Default (def)
> import qualified Data.Set as Set
> import qualified Data.Text.Encoding as T
> import qualified Graphics.Vty.Input as V
> import qualified System.Process as P
>
> cmd :: P.CreateProcess
> cmd = P.proc "ls" ["-a"]
>
> main :: IO ()
> main = mainWidget $ do
>   exit <- keyCombos $ Set.singleton (V.KChar 'c', [V.MCtrl])
>   p <- createProcess cmd def
>   stdout <- foldDyn (flip mappend) "" $ _process_stdout p
>   boxStatic def $ col $ do
>     fixed 3 $ boxStatic def $ text "reflex-process"
>     fixed 3 $ text "Press Ctrl-C to exit."
>     fixed 1 $ text "stdout:"
>     stretch $ text $ T.decodeUtf8 <$> current stdout
>   pure $ () <$ exit
```

Developer environment
---------------------

You can get `ghcid` running for working on the code with the command:

```bash
nix-shell -E '((import ./reflex-platform {}).ghc.callCabal2nix "reflex-process" ./. {}).env' --run ghcid
```

