reflex-process
==============

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex-process.svg)](https://hackage.haskell.org/package/reflex-process) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/reflex-frp/reflex-process/blob/master/LICENSE)

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
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as T
> import qualified Graphics.Vty.Input as V
> import qualified System.Process as P
>
> cmd :: P.CreateProcess
> cmd = P.proc "ls" ["-a"]
>
> main :: IO ()
> main = mainWidget $ initManager_ $ col $ do
>   exit <- keyCombos $ Set.singleton (V.KChar 'c', [V.MCtrl])
>   p <- createProcess cmd def
>   stdout <- foldDyn (flip mappend) "" $ _process_stdout p
>   grout flex $ boxStatic def $ col $ do
>     grout (fixed 3) $ boxStatic def $ text "reflex-process"
>     grout (fixed 3) $ text "Press Ctrl-C to exit."
>     grout (fixed 2) $ text $ pure $ "Running command: " <> case P.cmdspec cmd of
>       P.ShellCommand s -> T.pack s
>       P.RawCommand p args -> T.intercalate " " $ T.pack <$> (p : args)
>     grout (fixed 1) $ text "stdout:"
>     grout flex $ text $ T.decodeUtf8 <$> current stdout
>   pure $ () <$ exit
```

Developer environment
---------------------

You can enter a development shell by running `nix-shell`.

From the shell, you can run `cabal` or `ghc` commands or `ghcid`.
