{ system ? builtins.currentSystem }:
(import ./release.nix {}).${system}.ghc810.env
