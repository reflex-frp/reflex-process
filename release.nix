{ reflex-platform-fun ? import ./reflex-platform
}:

let
  native-reflex-platform = reflex-platform-fun {};
  inherit (native-reflex-platform.nixpkgs) lib;
  systems = [
    "x86_64-linux"
    "x86_64-darwin"
  ];

  perPlatform = lib.genAttrs systems (system: let
    reflex-platform = reflex-platform-fun { inherit system; };
    src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
      "release.nix"
      ".git"
      "dist"
      "dist-newstyle"
      "cabal.haskell-ci"
      "cabal.project"
      ".travis.yml"
    ])) ./.;
  in reflex-platform.ghc.callCabal2nix "reflex-process" src {});

in perPlatform // {
  cache = native-reflex-platform.pinBuildInputs
    "reflex-platform-everywhere"
    (builtins.attrValues perPlatform);
}
