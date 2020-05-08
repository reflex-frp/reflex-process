{ reflex-platform-fun ? import ./reflex-platform
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
}:

let
  native-reflex-platform = reflex-platform-fun {};
  inherit (native-reflex-platform.nixpkgs) lib;

  perPlatform = lib.genAttrs supportedSystems (system: let
    reflex-platform = reflex-platform-fun {
      inherit system;
      haskellOverlaysPost = [
        (self: super: {
          reflex = self.callHackageDirect {
            pkg = "reflex";
            ver = "0.7.1.0";
            sha256 = "0a933xz7yl931m90bbwi9akfz77q6px36grlx6wba55mn1klpn27";
          } {};
        })
      ];
    };
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
    "reflex-process-everywhere"
    (builtins.attrValues perPlatform);
}
