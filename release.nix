{ pkgs ? import ./nixpkgs {}
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
}:
let
  inherit (pkgs) lib;
  sharedOverrides = self: super: {
    reflex-vty = self.callHackageDirect {
      pkg = "reflex-vty";
      ver = "0.2.0.0";
      sha256 = "1vb38qx1a6l28i4wd1g48qqmymlzivq7lwmxbywjs0b36ynsnfk8";
    } {};
    reflex = self.callHackageDirect {
      pkg = "reflex";
      ver = "0.8.1.1";
      sha256 = "1sdakz8rgdhvrcq004926dmbwlmhmv7wsw9h7f8klnvdsydm7dh7";
    } {};
    patch = self.callHackageDirect {
      pkg = "patch";
      ver = "0.0.4.0";
      sha256 = "1x1rbi51r5gvbkg96884c2py7in4n0ijh5ins8ya3b5kga32siq4";
    } {};
  };

  ghcs = {
    ghc865 = pkgs.haskell.packages.ghc865.override {
      overrides = self: super: sharedOverrides self super // {
        ghc-lib-parser = self.callHackageDirect {
          pkg = "ghc-lib-parser";
          ver = "8.8.4.20210620";
          sha256 = "17y7f5h1mrfyblaz0ipws04a4z11vnwkfvzzk8mkyrkz4am1a8fp";
        } {};
        ghc-lib-parser-ex = self.callHackageDirect {
          pkg = "ghc-lib-parser-ex";
          ver = "8.8.5.8";
          sha256 = "1avdm9fzgk59xzq5xv5rlnncq4vgqsf3jyf46889cf7gcfb40aff";
        } {};
        hlint = self.callHackageDirect {
          pkg = "hlint";
          ver = "2.2.11";
          sha256 = "0v4axmqb3zwzznyvhiqbr50k23ah63sd9gsmi5sa2p97hch8kwx1";
        } {};

      };
    };
    ghc884 = pkgs.haskell.packages.ghc884.override {
      overrides = sharedOverrides;
    };
    ghc8104 = pkgs.haskell.packages.ghc8104.override {
      overrides = sharedOverrides;
    };
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "reflex-process" ./. {}) ghcs

