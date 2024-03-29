{ reflex-platform ? import ./reflex-platform
}:
let
  pkgs = (reflex-platform {}).nixpkgs;
  supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;
  commonOverrides = self: super: {
    vty = self.callHackageDirect {
      pkg = "vty";
      ver = "5.38";
      sha256 = "0kcd3ln9xmc62ka0i7habzvjjar8z63mlvl15rdhf8hqmda0b7r7";
    } {};
    reflex-vty = self.callHackageDirect {
      pkg = "reflex-vty";
      ver = "0.4.1.1";
      sha256 = "1dzkfhfwifl47fvvzd40yqvyckpc3q6d9g18az9mqlbxfhszfb45";
    } {};
  };
  ghcs = lib.genAttrs supportedSystems (system: let
    rp = reflex-platform { inherit system; __useNewerCompiler = true; };
    rpGhc810 = rp.ghc.override {
      overrides = commonOverrides;
    };
    rpOld = reflex-platform { inherit system; __useNewerCompiler = false; };
    rpGhc865 = rpOld.ghc.override {
      overrides = commonOverrides;
    };

    nixGhc961 = (import ./nixpkgs { inherit system; }).haskell.packages.ghc961.override {
      overrides = self: super: commonOverrides self super // {
        patch = self.callHackageDirect {
          pkg = "patch";
          ver = "0.0.8.2";
          sha256 = "160zqqhjg48fr3a33gffd82qm3728c8hwf8sn37pbpv82fw71rzg";
        } {};

        reflex = self.callHackageDirect {
          pkg = "reflex";
          ver = "0.9.0.1";
          sha256 = "1yrcashxxclvlvv3cs5gv75rvlsg1gb0m36kssnk2zvhbh94240y";
        } {};
        these-lens = self.callHackageDirect {
          pkg = "these-lens";
          ver = "1.0.1.3";
          sha256 = "0n1vkr57jz5yvy4jm15v5cs42rp342ni0gisib7aqyhibpicqs5c";
        } {};
        these = self.callHackageDirect {
          pkg = "these";
          ver = "1.2";
          sha256 = "1iaaq1fsvg8c3l0czcicshkmbbr00hnwkdamjbkljsa1qvlilaf0";
        } {};
        lens = self.callHackageDirect {
          pkg = "lens";
          ver = "5.2.2";
          sha256 = "0c4a421sxfjm1cj3nvgwkr4glll23mqnsvs2iv5qh85931h2f3cy";
        } {};

        assoc = self.callHackageDirect {
          pkg = "assoc";
          ver = "1.1";
          sha256 = "1krvcafrbj98z5hv55gq4zb1in5yd71nmz9zdiqgnywjzbrvpf75";
        } {};

        strict = self.callHackageDirect {
          pkg = "strict";
          ver = "0.5";
          sha256 = "02iyvrr7nd7fnivz78lzdchy8zw1cghqj1qx2yzbbb9869h1mny7";
        } {};
        vty = self.callHackageDirect {
          pkg = "vty";
          ver = "5.38";
          sha256 = "0kcd3ln9xmc62ka0i7habzvjjar8z63mlvl15rdhf8hqmda0b7r7";
        } {};


        # Jailbroken until https://github.com/audreyt/string-qq/pull/3
        string-qq = haskellLib.dontCheck super.string-qq;
        # Tests aren't compatible with transformers-0.6
        bimap = haskellLib.dontCheck super.bimap;
        exception-transformers = haskellLib.doJailbreak (haskellLib.dontCheck super.exception-transformers);

      };
    };
  in
  {
    recurseForDerivations = true;
    ghc865 = rpGhc865.callCabal2nix "reflex-process" (import ./src.nix) {};
    ghc810 = rpGhc810.callCabal2nix "reflex-process" (import ./src.nix) {};
    ghc961 = nixGhc961.callCabal2nix "reflex-process" (import ./src.nix) {};
  });
  in
    ghcs
