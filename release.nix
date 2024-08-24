{ compiler ? "ghc96" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        # use separate name here to avoid cyclic dependencies
        overriddenHaskellPackages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: rec {
              reflex-test-host = self.callHackageDirect {
                  pkg = "reflex-test-host";
                  ver = "0.1.2.3";
                  sha256 = "9ee3ad9ac4fc58c6dcabcd9fbc6d109a51d8e86ba2682cee8367bc6b452f09ea";
                } {};
              # I guess callHackageDirect depends on exceptions? So we don't use self here 
              exceptions = pkgs.haskell.packages."${compiler}".callHackageDirect {
                  pkg = "exceptions";
                  ver = "0.10.7";
                  sha256 = "7263226a83261d80377df7d0d1b34b3fcb58c7e1c2aff1a762b93cb1f48a02fd";
                } {};
            };
          };
        };
      };
    };
  };

  nixpkgs = import <nixpkgs> { inherit config; }; 
in
  { 
    tinytools = nixpkgs.haskell.overriddenHaskellPackages."${compiler}".callCabal2nix "tinytools" (import ./src.nix) { icu-i18n = nixpkgs.icu; icu-uc = nixpkgs.icu; };
  }

