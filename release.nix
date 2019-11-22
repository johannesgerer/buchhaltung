let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          buchhaltung = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in { buchhaltung = pkgs.haskellPackages.buchhaltung; }
