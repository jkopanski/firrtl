let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          firrtl = pkgs.haskell.lib.dontHaddock
            (
              haskellPackagesNew.callPackage ./default.nix { }
            );
          
          # no need with unstable channel
          # singletons = haskellPackagesNew.singletons_2_4_1;
        };
      };
    }; 
  };

  pkgs = import <unstable> { inherit config; };
  
in
  { firrtl = pkgs.haskellPackages.firrtl;
  }
