{ pkgs }:
{
  allowBroken = true;
  allowUnfree = true;

  haskellPackageOverrides = with pkgs.haskell.lib; self: super: {
    #base-orphans = dontCheck super.base-orphans;
    #concurrent-output = doJailbreak super.concurrent-output;
    #directory = doJailbreak super.directory;
    #liquid-fixpoint = overrideCabal super.liquid-fixpoint (args: {
    #  src = /home/ptival/liquid-fixpoint;
    #});
  };

  packageOverrides = pkgs: {

#    emacs = pkgs.lib.overrideDerivation (pkgs.emacs.override {
#      withGTK2 = true;
#      withGTK3 = false;
#    });

    mystuff = with pkgs; buildEnv {
      name = "mystuff";
      paths = [
        #atom
        #coq_8_5
        nodejs
        #opam
        vscode
      ];
    };

    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc7103 = pkgs.haskell.packages.ghc7103.override {
          overrides = self: super: {
            # This is a workaround the fact that generic-deriving-1.10.6 had a
            # single quote in a filename and would not build as a result.
            # This is fixed in generic-deriving-1.10.7
            generic-deriving = pkgs.haskell.lib.overrideCabal super.generic-deriving (args: {
              src = /home/ptival/PeaCoq/snap-framework/generic-deriving;
            });
            # This is a workaround the fact that Glob has its semigroups
            # dependency under a test for ghc >= 8, but ghc 7.10.3 somehow
            # complains about it missing
            Glob = pkgs.haskell.lib.overrideCabal super.Glob (args: {
              buildDepends = (args.buildDepends or []) ++ [self.semigroups];
            });
          };
        };
      };
    };
  };

}

