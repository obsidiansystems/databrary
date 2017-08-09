{ reflex-platform ? import ./reflex-platform {} }:

let 
	nixpkgs = reflex-platform.nixpkgs;
  dontCheck = nixpkgs.haskell.lib.dontCheck;

  # ghc 8 fails with template haskell errors
  #pkgs = reflex-platform.ghc.override {

  # reflex-platform.ghc7 fails with infinite recursion
  # 
	pkgs =	nixpkgs.pkgs.haskell.packages.ghc7103.override {
    overrides = self: super: {
      databrary = self.callPackage ./. {inherit (nixpkgs) ffmpeg;};
      # nix version is 0.5.1
    	# postgresql-typed = dontCheck super.postgresql-typed; 
      # databrary is using 0.4.5
    	postgresql-typed = dontCheck (self.callHackage  "postgresql-typed"
      "0.4.5" {});
			
			#partial-isomorphisms is for GHC7 only!
			partial-isomorphisms= dontCheck (self.callHackage  "partial-isomorphisms"
      "0.2" {});

			invertible = dontCheck super.invertible;
    };
  };
in { inherit nixpkgs pkgs; } 
