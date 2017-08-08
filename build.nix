{ reflex-platform ? import ./reflex-platform {} }:

let 
	nixpkgs = reflex-platform.nixpkgs;
  dontCheck = nixpkgs.haskell.lib.dontCheck;
  pkgs = reflex-platform.ghc.override {
    overrides = self: super: {
      databrary = self.callPackage ./. {inherit (nixpkgs) ffmpeg;};
    	postgresql-typed = dontCheck super.postgresql-typed; 
			invertible = dontCheck super.invertible;
    };
  };
in pkgs.databrary
