let 
inherit (import ./build.nix {}) pkgs nixpkgs nodePackages; 

#import node commands to nix-shell environment
inherit (nixpkgs.callPackage ./node-packages.nix {})  tarball package shell;
in

pkgs.databrary-dev.env.overrideAttrs (attrs: {
  buildInputs = attrs.buildInputs or [] ++ [nixpkgs.ffmpeg]; 
})
