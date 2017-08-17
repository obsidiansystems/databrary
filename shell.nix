let 
inherit (import ./build.nix {}) pkgs nixpkgs nodePackages; 

#TODO import node commands to nix-shell environment
in

pkgs.databrary-dev.env.overrideAttrs (attrs: {
  buildInputs = attrs.buildInputs or [] ++ 
    [nixpkgs.ffmpeg 
    # nodePackages.tarball
    # nodePackages.package
    # nodePackages.shell
     nixpkgs.nodePackages."coffee-script"
     nixpkgs.nodePackages."uglify-js"
     nixpkgs.nodePackages."stylus"
    ]; 
})
