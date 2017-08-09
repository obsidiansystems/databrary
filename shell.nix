let 
inherit (import ./build.nix {}) pkgs nixpkgs; 
in

pkgs.databrary.env.overrideAttrs (attrs: {
  buildInputs = attrs.buildInputs or [] ++ [nixpkgs.ffmpeg]; 
})
