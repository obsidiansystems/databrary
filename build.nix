{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.haskellPackages.callPackage ./. {
  inherit (nixpkgs) ffmpeg;
}
