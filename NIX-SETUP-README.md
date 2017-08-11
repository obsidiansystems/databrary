DISCLAIMER: This is a proto-type document for development. Do not use. 

This will guide a NIX-HASKELL DEVELOPER through the setup got Databrary so
far, please update this document with fixes as they are deployed. 

STEP 1: 
  From the root directory of this project, run $ ./bar 
  This is a script to set up the postgresql server along with required schemas

STEP 2:
  From the same root directory of this project, run $ nix-shell
  When/If that succeeds, run $ cabal repl databrary

STEP 3: 
  When cabal successfully builds all 303 dependencies, run $ main
  

Note: If errors occur, Some directories have previously been hardcoded to search for thier
requirements within the root directory of a generic Unix Distro. Fixes will
have to be deployed to make this more flexible. 
