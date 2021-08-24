{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.yosys
    pkgs.arachne-pnr
    pkgs.icestorm
    haskellPackages.cabal-install
    
   
   
  ];
  LC_ALL = "C.UTF-8";
}
