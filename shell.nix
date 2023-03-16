#{ pkgs ? import <nixpkgs> {} }:
{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "11d3bd58ce6e32703bf69cec04dc7c38eabe14ba";
      sha256 = "0q24hb4a3fvcizns17ddd4pshlbkfdq2m6pgcjfslwlvgnbrli5l";
    })
    {
      inherit pkgs;
    };
in
pkgs.mkShell {
  allowUnfree = true;
  buildInputs = with pkgs; [
    # keep this line if you use bash
    pkgs.bashInteractive

    easy-ps.dhall-simple
    easy-ps.psa
    easy-ps.pscid
    easy-ps.spago2nix
    easy-ps.psc-package
    easy-ps.pulp
    easy-ps.purp
    # easy-ps.purs-0_14_4

    easy-ps.purs-0_15_7
    easy-ps.spago
    easy-ps.purescript-language-server
    easy-ps.purs-tidy
    easy-ps.purty
    easy-ps.zephyr

    git
    lorri
    niv
    nixpkgs-fmt
    nodejs
    which
  ];
} 
