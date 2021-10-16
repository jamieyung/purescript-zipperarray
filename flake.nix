{
  description = "A non-empty array with one element focused";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, easy-purescript-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "purescript-zipperarray";

        pkgs = import nixpkgs { inherit system; };

        nodejs = pkgs.nodejs-14_x;

        # normalizing on one version of JavaScript for this project
        easy-ps = import easy-purescript-nix { inherit pkgs; } // rec {
          psa = import "${easy-purescript-nix}/psa" { inherit pkgs nodejs; };
          pscid = import "${easy-purescript-nix}/pscid" { inherit pkgs nodejs; };
          pulp = import "${easy-purescript-nix}/pulp" { inherit pkgs nodejs; };
          purescript-language-server = import "${easy-purescript-nix}/purescript-language-server" { inherit pkgs nodejs; };
          purs-tidy = import "${easy-purescript-nix}/purs-tidy" { inherit pkgs nodejs; };
        };

        purs = easy-ps.purs-0_14_4;
      in
      rec {
        devShell = pkgs.mkShell {
          inherit name;
          buildInputs = with pkgs; [
            dhall
            dhall-lsp-server
            nodePackages.bower
            purs
            easy-ps.spago
            # easy-ps.spago2nix
            easy-ps.pscid
            easy-ps.purescript-language-server
            easy-ps.purs-tidy
            easy-ps.pulp
            nodejs
          ];
        };
      }
    );
}
