{
  description = "Roc flake template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    roc.url = "github:roc-lang/roc";
  };

  outputs = { self, nixpkgs, flake-utils, roc, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # see "packages =" in https://github.com/roc-lang/roc/blob/main/flake.nix
        rocPkgs = roc.packages.${system};

        rocFull = rocPkgs.full;

      in
      {
        formatter = pkgs.nixpkgs-fmt;

        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs;
              [
                rocFull # includes CLI
              ];

            # For vscode plugin https://github.com/ivan-demchenko/roc-vscode-unofficial
            shellHook = ''
              export ROC_LANGUAGE_SERVER_PATH=${rocFull}/bin/roc_language_server
            '';
          };
        };
      });
}
