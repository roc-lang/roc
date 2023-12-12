{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    roc.url = "github:roc-lang/roc";
  };

  outputs = { self, nixpkgs, flake-utils, roc, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        rocPkgs = roc.packages.${system};

        rocFull = rocPkgs.full;

      in
      {
        formatter = pkgs.nixpkgs-fmt;

        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs;
              [
                rocFull # cli included in here
              ];

            # ROC_LSP_PATH will be read by https://github.com/ivan-demchenko/roc-vscode-unofficial
            shellHook = ''
              export ROC_LSP_PATH=${rocFull}/bin/roc_ls
            '';
          };
        };
      });
}
