{
  description = "Allows sharing dependencies between dev tools and roc";

  inputs = {
    # change this path to the path of your roc folder
    roc.url = "path:/home/anton/gitrepos/roc3/roc";
    # to easily make configs for multiple architectures
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, roc, flake-utils }:
    let supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import roc.inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        rocShell = roc.devShell.${system};
      in {
        devShell = pkgs.mkShell {
          packages = let
            devInputs = (with pkgs; [ less gdb ]);
            vscodeWithExtensions = pkgs.vscode-with-extensions.override {
              vscodeExtensions = with pkgs.vscode-extensions; [
                matklad.rust-analyzer
                eamodio.gitlens
                bbenoist.nix
                vadimcn.vscode-lldb
              ];
            };
          in [ vscodeWithExtensions devInputs ];

          inputsFrom = [ rocShell ];

          # env vars
          LLVM_SYS_130_PREFIX = rocShell.LLVM_SYS_130_PREFIX;
          NIX_GLIBC_PATH = rocShell.NIX_GLIBC_PATH;
          LD_LIBRARY_PATH = rocShell.LD_LIBRARY_PATH;
          NIXPKGS_ALLOW_UNFREE = rocShell.NIXPKGS_ALLOW_UNFREE;
        };
      });
}

