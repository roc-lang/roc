{
  description = "Allows sharing dependencies between dev tools and roc. Prevents version GLIBC_2.36 not found.";

  inputs = {
    # change this path to the path of your roc folder
    roc.url = "path:/home/username/gitrepos/roc9/roc";
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
            devInputs = (with pkgs; [ less gdb bashInteractive]);
            vscodeWithExtensions = pkgs.vscode-with-extensions.override {
              vscodeExtensions = with pkgs.vscode-extensions; [
                matklad.rust-analyzer
                # eamodio.gitlens
                bbenoist.nix
                vadimcn.vscode-lldb
                tamasfe.even-better-toml
              ]
                  ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
                     {
                        name = "roc-lang-support";
                        publisher = "benjamin-thomas";
                        version = "0.0.4";
                        # keep this sha for the first run, nix will tell you the correct one to change it to
                        sha256 = "sha256-mabNegZ+XPQ6EIHFk6jz2mAPLHAU6Pm3w0SiFB7IE+s=";
                      }
                    ]
                  ;
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
