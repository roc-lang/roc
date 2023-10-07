{
  description = "Allows sharing dependencies between dev tools and roc. Prevents version GLIBC_2.36 not found.";

  inputs = {
    # change this path to the path of your roc folder
    roc.url = "path:/home/username/gitrepos/roc";
  };
  outputs = { self, roc, flake-utils }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
      flake-utils = roc.inputs.flake-utils;
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import roc.inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        isAarch64Darwin = pkgs.stdenv.hostPlatform.system == "aarch64-darwin";

        rocShell = roc.devShell.${system};
      in
      {
        devShell = pkgs.mkShell {
          packages =
            let
              devInputs = with pkgs;
                [ less bashInteractive ]
                ++ (if isAarch64Darwin then [ ] else [ gdb ]);

              vscodeWithExtensions = pkgs.vscode-with-extensions.override {
                vscodeExtensions = with pkgs.vscode-extensions;
                  [
                    matklad.rust-analyzer
                    # eamodio.gitlens
                    bbenoist.nix
                    tamasfe.even-better-toml
                  ] ++ (if isAarch64Darwin then [ ] else [ vadimcn.vscode-lldb ])
                  ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
                    {
                      name = "roc-lang-support";
                      publisher = "benjamin-thomas";
                      version = "0.0.4";
                      # keep this sha for the first run, nix will tell you the correct one to change it to
                      sha256 = "sha256-USZiXdvYa8hxj62cy6hdiS5c2tIDIQxSyux684lyAEY=";
                    }
                  ]
                ;
              };
            in
            [ vscodeWithExtensions devInputs ];

          inputsFrom = [ rocShell ];

          # env vars
          NIX_GLIBC_PATH = rocShell.NIX_GLIBC_PATH;
          LD_LIBRARY_PATH = rocShell.LD_LIBRARY_PATH;
          NIXPKGS_ALLOW_UNFREE = rocShell.NIXPKGS_ALLOW_UNFREE;

          # to set the LLVM_SYS_<VERSION>_PREFIX
          shellHook = rocShell.shellHook;

          formatter = pkgs.nixpkgs-fmt;
        };
      });
}
