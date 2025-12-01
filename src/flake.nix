# Use this flake with `nix develop ./src`

{
  description = "Roc flake for the new compiler, written in Zig.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # to easily make configs for multiple architectures
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,

      ...
    }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];

    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        zig = pkgs.zig_0_15;
        shellFunctions = ''
          buildcmd() {
            zig build roc
          }
          export -f buildcmd

          testcmd() {
            zig build snapshot && zig build test
          }
          export -f testcmd

          fmtcmd() {
            zig build fmt
          }
          export -f fmtcmd

          cicmd() {
            zig build fmt && ./ci/zig_lints.sh && zig build && zig build snapshot && zig build test && zig build test-playground
          }
          export -f cicmd
        '';

      in
      {
        packages = {
          default = self.packages.${system}.roc;
          roc = pkgs.stdenv.mkDerivation (finalAttrs: {
            pname = "roc";
            version = "0.0.0";
            src = ./..;
            deps = pkgs.callPackage ../build.zig.zon.nix {};
            nativeBuildInputs = [ zig.hook ];
            buildInputs = [ ];
            dontConfigure = true;
            zigBuildFlags = [
              "--system"
              "${finalAttrs.deps}"
            ];

            meta.mainProgram = "roc";
          });
        };

        apps = {
          default = self.apps.${system}.roc;
          roc = {
            type = "app";
            program = pkgs.lib.getExe self.packages.${system}.roc;
            meta = {
              description = "Roc CLI";
              mainProgram = "roc";
            };
          };
        };

        devShell = pkgs.mkShell {
          buildInputs = [
            zig
            pkgs.zls
            pkgs.git # for use in ci/zig_lints.sh
            (pkgs.writeShellScriptBin "zon2lock" "nix run github:Cloudef/zig2nix -- zon2lock build.zig.zon")
            (pkgs.writeShellScriptBin "zon2nix" "nix run github:Cloudef/zig2nix -- zon2nix build.zig.zon2json-lock")
          ];

          shellHook = ''
            ${shellFunctions}

            echo "Some convenient commands:"
            echo "${shellFunctions}" | grep -E '^\s*[a-zA-Z_][a-zA-Z0-9_]*\(\)' | sed 's/().*//' | sed 's/^[[:space:]]*/  /' | while read func; do
              body=$(echo "${shellFunctions}" | sed -n "/''${func}()/,/^[[:space:]]*}/p" | sed '1d;$d' | tr '\n' ';' | sed 's/;$//' | sed 's/[[:space:]]*$//')
              echo "  $func = $body"
            done
            echo "  zon2lock = nix run github:Cloudef/zig2nix -- zon2lock build.zig.zon"
            echo "  zon2nix = nix run github:Cloudef/zig2nix -- zon2nix build.zig.zon2json-lock"
            echo ""

            unset NIX_CFLAGS_COMPILE
            unset NIX_LDFLAGS
          ''; # unset to fix: Unrecognized C flag from NIX_CFLAGS_COMPILE: -fmacro-prefix-map
        };
        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
