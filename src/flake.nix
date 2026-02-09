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
        isLinux = pkgs.stdenv.isLinux;

        # kcov build dependencies for Linux (coverage uses custom kcov fork built from source)
        kcovBuildDeps = with pkgs; [
          elfutils
          pkg-config
          curl
          zlib
        ];

        dependencies =
          (with pkgs; [
            zig
            zls
            git # for use in ci/zig_lints.sh
            typos # used in CI, helpful to run before committing to catch typos
            jq # see ci/benchmarks_zig.sh
          ])
          ++ pkgs.lib.optionals isLinux kcovBuildDeps;

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

          covcmd() {
            zig build coverage
          }
          export -f covcmd

          cicmd() {
            zig build fmt && ./ci/zig_lints.sh && zig build && zig build snapshot && zig build test && zig build test-playground && zig build coverage
          }
          export -f cicmd
        '';

      in
      {

        devShell = pkgs.mkShell {
          buildInputs = dependencies;

          shellHook = ''
            ${shellFunctions}

            echo "Some convenient commands:"
            echo "${shellFunctions}" | grep -E '^\s*[a-zA-Z_][a-zA-Z0-9_]*\(\)' | sed 's/().*//' | sed 's/^[[:space:]]*/  /' | while read func; do
              body=$(echo "${shellFunctions}" | sed -n "/''${func}()/,/^[[:space:]]*}/p" | sed '1d;$d' | tr '\n' ';' | sed 's/;$//' | sed 's/[[:space:]]*$//')
              echo "  $func = $body"
            done
            echo ""

            unset NIX_CFLAGS_COMPILE
            unset NIX_LDFLAGS
          ''; # unset to fix: Unrecognized C flag from NIX_CFLAGS_COMPILE: -fmacro-prefix-map
        };
      }
    );
}
