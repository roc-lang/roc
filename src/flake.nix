# Use this flake with `nix develop ./src`

{
  description = "Roc flake for the new compiler, written in Zig.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # to easily make configs for multiple architectures
    flake-utils.url = "github:numtide/flake-utils";

    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      gitignore,
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

        # kcov (built from source for coverage on every platform) links system
        # libs via pkg-config, but asks for "z"/"curl"/"dwarf" while nixpkgs
        # ships "zlib"/"libcurl"/"libdwarf". Provide alias .pc files so those
        # resolve; they must be packages since the nix pkg-config wrapper only
        # reads .pc dirs from buildInputs. On macOS the bare "-l" fallback only
        # searches the SDK (which has neither libz nor libcurl), so every lib
        # must resolve through pkg-config here.
        pcConfigAlias = name: pkg: pcName: pkgs.runCommand "pkgconfig-alias-${name}" { } ''
          mkdir -p "$out/lib/pkgconfig"
          cp "$(find ${pkg} -name '${pcName}.pc' | head -n1)" "$out/lib/pkgconfig/${name}.pc"
        '';
        kcovBuildDeps = with pkgs; [
          pkg-config
          curl
          zlib
          (pcConfigAlias "z" zlib.dev "zlib")
          (pcConfigAlias "curl" curl.dev "libcurl")
        ];
        # elfutils (libdw/libelf for DWARF parsing) is used by kcov on Linux.
        kcovLinuxOnlyDeps = with pkgs; [
          elfutils
        ];
        # macOS kcov uses libdwarf (linked as "dwarf") instead of elfutils.
        kcovDarwinOnlyDeps = with pkgs; [
          libdwarf
          (pcConfigAlias "dwarf" libdwarf.dev "libdwarf")
        ];
        zig = pkgs.zig_0_16;
        dependencies = [
          zig
          pkgs.zls
          pkgs.git # for use in ci/zig_lints.sh
          pkgs.typos # used in CI, helpful to run before committing to catch typos
          pkgs.jq # see ci/benchmarks_zig.sh
          (pkgs.writeShellScriptBin "zon2nix" "nix run github:Cloudef/zig2nix -- zon2nix")
        ]
        ++ kcovBuildDeps
        ++ pkgs.lib.optionals isLinux kcovLinuxOnlyDeps
        ++ pkgs.lib.optionals (!isLinux) kcovDarwinOnlyDeps;

        shellFunctions = ''
          buildcmd() {
            zig build roc
          }
          export -f buildcmd

          testcmd() {
            zig build run-check-snapshots && zig build run-test-zig
          }
          export -f testcmd

          fmtcmd() {
            zig build run-fmt-zig
          }
          export -f fmtcmd

          covcmd() {
            zig build run-coverage-parser
          }
          export -f covcmd

          cicmd() {
            zig build run-fmt-zig && zig build run-check-zig-lints && zig build roc && zig build run-check-snapshots && zig build run-test-zig && zig build run-test-playground && zig build run-coverage-parser
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
            src = gitignore.lib.gitignoreSource ./..;
            deps = pkgs.callPackage ../build.zig.zon.nix { inherit zig; };
            nativeBuildInputs = [
              zig.hook
              pkgs.pkg-config
            ]
            ++ pkgs.lib.lists.optional pkgs.stdenv.isDarwin [ pkgs.apple-sdk ];
            buildInputs = [ ];
            dontConfigure = true;
            zigBuildFlags = [
              "roc"
              "-Doptimize=Debug"
              "--system"
              "${finalAttrs.deps}"
            ];
            env.ZIG_GLOBAL_CACHE_DIR="$TMPDIR";
            meta = {
              broken = pkgs.stdenv.hostPlatform.isDarwin; # Currently this package only builds on Linux
              mainProgram = "roc";
            };
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
          buildInputs = dependencies;

          shellHook = ''
            ${shellFunctions}

            echo "Some convenient commands:"
            echo "${shellFunctions}" | grep -E '^\s*[a-zA-Z_][a-zA-Z0-9_]*\(\)' | sed 's/().*//' | sed 's/^[[:space:]]*/  /' | while read func; do
              body=$(echo "${shellFunctions}" | sed -n "/''${func}()/,/^[[:space:]]*}/p" | sed '1d;$d' | tr '\n' ';' | sed 's/;$//' | sed 's/[[:space:]]*$//')
              echo "  $func = $body"
            done
            echo "  zon2nix = nix run github:Cloudef/zig2nix -- zon2nix"
            echo ""

            unset NIX_CFLAGS_COMPILE
            unset NIX_LDFLAGS
          ''; # unset to fix: Unrecognized C flag from NIX_CFLAGS_COMPILE: -fmacro-prefix-map
        };
        formatter = pkgs.nixfmt-tree;
      }
    );
}
