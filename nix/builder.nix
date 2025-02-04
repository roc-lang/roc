{ pkgs, lib, rustPlatform, compile-deps, subPackage ? null }:
let
  inherit (compile-deps) zigPkg llvmPkgs glibcPath libGccSPath;

  subPackagePath = if subPackage != null then "crates/${subPackage}" else null;
  mainBin = if subPackage == "language_server" then "roc_language_server" else "roc";
  filteredSource = pkgs.callPackage ./fileFilter.nix { };
  buildRoc = buildTypeStr:
    rustPlatform.buildRustPackage {
      pname = "roc" + lib.optionalString (subPackage != null) "_${subPackage}";
      version = "0.0.1";
      buildType = buildTypeStr;

      buildAndTestSubdir = subPackagePath;

      src = filteredSource;

      cargoLock = {
        lockFile = ../Cargo.lock;
        outputHashes = {
          "inkwell-0.4.0" = "sha256-J2mdwf167GhEadEL2XJ39FXeY8roV9aYdhOBfIwbPbE=";
          "rustyline-9.1.1" = "sha256-aqQqz6nSp+Qn44gm3jXmmQUO6/fYTx7iLph2tbA24Bs=";
        };
      };

      shellHook = ''
        export LLVM_SYS_180_PREFIX="${llvmPkgs.dev}"
      '';

      # required for zig
      XDG_CACHE_HOME =
        "xdg_cache"; # prevents zig AccessDenied error github.com/ziglang/zig/issues/6810
      # want to see backtrace in case of failure
      RUST_BACKTRACE = 1;

      # skip running rust tests, problems:
      # building of example platforms requires network: Could not resolve host
      # zig AccessDenied error github.com/ziglang/zig/issues/6810
      # Once instance has previously been poisoned ??
      doCheck = false;

      nativeBuildInputs = (with pkgs; [
        cmake
        git
        pkg-config
        python3
        llvmPkgs.dev
        llvmPkgs.lib
        zigPkg
      ]);

      buildInputs = (with pkgs;
        [
          libffi
          libxml2
          ncurses
          zlib
          cargo
          makeWrapper # necessary for postBuild wrapProgram
        ] ++ lib.optionals pkgs.stdenv.isDarwin
          (with pkgs.darwin.apple_sdk.frameworks;
          [
            AppKit
            CoreFoundation
            CoreServices
            Foundation
            Security
          ]));

    LLVM_SYS_180_PREFIX = lib.optionalString (llvmPkgs.dev != null) "${llvmPkgs.dev}";


      # cp: to copy str.zig,list.zig...
      # wrapProgram pkgs.stdenv.cc: to make ld available for compiler/build/src/link.rs
      postInstall =
        let
          binPath =
            lib.makeBinPath [ pkgs.stdenv.cc ];
          linuxArgs = lib.optionalString pkgs.stdenv.isLinux
            "--set NIX_GLIBC_PATH ${glibcPath} --set NIX_LIBGCC_S_PATH ${libGccSPath}";
          rocPath = "$out/bin/roc";
          wrapRoc = "wrapProgram ${rocPath} ${linuxArgs} --prefix PATH : ${binPath}";
        in
        # need to check if roc bin exists since it might not if subPackage is set
        ''
          if test -f ${rocPath}; then
            ${wrapRoc}
          fi
        '';

      # https://ryantm.github.io/nixpkgs/stdenv/meta/
      meta = {
        homepage = "https://www.roc-lang.org/";
        license = lib.licenses.upl;
        mainProgram = mainBin;
      };
    };
in
{
  roc-debug = buildRoc "debug";
  roc-release = buildRoc "release";
}
