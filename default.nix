{ rev ? (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked.rev
, nixpkgsSource ? builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked.narHash;
  }
, pkgs ? import nixpkgsSource { }
,
}:
# we only use this file to release a nix package, use flake.nix for development
let
  desiredRustVersion = (builtins.fromTOML (builtins.readFile (./rust-toolchain.toml))).toolchain.channel;
  actualRustVersion = pkgs.rustc;
  rustVersionsMatch = pkgs.lib.strings.hasSuffix desiredRustVersion actualRustVersion;

  # When updating the zig or llvm version, make sure they stay in sync.
  # Also update in flake.nix (TODO: maybe we can use nix code to sync this)
  zigPkg = pkgs.zig_0_11;
  llvmPkgs = pkgs.llvmPackages_16;
  llvmVersion = builtins.splitVersion llvmPkgs.release_version;
  llvmMajorMinorStr = builtins.elemAt llvmVersion 0 + builtins.elemAt llvmVersion 1;
  # nix does not store libs in /usr/lib or /lib
  glibcPath =
    if pkgs.stdenv.isLinux then "${pkgs.glibc.out}/lib" else "";
  libGccSPath =
    if pkgs.stdenv.isLinux then "${pkgs.stdenv.cc.cc.lib}/lib" else "";
in

  assert pkgs.lib.assertMsg rustVersionsMatch ''
    The rust version changed in rust-toolchain.toml but the rev(commit) in nixpkgs.url in flake.nix was not updated.
    1. clone the nixpkgs repo: `git clone --depth 60000 git@github.com:NixOS/nixpkgs.git`
    2. `cd nixpkgs`
    3. `git log --oneline | rg -A 1 "rustc: <RUST_VERSION_IN_RUST_TOOLCHAIN_TOML>"`
    4. Copy the short SHA from the line **after** the commit with the message of for example `rustc: 1.67.1 -> 1.68.0`
    5. Find the long SHA by executing `git rev-parse <PASTE>`
    6. Copy the long SHA
    7. Paste it in place of the old SHA(rev) in flake.nix:inputs:nixpkgs.url
    8. execute `nix flake lock --update-input rust-overlay`
  '';

  pkgs.rustPlatform.buildRustPackage {
    pname = "roc";
    version = "0.0.1";

    src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

    cargoLock = {
      lockFile = ./Cargo.lock;
      outputHashes = {
        "criterion-0.3.5" = "sha256-+FibPQGiR45g28xCHcM0pMN+C+Q8gO8206Wb5fiTy+k=";
        "inkwell-0.2.0" = "sha256-VhTapYGonoSQ4hnDoLl4AAgj0BppAhPNA+UPuAJSuAU=";
        "plotters-0.3.1" = "sha256-noy/RSjoEPZZbOJTZw1yxGcX5S+2q/7mxnUrzDyxOFw=";
        "rustyline-9.1.1" = "sha256-aqQqz6nSp+Qn44gm3jXmmQUO6/fYTx7iLph2tbA24Bs=";
      };
    };

    shellHook = ''
      export LLVM_SYS_${llvmMajorMinorStr}_PREFIX="${llvmPkgs.llvm.dev}"
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
      llvmPkgs.clang
      llvmPkgs.llvm.dev
      llvmPkgs.bintools-unwrapped # contains lld      
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
      ] ++ lib.optionals pkgs.stdenv.isDarwin [
        pkgs.darwin.apple_sdk.frameworks.AppKit
        pkgs.darwin.apple_sdk.frameworks.CoreFoundation
        pkgs.darwin.apple_sdk.frameworks.CoreServices
        pkgs.darwin.apple_sdk.frameworks.Foundation
        pkgs.darwin.apple_sdk.frameworks.Security
      ]);

    # cp: to copy str.zig,list.zig...
    # wrapProgram pkgs.stdenv.cc: to make ld available for compiler/build/src/link.rs
    postInstall =
      if pkgs.stdenv.isLinux then ''
        wrapProgram $out/bin/roc --set NIX_GLIBC_PATH ${glibcPath} --set NIX_LIBGCC_S_PATH ${libGccSPath} --prefix PATH : ${
          pkgs.lib.makeBinPath [ pkgs.stdenv.cc ]
        }
      '' else ''
        wrapProgram $out/bin/roc --prefix PATH : ${
          pkgs.lib.makeBinPath [ pkgs.stdenv.cc ]
        }
      '';
  }
