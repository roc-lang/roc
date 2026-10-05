{ lib, callPackage, fetchurl, path, llvmPackages_22, stdenv }:

let
  # The reviewed nixpkgs pin has LLVM 22 but does not yet package Zig 0.17.
  # Reuse its source-build recipe so native libc detection is patched before
  # compiling Zig. Patching the prebuilt distribution's library cannot change
  # the compiler's baked-in /usr/bin/env probe, which fails in the Nix sandbox.
  sourceHash = "b6c7f1728f043700d6529bac980800792f824256a9d2f1839b3d62beed0b8abd";
  zig = callPackage (path + "/pkgs/development/compilers/zig/generic.nix") {
    version = "0.17.0";
    hash = sourceHash;
    llvmPackages = llvmPackages_22;
  };
in
zig.overrideAttrs (previous: {
  # Official source archive hash from https://ziglang.org/download/index.json;
  # this replaces the generic recipe's Codeberg source entirely.
  src = fetchurl {
    url = "https://ziglang.org/download/0.17.0/zig-0.17.0.tar.xz";
    sha256 = sourceHash;
  };
  cmakeFlags = previous.cmakeFlags ++ [ (lib.cmakeFeature "ZIG_VERSION" "0.17.0") ];
  preConfigure = (previous.preConfigure or "") + ''
    cmakeFlagsArray+=("-DZIG_EXTRA_BUILD_ARGS=-j$NIX_BUILD_CORES")
  '';
  # CMake's stage3 build already installs the language reference. The older
  # generic recipe builds it again using a removed build-runner argument.
  postBuild = "";
  postInstall = ''
    install -Dm444 stage3/doc/langref.html -t "$doc/share/doc/zig-0.17.0/html"
  '';
  doInstallCheck = stdenv.buildPlatform.canExecute stdenv.hostPlatform;
  installCheckPhase = ''
    runHook preInstallCheck
    export ZIG_GLOBAL_CACHE_DIR="$TMPDIR/zig-global-cache"
    export ZIG_LOCAL_CACHE_DIR="$TMPDIR/zig-local-cache"
    test "$($out/bin/zig version)" = "0.17.0"
    "$out/bin/zig" env
    # Executing a native libc binary also checks the detected dynamic loader.
    printf 'int main(void) { return 0; }\n' > "$TMPDIR/native.c"
    "$out/bin/zig" cc "$TMPDIR/native.c" -o "$TMPDIR/native"
    "$TMPDIR/native"
    runHook postInstallCheck
  '';
})
