{ lib, callPackage, fetchurl, path, llvmPackages_22, stdenv, glibc, patchelf }:

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
  cmakeFlags = previous.cmakeFlags ++ [ (lib.cmakeFeature "ZIG_VERSION" "0.17.0") ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [
      # A native Zig target embeds the builder's kernel version. Pin the OS
      # minimum and libc from the locked toolchain for reproducible host code.
      (lib.cmakeFeature "ZIG_TARGET_TRIPLE" "${stdenv.hostPlatform.parsed.cpu.name}-linux.4.19-gnu.${glibc.version}")
      (lib.cmakeFeature "ZIG_TARGET_DYNAMIC_LINKER" stdenv.cc.bintools.dynamicLinker)
      (lib.cmakeBool "ZIG_USE_LLVM_CONFIG" true)
    ];
  nativeBuildInputs = previous.nativeBuildInputs ++ lib.optionals stdenv.hostPlatform.isLinux [ patchelf ];
  # CMake already strips the Release compiler. After an RPATH expansion,
  # binutils strip moves its .dynstr section outside the loadable segment.
  dontStrip = stdenv.hostPlatform.isLinux || (previous.dontStrip or false);
  preConfigure = (previous.preConfigure or "") + ''
    # Explicit targets also need the locked system-library search paths that
    # native detection previously imported from NIX_LDFLAGS.
    cmakeFlagsArray+=("-DZIG_EXTRA_BUILD_ARGS=-j$NIX_BUILD_CORES${lib.optionalString stdenv.hostPlatform.isLinux (lib.concatMapStrings (package: ";--search-prefix;${lib.getLib package}") previous.buildInputs)}")
  '';
  # CMake's stage3 build already installs the language reference. The older
  # generic recipe builds it again using a removed build-runner argument.
  postBuild = "";
  postInstall = ''
    install -Dm444 stage3/doc/langref.html -t "$doc/share/doc/zig-0.17.0/html"
  '' + lib.optionalString stdenv.hostPlatform.isLinux ''
    # An explicit target does not import native NIX_LDFLAGS. These libraries
    # come from the locked recipe; fixup removes unused search directories.
    patchelf --set-rpath "${lib.makeLibraryPath (previous.buildInputs ++ [ stdenv.cc.cc.lib ])}" "$out/bin/zig"
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
