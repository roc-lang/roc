{ lib, stdenvNoCC, fetchurl, path, xcbuild, coreutils }:

let
  distributions = {
    x86_64-linux = {
      target = "x86_64-linux";
      sha256 = "1cbe9df9f27e6b78d14ccbca43b6703a404ef79ef1c463de901d7f088d4e2026";
    };
    aarch64-linux = {
      target = "aarch64-linux";
      sha256 = "9e8d11661d4ae3bd57702a3832781e23ad151dde5798e16a5ccd503f65234ff8";
    };
    x86_64-darwin = {
      target = "x86_64-macos";
      sha256 = "4f9a1c5269aa17ebda5e6d3c2b89d6cbf36f7d2b22a0306e9ab98f25f95529c6";
    };
    aarch64-darwin = {
      target = "aarch64-macos";
      sha256 = "b607e9b9234790a008116ae5bdb71c6243b84b9fb42a53a9e70fde41c06c536a";
    };
  };
  distribution = distributions.${stdenvNoCC.hostPlatform.system};
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "zig";
  version = "0.17.0";
  # Official distribution hashes from https://ziglang.org/download/index.json.
  # The reviewed nixpkgs pin does not yet package Zig 0.17.
  src = fetchurl {
    url = "https://ziglang.org/download/${finalAttrs.version}/zig-${distribution.target}-${finalAttrs.version}.tar.xz";
    inherit (distribution) sha256;
  };
  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;
  dontPatchShebangs = true;
  # Native libc detection needs a real dynamically linked executable. Nix's
  # sandbox has no /usr/bin/env; use the locked coreutils runtime instead.
  postPatch = ''
    substituteInPlace lib/std/zig/system.zig \
      --replace-fail /usr/bin/env ${lib.getExe' coreutils "env"}
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p "$out/bin" "$out/lib/zig" "$out/share/doc/zig"
    cp zig "$out/bin/zig"
    cp -r lib/. "$out/lib/zig/"
    cp LICENSE README.md "$out/share/doc/zig/"
    runHook postInstall
  '';
  # Reuse only the generic build hook from the locked nixpkgs source. The hook
  # invokes the Zig on PATH; it does not depend on nixpkgs' Zig 0.16 compiler.
  setupHook = path + "/pkgs/development/compilers/zig/setup-hook.sh";
  env = {
    zig_default_cpu_flag = "-Dcpu=baseline";
    zig_default_optimize_flag = "--release=safe";
  };
  propagatedNativeBuildInputs = lib.optionals stdenvNoCC.hostPlatform.isDarwin [ xcbuild ];
  passthru.hook = finalAttrs.finalPackage;
  doInstallCheck = stdenvNoCC.buildPlatform.canExecute stdenvNoCC.hostPlatform;
  installCheckPhase = ''
    export ZIG_GLOBAL_CACHE_DIR="$TMPDIR/zig-global-cache"
    export ZIG_LOCAL_CACHE_DIR="$TMPDIR/zig-local-cache"
    test "$($out/bin/zig version)" = "${finalAttrs.version}"
    "$out/bin/zig" env
  '';
  meta = {
    description = "Pinned official Zig 0.17 compiler distribution";
    homepage = "https://ziglang.org/";
    license = lib.licenses.mit;
    mainProgram = "zig";
    platforms = builtins.attrNames distributions;
  };
})
