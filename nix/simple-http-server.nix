{
  lib,
  stdenv,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
  darwin,
}:

rustPlatform.buildRustPackage rec {
  pname = "simple-http-server";
  version = "0.1.0";  # adjust version as needed
  
  src = fetchFromGitHub {
    owner = "Anton-4";
    repo = "simple-http-server";
    rev = "f3089e5736a1e8abdb69ba9e7618fe5e518a2df0";
    sha256 = "sha256-Vcckv75hmJV7F9mqPtM3piSIZg9MvKI/oU7/tv4viy4=";
  };
  
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
  };

  nativeBuildInputs = [ pkg-config ];

  buildInputs =
    [ openssl ]
    ++ lib.optionals stdenv.hostPlatform.isDarwin [
      darwin.apple_sdk.frameworks.Security
    ];
}
