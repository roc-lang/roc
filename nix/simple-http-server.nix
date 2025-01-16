{ rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage {
  pname = "simple-http-server";
  version = "0.1.0";  # adjust version as needed
  
  src = fetchFromGitHub {
    owner = "Anton-4";
    repo = "simple-http-server";
    rev = "f3089e5736a1e8abdb69ba9e7618fe5e518a2df0";
  };
  
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
  };
}
