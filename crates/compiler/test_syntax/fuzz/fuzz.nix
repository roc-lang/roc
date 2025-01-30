{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.rustup.override {
      targets = ["x86_64-unknown-linux-gnu"]; # Adjust for your desired target
    })
  ];

  shellHook = ''
    # Ensure rustup is initialized
    export RUSTUP_HOME="$PWD/.rustup"
    export CARGO_HOME="$PWD/.cargo"
    mkdir -p $RUSTUP_HOME $CARGO_HOME

    # Install Rust nightly
    rustup install nightly
    rustup default nightly

    # Add rustup and cargo to PATH
    export PATH="$CARGO_HOME/bin:$PATH"

    echo "Rust nightly and Cargo are available on PATH."
  '';
}
