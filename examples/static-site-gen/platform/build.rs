// If you open this platform in an IDE, with a rust-analyzer plugin,
// it needs to be able to see a custom Cargo build script.
// Otherwise it complains that Cargo will not link the host to the app.
// (The IDE has no way of knowing about the Roc compiler, which is our real "custom build script"!)
// This dummy build script is enough to keep everybody happy.
fn main() {}
