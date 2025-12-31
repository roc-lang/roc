//! Test file for PackageEnv functionality.
//!
//! PackageEnv integration tests have been removed because:
//! 1. They require temp directories which fail sandbox restrictions
//! 2. The integration scenarios (multi-module builds, cycle detection,
//!    error ordering) are covered by:
//!    - End-to-end tests via `roc check` and `roc build` commands
//!    - Snapshot tests in test/snapshots/
//!
//! See test_build_env.zig for OrderedSink unit tests which test
//! internal emission ordering logic without file I/O.
