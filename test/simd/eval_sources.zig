//! Canonical Roc sources shared with the evaluator's wasm differential test.

pub const oracle = @embedFile("oracle/SimdOracle.roc");
pub const differential = @embedFile("SimdDifferential.roc");
