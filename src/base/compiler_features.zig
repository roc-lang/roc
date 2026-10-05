//! Explicit, build-time rollout gates for compiler performance improvements.
//! All are off by default; the build includes their mask in artifact identity.

const options = @import("build_options");
pub const Feature = @import("compiler_feature_specs.zig").Feature;
pub const cacheIdentity = @import("compiler_feature_specs.zig").cacheIdentity;
pub const compilerArtifactHash = @import("compiler_feature_specs.zig").compilerArtifactHash;

comptime {
    if (@import("builtin").is_test) @import("std").testing.refAllDecls(@import("compiler_feature_specs.zig"));
}

pub fn enabled(comptime feature: Feature) bool {
    return (options.compiler_perf_flags & feature.mask()) != 0;
}

pub const inhabitedness_memo = enabled(.inhabitedness_memo);
pub const nominal_views = enabled(.nominal_views);
pub const constructor_projection = enabled(.constructor_projection);
pub const tag_projection = enabled(.tag_projection);
pub const settled_scratch = enabled(.settled_scratch);
pub const early_ctfe_cache = enabled(.early_ctfe_cache);
pub const const_completion = enabled(.const_completion);
pub const lazy_nominal_rows = enabled(.lazy_nominal_rows);
pub const finalized_literal_cache = enabled(.finalized_literal_cache);
pub const late_callable_cache = enabled(.late_callable_cache);
