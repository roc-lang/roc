//! Compile-time controls for the composition-cache experiment branch.
//! Each compiler is measured with its own empty cache; no environment variable
//! or runtime option selects an experiment.
pub const Mode = enum { baseline, none, worker_one, worker_four, shared_one, shared_four, caller, baseline_scratch, none_scratch, caller_scratch };
pub const mode: Mode = .baseline;
pub const worker_local = mode == .worker_one or mode == .worker_four;
pub const shared_frozen = mode == .shared_one or mode == .shared_four;
pub const ways: usize = if (mode == .worker_four or mode == .shared_four) 4 else 1;
pub const site_words: u32 = if (mode == .shared_four) 4 else 1;
pub const site_enabled = mode != .none and mode != .none_scratch;
pub const reclaim_scratch = mode == .baseline_scratch or mode == .none_scratch or mode == .caller_scratch;
pub const caller_prepares = mode == .caller or mode == .caller_scratch;
