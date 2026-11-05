//! Compile-time experiments. No environment setting selects a cache policy.
pub const Mode = enum { no_sites, no_intern, scratch_pool, no_sites_scratch_pool, profile };
pub const mode: Mode = .profile;
pub const sites = mode == .scratch_pool or mode == .profile;
pub const intern = mode != .no_intern;
pub const pool = mode == .scratch_pool or mode == .no_sites_scratch_pool;
pub const profile = mode == .profile;

pub const Counters = struct {
    lookups: u64 = 0,
    misses: u64 = 0,
    intern_calls: u64 = 0,
    key_words: u64 = 0,
    scratch_allocs: u64 = 0,
    scratch_bytes: u64 = 0,
    immortal_allocs: u64 = 0,
    immortal_bytes: u64 = 0,
    site_publishes: u64 = 0,
    origin_notes: u64 = 0,
};
pub threadlocal var counters: Counters = .{};
