//! Cache configuration and directory management for the Roc compiler cache system.
//!
//! This module provides platform-specific cache directory logic that matches
//! the Rust implementation, ensuring consistency across the codebase.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const CoreCtx = @import("ctx").CoreCtx;

const Allocator = std.mem.Allocator;

const CacheOs = enum { windows, macos, other };

fn cacheOs(os: std.Target.Os.Tag) CacheOs {
    return switch (os) {
        .windows => .windows,
        .macos => .macos,
        .freestanding,
        .other,
        .contiki,
        .fuchsia,
        .hermit,
        .managarm,
        .haiku,
        .hurd,
        .illumos,
        .linux,
        .plan9,
        .rtems,
        .serenity,
        .dragonfly,
        .freebsd,
        .netbsd,
        .openbsd,
        .driverkit,
        .ios,
        .maccatalyst,
        .tvos,
        .visionos,
        .watchos,
        .uefi,
        .@"3ds",
        .ps3,
        .ps4,
        .ps5,
        .psp,
        .vita,
        .emscripten,
        .wasi,
        .amdhsa,
        .amdpal,
        .cuda,
        .mesa3d,
        .nvcl,
        .opencl,
        .opengl,
        .vulkan,
        => .other,
    };
}

/// Cache configuration constants
pub const Constants = struct {
    /// Default cache directory name
    pub const DEFAULT_CACHE_DIR = ".roc_cache";

    /// Default file extension for cache files
    pub const CACHE_FILE_EXT = ".rcache";

    /// Maximum cache file size (256MB)
    pub const MAX_CACHE_SIZE = 256 * 1024 * 1024;

    /// Cache format version. Folded into the module-cache version hash, so bump
    /// this whenever the serialized layout changes in a way the automatic
    /// top-level-field hash can't see (e.g. a node-payload layout edit).
    /// 3: annotation node payload gained mentions/introduces-type-var flags.
    /// 4: numeric suffix metadata was renamed to suffix targets.
    /// 5: nominal record declared-field-order and unnamed padding fields added
    ///    node, diagnostic, and type-annotation payloads.
    /// 6: merge with typed node/static-dispatch payload layout changes.
    /// 7: field-order layout metadata moved from nominal-only to general field-order.
    /// 8: ModuleEnv stores source-relative file-import dependency metadata.
    /// 9: ModuleEnv records runtime-prepared static builtin serialization state.
    /// 10: ModuleEnv common identifiers include Crypto digest and hasher builtin names.
    /// 11: Builtin indices and common identifiers changed for Encoding.Json and Encoding.HttpHeader.
    /// 12: Builtin.Encoding.Json structural encode/parse support changed common identifiers.
    /// 13: ModuleEnv stores deep content identity hashes.
    /// 14: ModuleEnv also stores scheme instantiation evidence records.
    /// 15: Checked encoder_for runtime representation changed serialized compiler state.
    /// 16: Static dispatch constraints carry introducing-site provenance.
    /// 17: Numeral literals carry materialization metadata.
    /// 18: Exact-numeral pipeline: parser-owned numeral digit tables, the
    ///     exact-numeral pattern node, and NumeralInfo constraint payloads.
    /// 19: Annotation node payload records a precomputed `contains_underscore` flag.
    /// 20: Static-data roots and iterator representation metadata changed serialized state.
    /// 21: Nominal declaration table serialized with the type store.
    /// 22: Checked function serialization no longer carries instantiation stamps.
    /// 23: Stored nested functions carry durable local-procedure context identity.
    /// 24: CommonIdents carries the range_exclusive/range_inclusive method idents.
    /// 25: Canonical node storage records where-clause rigid ownership.
    /// 26: ModuleEnv scheme instantiation evidence was renamed to scheme-use evidence.
    /// 27: Empty-row provenance and checked iterator-step topology changed serialized state.
    /// 28: Field access canonicalization no longer contains method or qualified-dispatch fallbacks.
    /// 29: String interpolation constraint metadata moved into the type store.
    /// 30: Checked interpolation nodes retain their explicit dispatcher type variable.
    /// 31: Canonical expression storage includes compiler-derived method nodes.
    /// 32: Builtin indices include width-specific f32 conversion and math wrappers.
    /// 33: Builtin indices, identifiers, and checked layouts include integer SIMD vectors.
    /// 34: Combined the empty-row and interpolation checked-artifact layouts.
    /// 35: Platform requirement roots carry producer-authored dispatch evidence.
    /// 36: Checked iterator procedure identity includes Str.iter_utf8.
    /// 37: Nested procedure sites declare their producer-authored evidence source.
    /// 38: Platform provides entries include their exact platform-local definition.
    /// 39: ModuleEnv stores exact top-level demand dependencies.
    /// 41: Encoding protocol names containers by Roc shape: list, tuple, record, dict.
    /// 42: Checked modules store explicitly rejected static-dispatch obligations.
    /// 43: Checked type stores persist representatives and structural union ranks.
    /// 45: Where-clause owners persist rooted annotation ownership semantics.
    /// 46: Where alias declarations are their own statement kind.
    /// 47: Type descriptors carry the static-dispatch rejection marker.
    /// 48: Source imports retain parser-owned bindings and type-module owners
    ///     instead of reconstructing them from normalized module identities.
    /// 49: Associated lookups retain exact alias-resolution targets in CIR.
    /// 50: Interned literal entries record maximum runtime backing alignment.
    /// 51: Checked dispatch evidence and Boxy runtime metadata changed serialized compiler state.
    /// 52: Optional + defaulted record fields: canonical record annotations
    ///     carry field presence and default expressions, field-access paths
    ///     keep source-ordered required/optional segments, type-store fields
    ///     carry the static kind axis (required/optional/defaulted with a
    ///     default identity; no `absent` state, `present` renamed
    ///     `required`), and checked layouts include field kinds and archived
    ///     defaults (design.md "Field Kinds", "Defaulted Fields").
    /// 53: Generalized checked record fields retain presence-variable identity,
    ///     and const record evidence retains optional source-value types.
    /// 54: Compile-time root requests retain exact checked-root identity.
    /// 55: Record constructors retain exact checker-selected omitted defaults.
    /// 56: Record field presence uses an explicit sentinel representation.
    /// 57: Field-default roots can own their literal-conversion evaluation.
    /// 58: Checked iterator procedure identity includes List.iter_rev, the
    ///     numeric to/until ranges, and the F32/F64 range helpers, with
    ///     to/until carrying producer-specific representations distinct from
    ///     the range helpers they do not delegate to.
    /// 59: Range syntax produces Builtin.Num.Range values, range dispatch uses
    ///     `_to` methods, and stored ranges mint iterator representations while
    ///     numeric range hooks explicitly delegate to that representation.
    /// 60: Checked modules retain explicit rank-1 binding-scheme identities.
    /// 61: Static dispatch constraints no longer serialize checker-local expect regions.
    /// 62: For-loop dispatch plans retain explicit iterator and step type variables.
    /// 63: ModuleEnv retains canonicalization-selected top-level and value-binding definitions.
    /// 64: Compile-time root selection rejects values containing callables.
    /// 65: A hosted entry written without a module resolves to the platform
    ///     module's own declaration.
    /// 66: Record expressions carry a span of unset (`name: _`) fields.
    /// 67: Checked record expressions carry their unset field labels.
    /// 68: `??` defaults restricted to nominal backing records (new diagnostic).
    /// 69: Defaults are any pure expression; the not-literal diagnostic became
    ///     the CAN default-cycle diagnostic.
    /// 70: Serialized canonicalization diagnostics retain the explicit codec
    ///     family for internal builtin types.
    /// 71: Annotation node payload packs its boolean flags into one byte and
    ///     records the source region of the annotated name.
    /// 72: Checked binding schemes serialize generated-codec relations that
    ///     downstream specializations must revalidate.
    /// 73: Generated codec contracts retain structural body shapes and mark
    ///     specialization-conditional method capabilities.
    /// 74: Canonicalization warns on a `?` applied to a function's return
    ///     value (new diagnostic).
    /// 75: Checked dispatch data drops symbolic constraint-callable evidence
    ///     and records recursive resolved references explicitly.
    /// 76: Scheme uses record their substitution; schemes record their
    ///     quantified variables and each evidence parameter its slot.
    /// 77: Evidence parameters record their constraint callable type.
    /// 78: Procedure templates record their root evidence.
    /// 79: Recursive-reference provenance is separate from shared scheme uses.
    /// 80: CIR has an explicit mutable-binder pattern and binding-name warning
    ///     diagnostic node tags.
    /// 81: Stored functions retain callable-path dispatch evidence across
    ///     reusable compile-time values.
    /// 82: Literal dispatch plans retain their enclosing pattern-failure
    ///     expression without increasing the serialized plan size.
    /// 83: Where methods retain their complete type annotation in CIR.
    /// 84: Literal patterns record their exact failure owner, including
    ///     statements and definitions as well as expressions.
    /// 85: Scheme codec requirements retain instantiation and synthetic-owner
    ///     classifications across checked-environment rechecks.
    /// 86: Hoisted-root pruning resolves associated lookup identities through
    ///     the expression's owning module, including imported callable bodies.
    /// 87: Pure function types carry no effect dependencies; unifying an
    ///     effect-polymorphic function with a pure one makes each dependency
    ///     pure.
    /// 88: CIR retains exact source occurrences for mutable binding writes.
    /// 89: Literal patterns retain their equality callable and failure owner
    ///     in a pattern-only context.
    /// 90: Scheme-use evidence belongs to explicit value and dispatch edges;
    ///     type applications no longer publish spurious value-use records.
    /// 93: A mismatched `if`/`match` branch or return value is reported at and
    ///     poisons the expression producing its value, leaving the expected
    ///     result type intact.
    /// 94: Hosted declarations must have effectful function types and keep
    ///     their hosted type variables intact.
    /// 95: Tag unions in annotation output positions are implicitly open
    ///     (polarity): extensionless unions there carry an anonymous flex
    ///     extension, while alias declaration bodies and where-method
    ///     signatures store a rigid deferral marker resolved per use site.
    ///     Scheme-use records distinguish per-use where-method signature
    ///     copies from evidence-bearing instantiations, and checked dispatch
    ///     plans retain independent-callable/nested-evidence-reuse flags.
    /// 96: Serialized `SafeMultiList` columns hold live rows only (capacity ==
    ///     len) instead of a capacity-sized region, and every persisted byte is
    ///     declared: `Node.Payload` variants fill the union exactly and
    ///     `NumeralLiteral` declares its trailing bytes.
    /// 97: Record types always carry an explicit extension variable; a record
    ///     update's base row is an ordinary record whose tail is a flex var.
    /// 98: Checked procedure template tables publish the templates whose
    ///     evaluation can reach code checking replaced with a runtime error.
    /// 99: Calls of non-function values report a dedicated diagnostic, and
    ///     a call to an in-flight recursive def whose annotation is an alias
    ///     of a function type classifies its effect through the alias.
    /// 100: A static-dispatch obligation whose where-method signature is
    ///      erroneous is recorded as rejected on every receiver.
    /// 101: Canonicalization caches source-local output and defers imported
    ///      names, including literal suffixes, until imports finish checking.
    /// 102: Generated-codec method roles identify subject types modulo
    ///      transparent aliases, so subjects spelled through different aliases
    ///      of one type share a role.
    /// 103: A use of an annotated binding's predeclared scheme is recorded
    ///      against the binding's own scheme, never the predeclared copy.
    /// 104: Generated-codec calls carry no conditional flag; a derived record
    ///      parser always adds `MissingRequiredField(Str)` to its error row.
    /// 105: A generated-codec call for a derived nominal application the walk
    ///      already covers names the derivation that covers it.
    /// 106: A stored nested-function use records its scheme substitution and
    ///      the checked instance its containing value stores.
    /// 107: An expression that did not parse canonicalizes to a runtime error
    ///      carrying `expr_syntax_error`, and the canonicalize diagnostic tags
    ///      no stage produces are gone.
    /// 108: Each parameterized type declaration publishes its formals'
    ///      variances and its `Try` error-cell formal, which importers consult
    ///      at an external annotation base instead of answering unknown.
    /// 109: Definitions that closed their annotated result row by FORWARDING a
    ///      closed value publish that fact, which every use—importing modules
    ///      included—reads to re-open its own copy of the row (design.md
    ///      "Row Subsumption").
    /// 110: Every alias and nominal declaration publishes whether its body
    ///      opens a row at a positive or negative position; a result row named
    ///      through an alias records its adapter-reachable site; a signature
    ///      with a `where` clause publishes no row coercion; and a value
    ///      binding's unquantified implicitly opened rows are grounded by rank.
    /// 111: A hosted function records a row coercion for the `Try` error row
    ///      its annotation closes as written, so every use re-opens its copy;
    ///      a signature with a `where` clause records one like any other.
    /// 112: A signature naming a parameterised function alias opens its
    ///      formal's result occurrence as its own row, an alias's backing
    ///      decides where its argument rows sit, and a static-dispatch use
    ///      re-opens a coerced target's result row, so more definitions
    ///      record a row coercion for the same source.
    /// 113: A coerced row's re-open crosses alias links in its extension
    ///      chain and keeps no alias layer on its spine; an alias whose formal
    ///      is the result row's extension opens it however it is spelled.
    /// 114: A result-row twin crosses alias links in its argument's row, and
    ///      an alias whose formal took a twin presents the twin as that
    ///      argument, or is presented as its backing when it also used the
    ///      shared argument.
    /// 115: An annotated top-level VALUE whose root row, or its root `Try`'s
    ///      error row, was closed by forwarding records a row coercion marked
    ///      as a value's, and every lookup that re-opens a coerced row records
    ///      that it did (`ResultRowReopen`), which post-check stages read.
    /// 116: Every type alias instance records whether its backing is still
    ///      its declaration's body under its arguments or a copy opened
    ///      something inside it (`Alias.backing`); an opened instance is
    ///      related to another application of its alias by its backing, and
    ///      alias layers survive re-opens and result-row twins.
    /// 117: An opened alias instance records where it was opened (by the
    ///      annotation walk, or at a use), and a use's instance never wins a
    ///      merge, so an annotated definition keeps its annotation.
    /// 118: Every alias instance records which of its declaration's formals
    ///      the body uses (`Alias.body_formals`), and an opened instance's
    ///      phantom arguments are related exactly.
    /// 119: An alias instance carries its declaration's hidden arguments
    ///      after its declared ones (`Alias.declared_arity`) and where its
    ///      result spine ends (`Alias.spine`), replacing `Alias.backing` and
    ///      `Alias.body_formals`; checked aliases record their declared
    ///      arity.
    /// 120: A widened alias instance never wins a merge: after its backing is
    ///      related to a structure it joins the structure's class, so a
    ///      definition's type keeps its annotation's rows as written.
    /// 121: A formal reached off the spine through a spine link the body
    ///      also reaches elsewhere is split with a hidden `e⁺` rather than
    ///      taken as the spine slot itself, and an alias application checked
    ///      module data builds from a declaration's syntax carries the
    ///      declaration's hidden arguments.
    /// 122: A widened alias instance meeting a structure no longer joins the
    ///      structure's class; which spelling a merged class keeps is
    ///      presentation only.
    /// 124: An expression inside a block that refers to a nominal type the
    ///      block declares is not selected as a hoisted root.
    /// 125: A block-local alias of a value is not generalized.
    /// 126: A hoisted root is rejected by its checked dispatch evidence
    ///      naming a block-local nominal's method, not by syntax.
    /// 128: Hoist selection follows structural comparisons' component
    ///      derivation edges.
    /// 129: Hoist selection rejects roots whose instantiated types reach a
    ///      block-local `to_inspect` override.
    /// 130: A block-local alias generalizes only when it names a function.
    /// 131: Hoist selection recognizes an imported module's block-local
    ///      methods.
    pub const CACHE_VERSION = 131;
};

/// Configuration for the Roc cache system.
///
/// This struct controls cache behavior including storage location,
/// size limits, and cleanup policies.
pub const CacheConfig = struct {
    enabled: bool = true,
    cache_dir: ?[]const u8 = null, // null = use default
    max_size_mb: u32 = 1024, // 1GB default
    max_age_days: u32 = 30, // 30 days default
    verbose: bool = false, // Print cache statistics
    roc_ctx: CoreCtx = undefined,

    const Self = @This();

    /// Get the default cache directory for the current platform.
    ///
    /// This implementation matches the Rust roc_cache_dir() function:
    /// - Respects XDG_CACHE_HOME if set
    /// - Falls back to ~/.cache/roc on Unix and %APPDATA%\Roc on Windows
    /// - Uses "roc" on Unix and "Roc" on Windows as the cache dir name
    pub fn getDefaultCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        // ROC_CACHE_DIR selects the cache root ahead of platform defaults.
        // Useful for test isolation and CI on any OS.
        if (self.roc_ctx.getEnvVar("ROC_CACHE_DIR", allocator)) |roc_dir| {
            return roc_dir;
        } else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.EnvironmentVariableMissing => {},
        }
        // Respect XDG_CACHE_HOME if set
        if (self.roc_ctx.getEnvVar("XDG_CACHE_HOME", allocator)) |xdg_cache| {
            defer allocator.free(xdg_cache);
            return std.fs.path.join(allocator, &[_][]const u8{ xdg_cache, getCacheDirName() });
        } else |err| {
            switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.EnvironmentVariableMissing => {},
            }
            // Fall back to platform defaults
            const home_env = switch (cacheOs(builtin.target.os.tag)) {
                .windows => "APPDATA",
                .macos, .other => "HOME",
            };

            const home_dir = self.roc_ctx.getEnvVar(home_env, allocator) catch |home_err| switch (home_err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.EnvironmentVariableMissing => return error.NoHomeDirectory,
            };
            defer allocator.free(home_dir);

            const cache_path = switch (cacheOs(builtin.target.os.tag)) {
                .macos => try std.fs.path.join(allocator, &[_][]const u8{ home_dir, "Library", "Caches", getCacheDirName() }),
                .windows => try std.fs.path.join(allocator, &[_][]const u8{ home_dir, getCacheDirName() }),
                .other => try std.fs.path.join(allocator, &[_][]const u8{ home_dir, ".cache", getCacheDirName() }),
            };

            return cache_path;
        }
    }

    /// Get the effective cache directory, using default if none specified.
    pub fn getEffectiveCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        if (self.cache_dir) |dir| {
            return allocator.dupe(u8, dir);
        } else {
            return self.getDefaultCacheDir(allocator);
        }
    }

    /// Get the version-specific cache directory.
    ///
    /// This isolates cache entries by compiler version to prevent
    /// conflicts when switching between compiler versions.
    pub fn getVersionCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const base_dir = try self.getEffectiveCacheDir(allocator);
        defer allocator.free(base_dir);

        const version_dir = try getCompilerVersionDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ base_dir, version_dir });
    }

    /// Get the checked-artifact cache directory.
    pub fn getCheckedArtifactCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "mod" });
    }

    /// Exact platform/app composition results, separate from reusable modules.
    pub fn getPlatformPairingCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);
        return std.fs.path.join(allocator, &.{ version_dir, "pair" });
    }

    /// Get the canonicalized-module cache directory.
    ///
    /// Separate from the checked-artifact directory because the two caches key
    /// their entries on different inputs: a canonicalized entry names one
    /// module's source alone, while a checked entry names a module together
    /// with its imports' checked keys.
    pub fn getCanonicalizedModuleCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "can" });
    }

    /// Get the module source cache directory for tooling-owned materialized sources.
    pub fn getModuleCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "src" });
    }

    /// Get the executable cache directory (for cached linked executables).
    pub fn getExeCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "exe" });
    }

    /// Get the test cache directory (for cached test results).
    pub fn getTestCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "test" });
    }

    /// Get the prepared Wasm host cache directory.
    pub fn getWasmHostCacheDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "wasm-host" });
    }

    /// Get the cache entries directory (alias for module cache dir).
    pub fn getCacheEntriesDir(self: Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        return self.getModuleCacheDir(allocator);
    }

    /// Get maximum cache size in bytes.
    pub fn getMaxSizeBytes(self: Self) u64 {
        return @as(u64, self.max_size_mb) * 1024 * 1024;
    }

    /// Get maximum age in nanoseconds.
    pub fn getMaxAgeNanos(self: Self) i64 {
        return @as(i64, self.max_age_days) * 24 * 60 * 60 * 1_000_000_000;
    }
};

/// Statistics tracking for cache operations.
///
/// This struct tracks cache performance metrics that can be
/// displayed with the --verbose flag.
pub const CacheStats = struct {
    /// Checked-module cache counters. These keep their original meaning: they
    /// count only checked-artifact entries.
    hits: u64 = 0,
    misses: u64 = 0,
    invalidations: u64 = 0,
    stores: u64 = 0,
    store_failures: u64 = 0,
    bytes_read: u64 = 0,
    bytes_written: u64 = 0,

    /// Canonicalized-module cache counters, parallel to the checked ones above.
    canonicalized_hits: u64 = 0,
    canonicalized_misses: u64 = 0,
    canonicalized_invalidations: u64 = 0,
    canonicalized_stores: u64 = 0,
    canonicalized_store_failures: u64 = 0,
    canonicalized_bytes_read: u64 = 0,
    canonicalized_bytes_written: u64 = 0,

    const Self = @This();

    /// Which cache an operation belongs to. Every recording call names one, so
    /// no counter is ever shared between the two caches.
    pub const Kind = enum { checked, canonicalized };

    /// Record a cache hit.
    pub fn recordHit(self: *Self, bytes_read: u64) void {
        self.recordHitFor(.checked, bytes_read);
    }

    /// Record a cache miss.
    pub fn recordMiss(self: *Self) void {
        self.recordMissFor(.checked);
    }

    /// Record a cache invalidation.
    pub fn recordInvalidation(self: *Self) void {
        self.recordInvalidationFor(.checked);
    }

    /// Record a successful cache store.
    pub fn recordStore(self: *Self, bytes_written: u64) void {
        self.recordStoreFor(.checked, bytes_written);
    }

    /// Record a failed cache store.
    pub fn recordStoreFailure(self: *Self) void {
        self.recordStoreFailureFor(.checked);
    }

    /// Record a cache hit for one cache.
    pub fn recordHitFor(self: *Self, kind: Kind, bytes_read: u64) void {
        switch (kind) {
            .checked => {
                self.hits += 1;
                self.bytes_read += bytes_read;
            },
            .canonicalized => {
                self.canonicalized_hits += 1;
                self.canonicalized_bytes_read += bytes_read;
            },
        }
    }

    /// Record a cache miss for one cache.
    pub fn recordMissFor(self: *Self, kind: Kind) void {
        switch (kind) {
            .checked => self.misses += 1,
            .canonicalized => self.canonicalized_misses += 1,
        }
    }

    /// Record a cache invalidation for one cache.
    pub fn recordInvalidationFor(self: *Self, kind: Kind) void {
        switch (kind) {
            .checked => self.invalidations += 1,
            .canonicalized => self.canonicalized_invalidations += 1,
        }
    }

    /// Record a successful cache store for one cache.
    pub fn recordStoreFor(self: *Self, kind: Kind, bytes_written: u64) void {
        switch (kind) {
            .checked => {
                self.stores += 1;
                self.bytes_written += bytes_written;
            },
            .canonicalized => {
                self.canonicalized_stores += 1;
                self.canonicalized_bytes_written += bytes_written;
            },
        }
    }

    /// Record a failed cache store for one cache.
    pub fn recordStoreFailureFor(self: *Self, kind: Kind) void {
        switch (kind) {
            .checked => self.store_failures += 1,
            .canonicalized => self.canonicalized_store_failures += 1,
        }
    }

    /// Get total checked-cache operations.
    pub fn getTotalOps(self: Self) u64 {
        return self.hits + self.misses;
    }

    /// Get total canonicalized-cache operations.
    pub fn getCanonicalizedTotalOps(self: Self) u64 {
        return self.canonicalized_hits + self.canonicalized_misses;
    }

    /// Get checked-cache hit rate as a percentage.
    pub fn getHitRate(self: Self) f64 {
        const total = self.getTotalOps();
        if (total == 0) return 0.0;
        return (@as(f64, @floatFromInt(self.hits)) / @as(f64, @floatFromInt(total))) * 100.0;
    }

    /// Get canonicalized-cache hit rate as a percentage.
    pub fn getCanonicalizedHitRate(self: Self) f64 {
        const total = self.getCanonicalizedTotalOps();
        if (total == 0) return 0.0;
        return (@as(f64, @floatFromInt(self.canonicalized_hits)) / @as(f64, @floatFromInt(total))) * 100.0;
    }
};

/// Get the platform-specific cache directory name.
/// Returns "roc" on Unix and "Roc" on Windows (matches Rust implementation).
pub fn getCacheDirName() []const u8 {
    return switch (cacheOs(builtin.target.os.tag)) {
        .windows => "Roc",
        .macos, .other => "roc",
    };
}

/// Get the temporary directory for runtime executables.
/// This is in the system temp dir, not the persistent cache.
pub fn getTempDir(roc_ctx: CoreCtx, allocator: Allocator) Allocator.Error![]u8 {
    const temp_base = switch (cacheOs(builtin.target.os.tag)) {
        .windows => roc_ctx.getEnvVar("TEMP", allocator) catch
            roc_ctx.getEnvVar("TMP", allocator) catch
            try allocator.dupe(u8, "C:\\Windows\\Temp"),
        .macos, .other => roc_ctx.getEnvVar("TMPDIR", allocator) catch
            try allocator.dupe(u8, "/tmp"),
    };
    defer allocator.free(temp_base);

    return std.fs.path.join(allocator, &[_][]const u8{ temp_base, "roc" });
}

/// Get the version-specific temporary directory for runtime executables.
pub fn getVersionTempDir(roc_ctx: CoreCtx, allocator: Allocator) Allocator.Error![]u8 {
    const temp_base = try getTempDir(roc_ctx, allocator);
    defer allocator.free(temp_base);

    const version_dir = try getCompilerVersionDir(allocator);
    defer allocator.free(version_dir);

    return std.fs.path.join(allocator, &[_][]const u8{ temp_base, version_dir });
}

/// Get a compiler version-specific directory name.
///
/// Returns the human-readable compiler version string (e.g., "debug-abcd1234")
/// to isolate cache entries between different compiler builds.
pub fn getCompilerVersionDir(allocator: Allocator) Allocator.Error![]u8 {
    // Use build-time compiler version that includes git commit SHA
    const version_info = build_options.compiler_version;
    return allocator.dupe(u8, version_info);
}
