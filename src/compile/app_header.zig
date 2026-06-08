//! Parse the header of an `app .roc` file.
//!
//! Returns the platform spec, platform qualifier, and the non-platform
//! package entries declared in the header.
//!
//! This is a single-pass replacement for the three CLI helpers
//! `extractPlatformSpecFromApp`, `extractPlatformQualifier`, and
//! `extractNonPlatformPackages`. It performs no URL resolution — package
//! specs are returned exactly as written in source so callers can apply
//! their own resolution policy (caching, fetching, virtualised paths).
//!
//! Reads through the `Io` vtable, so it works against virtual filesystems
//! (e.g. embedders, the wasm playground).

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");

const Allocator = std.mem.Allocator;
const Io = @import("ctx").CoreCtx;
const ModuleEnv = can.ModuleEnv;

/// One non-platform package reference from an `app` header.
pub const PackageEntry = struct {
    /// Shorthand name (the key — e.g. `hlp` in `{ hlp: "./helper_pkg/main.roc" }`).
    /// Arena-owned.
    shorthand: []const u8,
    /// Raw package spec as written in source (relative path, absolute path, or URL).
    /// Callers are responsible for resolution. Arena-owned.
    spec: []const u8,
};

/// Information extracted from an `app` header. All slices are arena-owned
/// by the `arena` allocator passed to `parseAppHeader`.
pub const AppHeaderInfo = struct {
    /// Raw platform spec (the string literal after `platform`). Empty if absent.
    platform_spec: []const u8,
    /// Platform qualifier (the key in `{ pf: platform "..." }`). Null if absent.
    platform_qualifier: ?[]const u8,
    /// Non-platform package entries from the `packages` collection.
    non_platform_packages: []const PackageEntry,
};

/// Errors `parseAppHeader` can return — either a header-shape problem
/// (`NotAnAppHeader`), an allocation failure, or an `Io.readFile` failure.
pub const Error = error{
    NotAnAppHeader,
    OutOfMemory,
} || Io.ReadError;

/// Read and parse an `app .roc` file, returning the header's platform and
/// package metadata.
///
/// - `io`: filesystem to read the source through.
/// - `gpa`: used for transient parsing state (freed before this returns).
/// - `arena`: holds the returned strings — kept alive as long as the caller
///   needs `AppHeaderInfo`.
pub fn parseAppHeader(
    io: Io,
    gpa: Allocator,
    arena: Allocator,
    app_path: []const u8,
) Error!AppHeaderInfo {
    var source = try io.readFile(app_path, gpa);
    source = base.source_utils.normalizeLineEndingsRealloc(gpa, source) catch |err| {
        gpa.free(source);
        return err;
    };
    defer gpa.free(source);

    var env = ModuleEnv.init(gpa, source) catch return error.OutOfMemory;
    defer env.deinit();
    env.common.source = source;

    const ast = parse.file(gpa, &env.common) catch return error.OutOfMemory;
    defer ast.deinit();

    const file_node = ast.store.getFile();
    const header = ast.store.getHeader(file_node.header);

    const app = switch (header) {
        .app => |a| a,
        else => return error.NotAnAppHeader,
    };

    const platform_field = ast.store.getRecordField(app.platform_idx);

    const platform_spec: []const u8 = blk: {
        const value_expr = platform_field.value orelse break :blk "";
        const s = stringFromExpr(ast, value_expr) catch break :blk "";
        break :blk try arena.dupe(u8, s);
    };

    const platform_qualifier: ?[]const u8 = blk: {
        const key_region = ast.tokens.resolve(platform_field.name);
        const q = source[key_region.start.offset..key_region.end.offset];
        if (q.len == 0) break :blk null;
        break :blk try arena.dupe(u8, q);
    };

    var entries: std.array_list.Managed(PackageEntry) = .init(arena);

    const packages_coll = ast.store.getCollection(app.packages);
    const fields = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
    for (fields) |field_idx| {
        const field = ast.store.getRecordField(field_idx);
        const key_region = ast.tokens.resolve(field.name);
        const shorthand = source[key_region.start.offset..key_region.end.offset];

        if (platform_qualifier) |qual| {
            if (std.mem.eql(u8, shorthand, qual)) continue;
        }

        const value_idx = field.value orelse continue;
        const value_node = ast.store.getExpr(value_idx);
        const spec: []const u8 = switch (value_node) {
            .string => |str| inner: {
                const str_region = ast.tokenizedRegionToRegion(str.region);
                const raw = source[str_region.start.offset..str_region.end.offset];
                if (raw.len < 2 or raw[0] != '"' or raw[raw.len - 1] != '"') break :inner "";
                break :inner raw[1 .. raw.len - 1];
            },
            else => "",
        };
        if (spec.len == 0) continue;

        try entries.append(.{
            .shorthand = try arena.dupe(u8, shorthand),
            .spec = try arena.dupe(u8, spec),
        });
    }

    return .{
        .platform_spec = platform_spec,
        .platform_qualifier = platform_qualifier,
        .non_platform_packages = try entries.toOwnedSlice(),
    };
}

fn stringFromExpr(ast: *parse.AST, expr_idx: parse.AST.Expr.Idx) error{ExpectedString}![]const u8 {
    const e = ast.store.getExpr(expr_idx);
    return switch (e) {
        .string => |s| {
            for (ast.store.exprSlice(s.parts)) |part_idx| {
                const part = ast.store.getExpr(part_idx);
                if (part == .string_part) {
                    return ast.resolve(part.string_part.token);
                }
            }
            return error.ExpectedString;
        },
        else => error.ExpectedString,
    };
}
