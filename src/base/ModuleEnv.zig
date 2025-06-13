//! The common state for a module: any data useful over the full lifetime of its compilation that lives beyond individual IR's.
//!
//! Stores all interned data like idents, strings, and problems.
//!
//! This reduces the size of this module's IRs as they can store references to this
//! interned (and deduplicated) data instead of storing the values themselves.

const std = @import("std");
const type_mod = @import("../types.zig");
const problem = @import("../problem.zig");
const collections = @import("../collections.zig");
const Ident = @import("Ident.zig");
const StringLiteral = @import("StringLiteral.zig");
const DiagnosticPosition = @import("DiagnosticPosition.zig");
const exitOnOom = collections.utils.exitOnOom;

const Type = type_mod.Type;
const Problem = problem.Problem;

const Self = @This();

gpa: std.mem.Allocator,
idents: Ident.Store = .{},
ident_ids_for_slicing: collections.SafeList(Ident.Idx),
strings: StringLiteral.Store,
types_store: type_mod.Store,
problems: Problem.List,
/// Line starts for error reporting. We retain only start and offset positions in the IR
/// and then use these line starts to calculate the line number and column number as required.
/// this is a more compact representation at the expense of extra computation only when generating error diagnostics.
line_starts: std.ArrayList(u32),
/// The original source bytes. We use these to generate error diagnostics.
/// TODO think about how we will manage this using the cache. Should we only
/// read these when we need them to report an error? instead of keeping all of this in memory.
/// This implementation here is simple, but let's us progress with working snapshot tests
/// and we can validate the error messages and region information there.
source: std.ArrayList(u8),

/// Initialize the module environment.
pub fn init(gpa: std.mem.Allocator, source_bytes: []const u8) Self {
    // TODO: maybe wire in smarter default based on the initial input text size.

    var source = std.ArrayList(u8).init(gpa);
    source.appendSlice(source_bytes) catch |err| exitOnOom(err);

    return Self{
        .gpa = gpa,
        .idents = Ident.Store.initCapacity(gpa, 1024),
        .ident_ids_for_slicing = collections.SafeList(Ident.Idx).initCapacity(gpa, 256),
        .strings = StringLiteral.Store.initCapacityBytes(gpa, 4096),
        .types_store = type_mod.Store.initCapacity(gpa, 2048, 512),
        .problems = Problem.List.initCapacity(gpa, 64),
        .line_starts = std.ArrayList(u32).init(gpa),
        .source = source,
    };
}

/// Deinitialize the module environment.
pub fn deinit(self: *Self) void {
    self.idents.deinit(self.gpa);
    self.ident_ids_for_slicing.deinit(self.gpa);
    self.strings.deinit(self.gpa);
    self.types_store.deinit();
    self.problems.deinit(self.gpa);
    self.line_starts.deinit();
    self.source.deinit();
}

/// Helper to push a problem to the ModuleEnv
pub fn pushProblem(self: *Self, p: Problem) void {
    _ = self.problems.append(self.gpa, p);
}

/// Calculate and store line starts from the source text
pub fn calculateLineStarts(self: *Self, source: []const u8) !void {
    self.line_starts.clearRetainingCapacity();
    self.line_starts = try DiagnosticPosition.findLineStarts(self.gpa, source);
}

/// Get diagnostic position information for a given range
pub fn getDiagnosticPosition(self: *Self, source: []const u8, begin: u32, end: u32) !DiagnosticPosition {
    return DiagnosticPosition.position(source, self.line_starts.items, begin, end);
}
