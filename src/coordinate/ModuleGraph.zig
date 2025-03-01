const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const utils = @import("utils.zig");
const cache = @import("../cache.zig");
const collections = @import("../collections.zig");
const can = @import("../check/canonicalize.zig");
const parse = @import("../check/parse.zig");
const Filesystem = @import("../coordinate/Filesystem.zig");

const Package = base.Package;
const ModuleImport = base.ModuleImport;
const ModuleWork = base.ModuleWork;
const ModuleWorkIdx = base.ModuleWorkIdx;
const exitOnOom = collections.utils.exitOnOom;

const Self = @This();

modules: std.ArrayList(ModuleWork(can.IR)),
adjacencies: std.ArrayList(std.ArrayList(usize)),
gpa: std.mem.Allocator,

pub fn deinit(self: *Self) void {
    self.modules.deinit();
    for (self.adjacencies.items) |adjacency_list| {
        adjacency_list.deinit();
    }
    self.adjacencies.deinit();
}

pub const ConstructResult = union(enum) {
    success: Self,
    failed_to_open_module: Filesystem.OpenError,
};

/// Create a graph from a list of packages.
pub fn fromPackages(package_store: *Package.Store, fs: Filesystem, gpa: std.mem.Allocator) ConstructResult {
    var graph = Self{
        .modules = std.ArrayList(ModuleWork(can.IR)).init(gpa),
        .adjacencies = std.ArrayList(std.ArrayList(usize)).init(gpa),
        .gpa = gpa,
    };

    for (package_store.packages.items.items, 0..) |package, package_index| {
        const package_idx: u32 = @truncate(package_index);
        for (package.modules.items.items, 0..) |module, module_index| {
            const module_idx: u32 = @truncate(module_index);
            const can_ir = loadOrCompileCanIr(
                package.absolute_dirpath,
                module.filepath_relative_to_package_root,
                fs,
                gpa,
            ) catch |err| {
                return .{ .failed_to_open_module = err };
            };

            graph.modules.append(ModuleWork(can.IR){
                .package_idx = @enumFromInt(package_idx),
                .module_idx = @enumFromInt(module_idx),
                .work = can_ir,
            }) catch exitOnOom();
            graph.adjacencies.append(std.ArrayList(usize).init(gpa)) catch exitOnOom();
        }
    }

    graph.collectAdjacencies(package_store);

    return .{ .success = graph };
}

fn loadOrCompileCanIr(
    absdir: []const u8,
    relpath: []const u8,
    fs: Filesystem,
    gpa: std.mem.Allocator,
) Filesystem.OpenError!can.IR {
    // TODO: find a way to provide the current Roc compiler version
    const current_roc_version = "";
    const abs_file_path = std.fs.path.join(gpa, &.{ absdir, relpath }) catch exitOnOom();
    // TODO: this should be an internal error if the file is missing,
    // since we should only be trying to read files that were found during
    // traversing the file system earlier.
    const contents = try fs.readFile(abs_file_path, gpa);
    const hash_of_contents = utils.blake3Hash(contents);
    const cache_lookup = cache.getCanIrForHashAndRocVersion(&hash_of_contents, current_roc_version);

    return if (cache_lookup) |ir| ir else blk: {
        var can_ir = can.IR.init(gpa);
        var parse_ir = parse.parse(&can_ir.env, gpa, contents);
        can.canonicalize(&can_ir, &parse_ir, gpa);

        break :blk can_ir;
    };
}

fn collectAdjacencies(graph: *Self, package_store: *Package.Store) void {
    for (graph.modules.items, 0..) |metadata, metadata_index| {
        const import_store = metadata.work.env.imports;
        const package = package_store.packages.get(metadata.package_idx);

        import_loop: for (import_store.imports.items.items) |*import| {
            const from_package_idx = if (import.package_shorthand) |imp_shorthand| shorthand_blk: {
                for (package.dependencies.items.items) |dependency| {
                    if (std.mem.eql(u8, dependency.shorthand, imp_shorthand)) {
                        switch (dependency.package) {
                            .idx => |idx| break :shorthand_blk idx,
                            .err => continue :import_loop,
                        }
                    }
                }

                continue :import_loop;
            } else metadata.package_idx;

            const from_package = package_store.packages.get(from_package_idx);
            const from_package_modules = from_package.modules;

            for (from_package_modules.items.items, @as(u32, 0)..) |from_module, from_module_index| {
                const from_module_idx: Package.Module.Idx = @enumFromInt(from_module_index);
                if (!std.mem.eql(u8, from_module.name, import.name)) continue :import_loop;

                import.resolved = ModuleImport.Resolved{
                    .package_idx = from_package_idx,
                    .module_idx = from_module_idx,
                };

                // TODO: find out what we need to store to avoid needing this expensive loop
                for (graph.modules.items, 0..) |search_metadata, search_index| {
                    if (search_metadata.package_idx != from_package_idx) continue;
                    if (search_metadata.module_idx != from_module_idx) continue;

                    graph.adjacencies.items[metadata_index].append(search_index) catch exitOnOom();
                }
            }
        }
    }
}

const Attributes = struct {
    index: usize,
    low_link: usize,
    on_stack: bool,
};

/// Strongly connected components
pub const Sccs = struct {
    groups: std.ArrayList(std.ArrayList(usize)),
};

pub const OrderingResult = union(enum) {
    ordered: std.ArrayList(ModuleWork(can.IR)),
    found_cycle: std.ArrayList(ModuleWork(can.IR)),
};

pub fn putModulesInCompilationOrder(
    self: *const Self,
    sccs: *const Sccs,
    allocator: std.mem.Allocator,
) OrderingResult {
    var modules = std.ArrayList(ModuleWork(can.IR)).init(allocator);
    errdefer modules.deinit();

    var group_index = sccs.groups.items.len;
    while (group_index > 0) {
        group_index -= 1;
        const group = sccs.groups.items[group_index];

        std.debug.assert(group.items.len >= 1);

        if (group.items.len == 1) {
            const module_index = group.items[0];
            modules.append(self.modules.items[module_index]) catch exitOnOom();
        } else {
            var cycle = std.ArrayList(ModuleWork(can.IR)).init(allocator);
            for (group.items) |group_item_index| {
                cycle.append(self.modules.items[group_item_index]) catch exitOnOom();
            }

            return .{ .found_cycle = cycle };
        }
    }

    return .{ .ordered = modules };
}

// Uses Tarjan's algorithm as described in https://www.thealgorists.com/Algo/GraphTheory/Tarjan/SCC
pub fn findStronglyConnectedComponents(self: *const Self, gpa: std.mem.Allocator) Sccs {
    var next_unused_index: usize = 0;
    var stack = std.ArrayList(usize).init(self.gpa);

    var all_attributes = gpa.alloc(Attributes, self.modules.items.len) catch exitOnOom();
    defer gpa.free(all_attributes);

    var sccs = Sccs{ .groups = std.ArrayList(std.ArrayList(usize)).init(self.gpa) };

    for (0..all_attributes.len) |attr_index| {
        all_attributes[attr_index].index = std.math.maxInt(usize);
        all_attributes[attr_index].low_link = std.math.maxInt(usize);
        all_attributes[attr_index].on_stack = false;
    }

    for (all_attributes, 0..) |attributes, module_index| {
        if (attributes.index == std.math.maxInt(usize)) {
            self.sccRecurseIntoGraph(module_index, &next_unused_index, &stack, all_attributes, &sccs);
        }
    }

    return sccs;
}

fn sccRecurseIntoGraph(
    self: *const Self,
    current: usize,
    next_unused_index: *usize,
    stack: *std.ArrayList(usize),
    all_attributes: []Attributes,
    sccs: *Sccs,
) void {
    // Set the depth index for "current" to the smallest unused index
    all_attributes[current].index = next_unused_index.*;
    all_attributes[current].low_link = next_unused_index.*;
    next_unused_index.* += 1;
    stack.append(current) catch exitOnOom();
    all_attributes[current].on_stack = true;

    // TODO: should this be adjacencies for "current" or all nodes?
    for (self.adjacencies.items[current].items) |neighbor| {
        if (all_attributes[neighbor].index == std.math.maxInt(usize)) {
            // Successor "neighbor" has not yet been visited; recurse on it
            self.sccRecurseIntoGraph(current, next_unused_index, stack, all_attributes, sccs);
            all_attributes[current].low_link = @min(
                all_attributes[current].low_link,
                all_attributes[neighbor].low_link,
            );
        } else if (all_attributes[neighbor].on_stack) {
            // Successor "neighbor" is in "stack" and hence in the current SCC
            // If "neighbor" is not on stack, then the given edge is pointing
            // to an SCC already found and must be ignored
            all_attributes[current].low_link = @min(
                all_attributes[current].low_link,
                all_attributes[neighbor].index,
            );
        }
    }

    // If "current" is a root node, pop the stack and generate an SCC
    if (all_attributes[current].low_link == all_attributes[current].index) {
        var scc = std.ArrayList(usize).init(self.gpa);

        while (true) {
            const scc_item = stack.pop();
            all_attributes[scc_item].on_stack = false;
            scc.append(scc_item) catch exitOnOom();

            if (scc_item == current) {
                break;
            }
        }

        sccs.groups.append(scc) catch exitOnOom();
    }
}

// test "ModuleGraph fromPackages constructs correctly" {
//     const module = .{
//         .filepath_relative_to_package_root = "mock_module.roc",
//         .name = "MockModule",
//     };

//     var package = base.Package{
//         .absolute_dirpath = "mock_package",
//         .modules = std.ArrayList(base.Module).init(testing.allocator),
//         .dependencies = std.ArrayList(base.Dependency).init(testing.allocator),
//     };
//     defer package.deinit();

//     var package_store = base.Package.Store.init(testing.allocator);
//     defer package_store.deinit();

//     package.modules.append(module) catch Self.exitOnOom();

//     package_store.packages.append(package) catch Self.exitOnOom();

//     var graph = Self.fromPackages(package_store, testing.allocator);
//     defer graph.deinit();

//     // Verify that the graph has one module
//     try std.testing.expect(graph.modules.items.len == 1);

//     // Verify that the module's details are correct
//     const module_work = graph.modules.items[0];
//     try std.testing.expect(std.mem.eql(u8, module_work.package_root_absdir, "mock_package"));
//     try std.testing.expect(std.mem.eql(u8, module_work.filename_relative_to_root, "mock_module.roc"));
// }
