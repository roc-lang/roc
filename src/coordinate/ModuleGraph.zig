const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const utils = @import("../utils.zig");
const cache = @import("../cache.zig");
const collections = @import("../collections.zig");
const can = @import("../check/canonicalize.zig");
const parse = @import("../check/parse.zig");

const Package = base.Package;
const ModuleImport = base.ModuleImport;
const ModuleWork = base.ModuleWork;
const ModuleWorkIdx = base.ModuleWorkIdx;
const exitOnOom = collections.utils.exitOnOom;

const Self = @This();

modules: std.ArrayList(),
adjacencies: std.ArrayList(std.ArrayList(usize)),
gpa: std.mem.Allocator,

pub fn deinit(self: *Self) void {
    self.modules.deinit();
    for (self.adjacencies.items) |adjacency_list| {
        adjacency_list.deinit();
    }
    self.adjacencies.deinit();
}

pub fn fromPackages(package_store: Package.Store, gpa: std.mem.Allocator) Self {
    var graph = Self{
        .modules = std.ArrayList(ModuleWork(can.IR)).init(gpa),
        .adjacencies = std.ArrayList(std.ArrayList(usize)).init(gpa),
        .gpa = gpa,
    };

    var package_idx_iter = package_store.packages.iterIndices();
    while (package_idx_iter.next()) |package_idx| {
        const package = package_store.packages.get(package_idx);

        var module_idx_iter = package.modules.iterIndices();
        while (module_idx_iter.next()) |module_idx| {
            const module = package.modules.get(module_idx);

            const can_ir = loadOrCompileCanIr(
                package.absolute_dirpath,
                module.filepath_relative_to_package_root,
                gpa,
            );
            graph.modules.append(ModuleWork(can.IR){
                .package_root_absdir = package.absolute_dirpath,
                .filename_relative_to_root = module.filepath_relative_to_package_root,
                .package = package_idx,
                .can_ir = can_ir,
            });
            graph.adjacencies.append(std.ArrayList(usize).init(gpa));
        }
    }

    graph.collectAdjacencies(package_store);

    return graph;
}

fn loadOrCompileCanIr(absdir: []u8, relpath: []u8, gpa: std.mem.Allocator) can.IR {
    // TODO: find a way to provide the current Roc compiler version
    const current_roc_version = "";
    const abs_file_path = std.fs.path.join(gpa, &.{ absdir, relpath });
    // TODO: this should be an internal error if the file is missing,
    // since we should only be trying to read files that were found during
    // traversing the file system earlier.
    const contents = try utils.readFile(abs_file_path, gpa);
    const hash_of_contents = try utils.blake3Hash(contents);
    const cache_lookup = try cache.getCanIrForHashAndRocVersion(hash_of_contents, current_roc_version);

    return if (cache_lookup) |ir| ir else blk: {
        var can_ir = can.IR.init(gpa);
        const parse_ir = parse.parse(gpa, contents);
        can.canonicalize(&can_ir, &parse_ir, gpa);

        break :blk can_ir;
    };
}

fn collectAdjacencies(graph: *Self, package_store: *Package.Store) void {
    for (graph.modules.items, 0..) |metadata, metadata_index| {
        const import_store = metadata.can_ir.env.module_imports;
        const package = package_store.packages.get(metadata.package_idx);

        import_loop: for (import_store.modules.items.items) |import| {
            const from_package_idx = if (import.shorthand) |imp_shorthand| shorthand_blk: {
                for (package.dependencies.items.items) |dependency| {
                    if (std.mem.eql(u8, dependency.shorthand, imp_shorthand)) {
                        break :shorthand_blk dependency.package;
                    }
                }

                continue :import_loop;
            } else {
                metadata.package_idx;
            };

            const from_package = package_store.packages.get(from_package_idx);
            const from_package_modules = from_package.modules;

            var from_module_idx_iter = from_package_modules.iterIndices();
            while (from_module_idx_iter.next()) |from_module_idx| {
                const from_module = from_package_modules.get(from_module_idx);
                if (!std.mem.eql(u8, from_module.name, import.name)) continue :import_loop;

                import.resolved = ModuleImport.Resolved{
                    .package_idx = from_package_idx,
                    .module_idx = from_module_idx,
                };

                // TODO: find out what we need to store to avoid needing this expensive loop
                for (graph.modules.items, 0..) |search_metadata, search_index| {
                    if (search_metadata.package_idx != from_package_idx) continue;
                    if (search_metadata.module_idx != from_module_idx) continue;

                    graph.adjacencies.items[metadata_index].append(search_index);
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

pub const Sccs = struct {
    groups: std.ArrayList(std.ArrayList(usize)),
};

pub const OrderingResult = union(enum) {
    Ordered: std.ArrayList(ModuleWork(can.IR)),
    FoundCycles: std.ArrayList(std.ArrayList(*ModuleWork(can.IR))),
};

pub fn putModulesInCompilationOrder(
    self: *const Self,
    sccs: *const Sccs,
    allocator: std.mem.Allocator,
) OrderingResult {
    var modules = std.ArrayList(ModuleWork(can.IR)).init(allocator);
    var cycles = std.ArrayList(std.ArrayList(*ModuleWork(can.IR))).init(allocator);

    var group_index = sccs.groups.items.len;
    while (group_index > 0) {
        group_index -= 1;
        const group = sccs.groups.items[group_index];

        std.debug.assert(group.len >= 1);

        if (group.len == 1) {
            const module_index = group[0];
            modules.append(self.modules.items[module_index]) catch exitOnOom();
        } else {
            const cycle = std.ArrayList(*ModuleWork(can.IR)).init(allocator);
            for (group) |group_item_index| {
                cycle.append(self.modules[group_item_index]) catch exitOnOom();
            }

            cycles.append(cycle) catch exitOnOom();
        }
    }

    if (cycles.items.len == 0) {
        cycles.deinit();
        return .{ .Ordered = modules };
    } else {
        modules.deinit();
        return .{ .FoundCycles = cycles };
    }
}

// Uses Tarjan's algorithm as described in https://www.thealgorists.com/Algo/GraphTheory/Tarjan/SCC
pub fn findStronglyConnectedComponents(self: *const Self) Sccs {
    var next_unused_index = 0;
    var stack = std.ArrayList(usize).init(self.gpa);
    var all_attributes: [self.modules.items.len]Attributes = undefined;
    var sccs = Sccs{ .groups = std.ArrayList(std.ArrayList(usize)).init(self.gpa) };

    for (all_attributes) |attributes| {
        attributes.index = std.math.maxInt(usize);
        attributes.low_link = std.math.maxInt(usize);
        attributes.on_stack = false;
    }

    for (all_attributes, 0..) |attributes, module_index| {
        if (attributes.index == std.math.maxInt(usize)) {
            self.sccRecurseIntoGraph(module_index, &next_unused_index, &stack, &all_attributes, &sccs);
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
    all_attributes[current].index = next_unused_index;
    all_attributes[current].low_link = next_unused_index;
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

test "ModuleGraph fromPackages constructs correctly" {
    const module = base.Module{
        .filepath_relative_to_package_root = "mock_module.roc",
        .name = "MockModule",
    };

    var package = base.Package{
        .absolute_dirpath = "mock_package",
        .modules = std.ArrayList(base.Module).init(testing.allocator),
        .dependencies = std.ArrayList(base.Dependency).init(testing.allocator),
    };
    defer package.deinit();

    var package_store = base.Package.Store.init(testing.allocator);
    defer package_store.deinit();

    package.modules.append(module) catch Self.exitOnOom();

    package_store.packages.append(package) catch Self.exitOnOom();

    var graph = Self.fromPackages(package_store, testing.allocator);
    defer graph.deinit();

    // Verify that the graph has one module
    try std.testing.expect(graph.modules.items.len == 1);

    // Verify that the module's details are correct
    const module_work = graph.modules.items[0];
    try std.testing.expect(std.mem.eql(u8, module_work.package_root_absdir, "mock_package"));
    try std.testing.expect(std.mem.eql(u8, module_work.filename_relative_to_root, "mock_module.roc"));
}
