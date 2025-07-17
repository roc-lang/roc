//! Direct memory cache for Roc modules using pointer relocation
//!
//! This module provides an alternative caching strategy that:
//! 1. Writes the ModuleEnv directly as the first bytes of the cache
//! 2. Writes all data structures contiguously after the ModuleEnv
//! 3. On load, reads the entire file into memory and fixes up pointers
//!
//! This approach minimizes deserialization overhead by avoiding separate
//! allocations and copies for each data structure.

const std = @import("std");
const base = @import("../base.zig");
const canonicalize = @import("../check/canonicalize.zig");
const collections = @import("../collections.zig");
const types = @import("../types.zig");
const relocate_mod = @import("../base/relocate.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = base.ModuleEnv;
const CIR = canonicalize.CIR;
const ALIGNMENT = 16; // All data is aligned to 16 bytes

/// Magic number for cache validation
const CACHE_MAGIC: u32 = 0x444D4352; // "DMRC" (Direct Memory Roc Cache)
const CACHE_VERSION: u32 = 1;

/// Header written at the very beginning of the cache file
pub const Header = struct {
    magic: u32,
    version: u32,
    /// Total size of the file including header
    total_size: u64,
    /// Offset where ModuleEnv starts (right after header)
    module_env_offset: u64,
    /// Size of the serialized CIR data
    cir_size: u64,
    /// Diagnostic counts
    error_count: u32,
    warning_count: u32,
    /// Padding to ensure alignment
    _padding: [8]u8 = [_]u8{0} ** 8,

    comptime {
        std.debug.assert(@sizeOf(Header) % ALIGNMENT == 0);
    }
};

/// Metadata for CIR serialization
const CIRMetadata = struct {
    all_defs: canonicalize.CIR.Def.Span,
    all_statements: canonicalize.CIR.Statement.Span,
    module_name_len: u32,
    nodes_len: u32,
    regions_len: u32,
    extra_data_len: u32,
    external_decls_len: u32,
};

const Node = @import("../check/canonicalize/Node.zig");

/// Write a ModuleEnv and CIR to a cache file using direct memory layout
pub fn writeCacheFile(
    allocator: Allocator,
    path: []const u8,
    module_env: *ModuleEnv,
    cir: CIR,
) !void {
    // Calculate sizes needed
    const header_size = @sizeOf(Header);
    const module_env_size = @sizeOf(ModuleEnv);

    // Calculate size for each data structure with padding
    const idents_bytes_size = calculatePaddedSize(module_env.idents.interner.bytes.items.len);
    const idents_strings_size = calculatePaddedSize(@sizeOf(u32) * module_env.idents.interner.strings.count());
    const idents_outer_indices_size = calculatePaddedSize(module_env.idents.interner.outer_indices.items.len * @sizeOf(collections.SmallStringInterner.StringIdx));
    const idents_regions_size = calculatePaddedSize(module_env.idents.interner.regions.items.len * @sizeOf(base.Region));
    const idents_attributes_size = calculatePaddedSize(module_env.idents.attributes.items.len * @sizeOf(base.Ident.Attributes));
    const ident_ids_size = calculatePaddedSize(module_env.ident_ids_for_slicing.items.items.len * @sizeOf(base.Ident.Idx));
    const strings_size = calculatePaddedSize(module_env.strings.buffer.items.len);
    const types_slots_size = calculatePaddedSize(module_env.types.slots.backing.items.items.len * @sizeOf(types.store.Slot));
    const types_descs_size = calculatePaddedSize(@sizeOf(usize) * 2 + module_env.types.descs.backing.len * 128);
    const types_vars_size = calculatePaddedSize(module_env.types.vars.items.items.len * @sizeOf(types.Var));
    const types_record_fields_size = calculatePaddedSize(@sizeOf(usize) * 2 + module_env.types.record_fields.items.len * 64);
    const types_tags_size = calculatePaddedSize(@sizeOf(usize) * 2 + module_env.types.tags.items.len * 64);
    const exposed_by_str_size = calculatePaddedSize(calculateHashMapSize(void, &module_env.exposed_by_str));
    const exposed_nodes_size = calculatePaddedSize(calculateHashMapSize(u16, &module_env.exposed_nodes));
    const line_starts_size = calculatePaddedSize(module_env.line_starts.items.items.len * @sizeOf(u32));
    const source_size = calculatePaddedSize(module_env.source.len);

    // Calculate CIR sizes
    const cir_store_nodes_size = calculatePaddedSize(@sizeOf(usize) * 2 + cir.store.nodes.len() * 64);
    const cir_store_regions_size = calculatePaddedSize(@sizeOf(usize) * 2 + cir.store.regions.len() * 32);
    const cir_store_extra_data_size = calculatePaddedSize(cir.store.extra_data.items.len * @sizeOf(u32));
    const cir_external_decls_size = calculatePaddedSize(cir.external_decls.items.items.len * @sizeOf(canonicalize.CIR.ExternalDecl));
    const cir_imports_size = calculatePaddedSize(calculateImportsSize(&cir.imports));

    const cir_size = cir_store_nodes_size + cir_store_regions_size + cir_store_extra_data_size +
        cir_external_decls_size + cir_imports_size + @sizeOf(CIRMetadata);

    // Calculate total size
    const total_data_size = module_env_size +
        idents_bytes_size + idents_strings_size + idents_outer_indices_size + idents_regions_size + idents_attributes_size +
        ident_ids_size + strings_size +
        types_slots_size + types_descs_size + types_vars_size +
        types_record_fields_size + types_tags_size +
        exposed_by_str_size + exposed_nodes_size +
        line_starts_size + source_size + cir_size;

    const total_size = header_size + total_data_size;

    // Allocate buffer
    var buffer = try allocator.alignedAlloc(u8, ALIGNMENT, total_size);
    defer allocator.free(buffer);

    // Write header
    var header = Header{
        .magic = CACHE_MAGIC,
        .version = CACHE_VERSION,
        .total_size = total_size,
        .module_env_offset = header_size,
        .cir_size = cir_size,
        .error_count = if (cir.diagnostics) |diags| @as(u32, @intCast(diags.len)) else 0,
        .warning_count = 0, // TODO: track warnings separately
    };
    @memcpy(buffer[0..header_size], std.mem.asBytes(&header));

    // Copy ModuleEnv struct
    var offset: usize = header_size;
    @memcpy(buffer[offset..][0..module_env_size], std.mem.asBytes(module_env));
    offset += module_env_size;

    // Write data structures in order matching ModuleEnv fields
    // The key insight: we write them in the exact order they appear in ModuleEnv
    // so the relocation logic can find them predictably

    // idents.interner.bytes
    const idents_bytes = module_env.idents.interner.bytes.items;
    @memcpy(buffer[offset..][0..idents_bytes.len], idents_bytes);
    offset += calculatePaddedSize(idents_bytes.len);

    // idents.interner.strings (hash map count)
    const strings_count = @as(u32, @intCast(module_env.idents.interner.strings.count()));
    @memcpy(buffer[offset..][0..@sizeOf(u32)], std.mem.asBytes(&strings_count));
    offset += calculatePaddedSize(@sizeOf(u32) * strings_count);

    // idents.interner.outer_indices
    const idents_outer_indices = module_env.idents.interner.outer_indices.items;
    @memcpy(buffer[offset..][0 .. idents_outer_indices.len * @sizeOf(collections.SmallStringInterner.StringIdx)], std.mem.sliceAsBytes(idents_outer_indices));
    offset += calculatePaddedSize(idents_outer_indices.len * @sizeOf(collections.SmallStringInterner.StringIdx));

    // idents.interner.regions
    const idents_regions = module_env.idents.interner.regions.items;
    @memcpy(buffer[offset..][0 .. idents_regions.len * @sizeOf(base.Region)], std.mem.sliceAsBytes(idents_regions));
    offset += calculatePaddedSize(idents_regions.len * @sizeOf(base.Region));

    // idents.attributes
    const idents_attributes = module_env.idents.attributes.items;
    @memcpy(buffer[offset..][0 .. idents_attributes.len * @sizeOf(base.Ident.Attributes)], std.mem.sliceAsBytes(idents_attributes));
    offset += calculatePaddedSize(idents_attributes.len * @sizeOf(base.Ident.Attributes));

    // ident_ids_for_slicing
    const ident_ids = module_env.ident_ids_for_slicing.items.items;
    @memcpy(buffer[offset..][0 .. ident_ids.len * @sizeOf(base.Ident.Idx)], std.mem.sliceAsBytes(ident_ids));
    offset += calculatePaddedSize(ident_ids.len * @sizeOf(base.Ident.Idx));

    // strings
    const strings = module_env.strings.buffer.items;
    @memcpy(buffer[offset..][0..strings.len], strings);
    offset += calculatePaddedSize(strings.len);

    // types - slots
    const slots = module_env.types.slots.backing.items.items;
    @memcpy(buffer[offset..][0 .. slots.len * @sizeOf(types.store.Slot)], std.mem.sliceAsBytes(slots));
    offset += calculatePaddedSize(slots.len * @sizeOf(types.store.Slot));

    // types - descs (MultiArrayList)
    offset = writeMultiArrayListSimple(&module_env.types.descs.backing, buffer, offset);

    // types - vars
    const vars = module_env.types.vars.items.items;
    @memcpy(buffer[offset..][0 .. vars.len * @sizeOf(types.Var)], std.mem.sliceAsBytes(vars));
    offset += calculatePaddedSize(vars.len * @sizeOf(types.Var));

    // types - record_fields (MultiArrayList)
    offset = writeMultiArrayListSimple(&module_env.types.record_fields.items, buffer, offset);

    // types - tags (MultiArrayList)
    offset = writeMultiArrayListSimple(&module_env.types.tags.items, buffer, offset);

    // exposed_by_str
    offset = try writeHashMap(void, allocator, &module_env.exposed_by_str, buffer, offset);

    // exposed_nodes
    offset = try writeHashMap(u16, allocator, &module_env.exposed_nodes, buffer, offset);

    // line_starts
    const line_starts = module_env.line_starts.items.items;
    @memcpy(buffer[offset..][0 .. line_starts.len * @sizeOf(u32)], std.mem.sliceAsBytes(line_starts));
    offset += calculatePaddedSize(line_starts.len * @sizeOf(u32));

    // source
    @memcpy(buffer[offset..][0..module_env.source.len], module_env.source);
    offset += calculatePaddedSize(module_env.source.len);

    // Write CIR metadata
    const cir_metadata = CIRMetadata{
        .all_defs = cir.all_defs,
        .all_statements = cir.all_statements,
        .module_name_len = @intCast(cir.module_name.len),
        .nodes_len = @intCast(cir.store.nodes.len()),
        .regions_len = @intCast(cir.store.regions.len()),
        .extra_data_len = @intCast(cir.store.extra_data.items.len),
        .external_decls_len = @intCast(cir.external_decls.items.items.len),
    };
    @memcpy(buffer[offset..][0..@sizeOf(CIRMetadata)], std.mem.asBytes(&cir_metadata));
    offset += @sizeOf(CIRMetadata);

    // Write module name
    @memcpy(buffer[offset..][0..cir.module_name.len], cir.module_name);
    offset += calculatePaddedSize(cir.module_name.len);

    // Write CIR store components
    // Write nodes (MultiArrayList)
    offset = writeMultiArrayListSimple(@constCast(&cir.store.nodes), buffer, offset);

    // Write regions (MultiArrayList)
    offset = writeMultiArrayListSimple(@constCast(&cir.store.regions), buffer, offset);

    if (cir.store.extra_data.items.len > 0) {
        const extra_data_bytes = std.mem.sliceAsBytes(cir.store.extra_data.items);
        @memcpy(buffer[offset..][0..extra_data_bytes.len], extra_data_bytes);
        offset += calculatePaddedSize(extra_data_bytes.len);
    }

    // Write external decls
    if (cir.external_decls.items.items.len > 0) {
        const external_decls_bytes = std.mem.sliceAsBytes(cir.external_decls.items.items);
        @memcpy(buffer[offset..][0..external_decls_bytes.len], external_decls_bytes);
        offset += calculatePaddedSize(external_decls_bytes.len);
    }

    // Write imports (simplified for now)
    offset += calculatePaddedSize(calculateImportsSize(&cir.imports));

    // Write to file
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();
    try file.writeAll(buffer);
}

/// Load a cache file and restore ModuleEnv and CIR
pub fn loadCacheFile(
    allocator: Allocator,
    path: []const u8,
) !struct { module_env: *ModuleEnv, cir: CIR, buffer: []u8 } {
    // Read entire file
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    var buffer = try allocator.alignedAlloc(u8, ALIGNMENT, file_size);
    errdefer allocator.free(buffer);

    _ = try file.read(buffer);

    // Validate header
    if (buffer.len < @sizeOf(Header)) return error.InvalidCache;
    const header = @as(*const Header, @ptrCast(buffer.ptr));

    if (header.magic != CACHE_MAGIC) return error.InvalidMagic;
    if (header.version != CACHE_VERSION) return error.InvalidVersion;
    if (header.total_size != file_size) return error.SizeMismatch;

    // Get ModuleEnv pointer
    const module_env = @as(*ModuleEnv, @ptrCast(@alignCast(&buffer[header.module_env_offset])));

    // Now we need to fix up all the pointers
    // The data follows immediately after ModuleEnv in a predictable order
    var offset: usize = header.module_env_offset + @sizeOf(ModuleEnv);
    const base_addr = @intFromPtr(buffer.ptr);

    // Fix idents.interner.bytes
    if (module_env.idents.interner.bytes.items.len > 0) {
        module_env.idents.interner.bytes.items.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.idents.interner.bytes.items.len);
    }

    // Skip strings hash map for now (needs reconstruction)
    const strings_count = std.mem.readInt(u32, buffer[offset..][0..@sizeOf(u32)], .little);
    offset += calculatePaddedSize(@sizeOf(u32) * strings_count);

    // Fix idents.interner.outer_indices
    if (module_env.idents.interner.outer_indices.items.len > 0) {
        module_env.idents.interner.outer_indices.items.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.idents.interner.outer_indices.items.len * @sizeOf(collections.SmallStringInterner.StringIdx));
    }

    // Fix idents.interner.regions
    if (module_env.idents.interner.regions.items.len > 0) {
        module_env.idents.interner.regions.items.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.idents.interner.regions.items.len * @sizeOf(base.Region));
    }

    // Fix idents.attributes
    if (module_env.idents.attributes.items.len > 0) {
        module_env.idents.attributes.items.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.idents.attributes.items.len * @sizeOf(base.Ident.Attributes));
    }

    // Fix ident_ids_for_slicing
    if (module_env.ident_ids_for_slicing.items.items.len > 0) {
        module_env.ident_ids_for_slicing.items.items.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.ident_ids_for_slicing.items.items.len * @sizeOf(base.Ident.Idx));
    }

    // Fix strings
    if (module_env.strings.buffer.items.len > 0) {
        module_env.strings.buffer.items.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.strings.buffer.items.len);
    }

    // Fix types.slots
    if (module_env.types.slots.backing.items.items.len > 0) {
        module_env.types.slots.backing.items.items.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.types.slots.backing.items.items.len * @sizeOf(types.store.Slot));
    }

    // Fix types.descs (MultiArrayList)
    offset = fixMultiArrayListSimple(&module_env.types.descs.backing, buffer[offset..], 0, base_addr + offset);

    // Fix types.vars
    if (module_env.types.vars.items.items.len > 0) {
        module_env.types.vars.items.items.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.types.vars.items.items.len * @sizeOf(types.Var));
    }

    // Fix types.record_fields
    offset = fixMultiArrayListSimple(&module_env.types.record_fields.items, buffer[offset..], 0, base_addr + offset);

    // Fix types.tags
    offset = fixMultiArrayListSimple(&module_env.types.tags.items, buffer[offset..], 0, base_addr + offset);

    // Fix exposed_by_str
    offset = fixHashMap(void, &module_env.exposed_by_str, buffer, offset, base_addr);

    // Fix exposed_nodes
    offset = fixHashMap(u16, &module_env.exposed_nodes, buffer, offset, base_addr);

    // Fix line_starts
    if (module_env.line_starts.items.items.len > 0) {
        module_env.line_starts.items.items.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.line_starts.items.items.len * @sizeOf(u32));
    }

    // Fix source
    if (module_env.source.len > 0) {
        module_env.source.ptr = @ptrFromInt(base_addr + offset);
        offset += calculatePaddedSize(module_env.source.len);
    }

    // Set the allocator
    module_env.gpa = allocator;

    // Load CIR
    const cir = try loadCIR(allocator, module_env, buffer[offset..][0..header.cir_size], base_addr, offset);

    return .{ .module_env = module_env, .cir = cir, .buffer = buffer };
}

// Helper functions

fn calculatePaddedSize(size: usize) usize {
    return std.mem.alignForward(usize, size, ALIGNMENT);
}

// Simplified MultiArrayList serialization that just reserves space

fn writeMultiArrayListSimple(list: anytype, buffer: []u8, start_offset: usize) usize {
    _ = list;
    var offset = start_offset;

    // Write a marker for the size (we'll use a fixed size for now)
    const size_marker = @as(u32, 1024 * 16); // 16KB per MultiArrayList
    @memcpy(buffer[offset..][0..@sizeOf(u32)], std.mem.asBytes(&size_marker));
    offset += @sizeOf(u32);

    // Reserve the space
    offset += size_marker;

    return calculatePaddedSize(offset - start_offset) + start_offset;
}

fn fixMultiArrayListSimple(list: anytype, buffer: []const u8, start_offset: usize, base_addr: usize) usize {
    _ = list;
    _ = base_addr;
    var offset = start_offset;

    // Read the size marker
    const size_marker = std.mem.readInt(u32, buffer[offset..][0..@sizeOf(u32)], .little);
    offset += @sizeOf(u32);

    // Skip the reserved space
    offset += size_marker;

    return calculatePaddedSize(offset - start_offset) + start_offset;
}

fn calculateHashMapSize(comptime V: type, map: anytype) usize {
    _ = map.map.count();
    // Size = count + (count * (key_len + key_bytes + value_bytes))
    var size: usize = @sizeOf(u32); // count

    var iter = map.map.iterator();
    while (iter.next()) |entry| {
        size += @sizeOf(u32); // key length
        size += entry.key_ptr.len; // key bytes
        if (V != void) {
            size += @sizeOf(V); // value
        }
    }

    return size;
}

fn writeHashMap(comptime V: type, allocator: Allocator, map: anytype, buffer: []u8, start_offset: usize) !usize {
    var offset = start_offset;

    // Write count
    const count = @as(u32, @intCast(map.map.count()));
    @memcpy(buffer[offset..][0..@sizeOf(u32)], std.mem.asBytes(&count));
    offset += @sizeOf(u32);

    // Write entries
    var entries = try allocator.alloc(struct { key: []const u8, value: V }, count);
    defer allocator.free(entries);

    var iter = map.map.iterator();
    var i: usize = 0;
    while (iter.next()) |entry| : (i += 1) {
        entries[i] = .{ .key = entry.key_ptr.*, .value = entry.value_ptr.* };
    }

    // Sort entries for deterministic output
    std.mem.sort(@TypeOf(entries[0]), entries, {}, struct {
        fn lessThan(_: void, a: @TypeOf(entries[0]), b: @TypeOf(entries[0])) bool {
            return std.mem.lessThan(u8, a.key, b.key);
        }
    }.lessThan);

    // Write sorted entries
    for (entries) |entry| {
        // Write key length
        const key_len = @as(u32, @intCast(entry.key.len));
        @memcpy(buffer[offset..][0..@sizeOf(u32)], std.mem.asBytes(&key_len));
        offset += @sizeOf(u32);

        // Write key
        @memcpy(buffer[offset..][0..entry.key.len], entry.key);
        offset += entry.key.len;

        // Write value
        if (V != void) {
            @memcpy(buffer[offset..][0..@sizeOf(V)], std.mem.asBytes(&entry.value));
            offset += @sizeOf(V);
        }
    }

    return calculatePaddedSize(offset - start_offset) + start_offset;
}

fn fixHashMap(comptime V: type, map: anytype, buffer: []u8, start_offset: usize, base_addr: usize) usize {
    _ = map;
    _ = base_addr;
    var offset = start_offset;

    // Read count
    const count = std.mem.readInt(u32, buffer[offset..][0..@sizeOf(u32)], .little);
    offset += @sizeOf(u32);

    // For now, we'll need to reconstruct the hash map
    // This is a limitation of the current approach - hash maps need special handling
    // In a production version, we might want to use a custom hash map that can be relocated

    // Skip the data for now
    var i: usize = 0;
    while (i < count) : (i += 1) {
        const key_len = std.mem.readInt(u32, buffer[offset..][0..@sizeOf(u32)], .little);
        offset += @sizeOf(u32) + key_len;
        if (V != void) {
            offset += @sizeOf(V);
        }
    }

    return calculatePaddedSize(offset - start_offset) + start_offset;
}

fn calculateImportsSize(imports: *const canonicalize.CIR.Import.Store) usize {
    // For now, return a fixed size - proper implementation would serialize the imports
    _ = imports;
    return 1024; // 1KB placeholder for imports
}

/// Calculate the total file size needed for a ModuleEnv
pub fn calculateFileSize(module_env: *ModuleEnv) usize {
    // This is a helper function for tests
    // In practice, the size would be stored in the header
    _ = module_env;
    return 1024 * 1024 * 10; // 10MB placeholder
}

fn loadCIR(allocator: Allocator, module_env: *ModuleEnv, buffer: []const u8, base_addr: usize, start_offset: usize) !CIR {
    var offset: usize = 0;

    // Read CIR metadata
    const metadata = @as(*const CIRMetadata, @ptrCast(@alignCast(buffer[offset..])));
    offset += @sizeOf(CIRMetadata);

    // Read module name
    const module_name = buffer[offset..][0..metadata.module_name_len];
    offset += calculatePaddedSize(metadata.module_name_len);

    // Create NodeStore
    var store = canonicalize.CIR.NodeStore{
        .gpa = allocator,
        .nodes = .{},
        .regions = .{},
        .extra_data = .{},
        .scratch_statements = try base.Scratch(canonicalize.CIR.Statement.Idx).init(allocator),
        .scratch_exprs = try base.Scratch(canonicalize.CIR.Expr.Idx).init(allocator),
        .scratch_record_fields = try base.Scratch(canonicalize.CIR.RecordField.Idx).init(allocator),
        .scratch_match_branches = try base.Scratch(canonicalize.CIR.Expr.Match.Branch.Idx).init(allocator),
        .scratch_match_branch_patterns = try base.Scratch(canonicalize.CIR.Expr.Match.BranchPattern.Idx).init(allocator),
        .scratch_if_branches = try base.Scratch(canonicalize.CIR.Expr.IfBranch.Idx).init(allocator),
        .scratch_where_clauses = try base.Scratch(canonicalize.CIR.WhereClause.Idx).init(allocator),
        .scratch_patterns = try base.Scratch(canonicalize.CIR.Pattern.Idx).init(allocator),
        .scratch_pattern_record_fields = try base.Scratch(canonicalize.CIR.PatternRecordField.Idx).init(allocator),
        .scratch_record_destructs = try base.Scratch(canonicalize.CIR.Pattern.RecordDestruct.Idx).init(allocator),
        .scratch_type_annos = try base.Scratch(canonicalize.CIR.TypeAnno.Idx).init(allocator),
        .scratch_anno_record_fields = try base.Scratch(canonicalize.CIR.TypeAnno.RecordField.Idx).init(allocator),
        .scratch_exposed_items = try base.Scratch(canonicalize.CIR.ExposedItem.Idx).init(allocator),
        .scratch_defs = try base.Scratch(canonicalize.CIR.Def.Idx).init(allocator),
        .scratch_diagnostics = try base.Scratch(canonicalize.CIR.Diagnostic.Idx).init(allocator),
    };

    // Fix up pointers for nodes (MultiArrayList)
    if (metadata.nodes_len > 0) {
        offset = fixMultiArrayListSimple(&store.nodes, buffer[offset..], 0, base_addr + start_offset + offset);
    }

    // Fix up pointers for regions (MultiArrayList)
    if (metadata.regions_len > 0) {
        offset = fixMultiArrayListSimple(&store.regions, buffer[offset..], 0, base_addr + start_offset + offset);
    }

    // Fix up pointers for extra_data
    if (metadata.extra_data_len > 0) {
        store.extra_data.items.ptr = @ptrFromInt(base_addr + start_offset + offset);
        store.extra_data.items.len = metadata.extra_data_len;
        store.extra_data.capacity = metadata.extra_data_len;
        offset += calculatePaddedSize(metadata.extra_data_len * @sizeOf(u32));
    }

    // Create external decls list
    var external_decls = collections.SafeList(canonicalize.CIR.ExternalDecl){};
    if (metadata.external_decls_len > 0) {
        external_decls.items.items.ptr = @ptrFromInt(base_addr + start_offset + offset);
        external_decls.items.items.len = metadata.external_decls_len;
        offset += calculatePaddedSize(metadata.external_decls_len * @sizeOf(canonicalize.CIR.ExternalDecl));
    }

    // Create CIR
    return canonicalize.CIR{
        .env = module_env,
        .store = store,
        .diagnostics = null,
        .all_defs = metadata.all_defs,
        .all_statements = metadata.all_statements,
        .external_decls = external_decls,
        .imports = canonicalize.CIR.Import.Store.init(),
        .module_name = module_name,
    };
}
