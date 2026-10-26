//! The room each stack-bound check in a reserved-low-memory wasm object needs.
//!
//! When a platform reserves the addresses below its `global_base`, static data
//! sits there and the stack sits above it, so a stack overflow would write into
//! static data instead of trapping. Code compiled for such a platform therefore
//! compares the stack pointer against `__stack_low` wherever the stack can grow
//! without limit: at the entry of every function with a stack frame that lies on
//! a call cycle, and at every function the host calls. Each comparison's bound is
//! an `i32.const` relocated against `__stack_low`, and the relocation's addend is
//! the room that check needs: how far below the function's entry stack pointer
//! the function and every call chain it can start reach before another check
//! runs. Frame sizes are only known once code is compiled, so `fillCheckRooms`
//! reads them from the composed Roc object and writes each addend.
//!
//! The analysis follows the values the code derives from `__stack_pointer`
//! to the frames it reserves by lowering the stack pointer, and to the stack
//! pointer each call starts its callee with. A function that reads the stack
//! pointer without ever lowering it uses LLVM's red zone below it, which the
//! LLVM wasm backend limits to `red_zone_bytes`; code generation itself never
//! uses one. A call into a function that runs a check of its own needs no room
//! in its caller, because that check covers it. Code the host provides is the
//! host's own business and needs none either.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const shim_symbols = @import("builtins").shim_symbols;

const WasmModule = @import("WasmModule.zig");
const WasmLinking = @import("WasmLinking.zig");

const Allocator = std.mem.Allocator;

/// The data symbol whose address a stack-bound check compares against.
pub const stack_low_symbol = "__stack_low";
const stack_pointer_symbol = "__stack_pointer";

pub const Error = Allocator.Error || WasmModule.ParseError;

/// Return a copy of `object_bytes`, a relocatable wasm object, in which every
/// stack-bound check's `__stack_low` relocation carries the room that check
/// needs. Returns null when the object has no checks.
pub fn fillCheckRooms(gpa: Allocator, object_bytes: []const u8) Error!?[]u8 {
    var module = try WasmModule.preload(gpa, object_bytes, .relocatable_for_sealing);
    defer module.deinit();

    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const symbols = Symbols.find(&module);
    if (symbols.stack_low == null) return null;

    const signatures = try parseSignatures(arena, object_bytes);
    const relocs = module.reloc_code.entries.items;

    const facts = try arena.alloc(FunctionFacts, module.function_offsets.items.len);
    for (facts, 0..) |*function_facts, local| {
        // The overflow function gives up the stack, leaving the stack pointer
        // below the stack for the host to see, and traps.
        if (symbols.overflow_local == @as(u32, @intCast(local))) {
            function_facts.* = .{ .own_depth = 0, .calls = &.{}, .checked = true, .check_relocs = &.{} };
            continue;
        }
        function_facts.* = try analyzeFunction(arena, &module, signatures, symbols, relocs, @intCast(local));
    }

    const rooms = try computeRooms(arena, &module, facts, relocs);

    var addends = std.AutoHashMap(u32, i32).init(arena);
    for (facts, 0..) |function_facts, local| {
        for (function_facts.check_relocs) |reloc_index| {
            const room = rooms[local];
            const addend = std.math.cast(i32, room) orelse
                base.invariant("wasm stack room invariant violated: check room {d} exceeds the address space", .{room});
            try addends.put(reloc_index, addend);
        }
    }
    if (addends.count() == 0) return null;
    return try rewriteCodeRelocAddends(gpa, object_bytes, &addends);
}

/// The symbols the analysis recognizes: symbol table indices, and the local
/// index of the overflow function a failed check calls.
const Symbols = struct {
    stack_pointer: ?u32,
    stack_low: ?u32,
    overflow_local: ?u32,

    fn find(module: *const WasmModule) Symbols {
        var found = Symbols{ .stack_pointer = null, .stack_low = null, .overflow_local = null };
        for (module.linking.symbol_table.items, 0..) |sym, index| {
            const name = sym.resolveName(module.imports.items, module.global_imports.items, module.table_imports.items) orelse continue;
            switch (sym.kind) {
                .global => if (std.mem.eql(u8, name, stack_pointer_symbol)) {
                    found.stack_pointer = @intCast(index);
                },
                .data => if (sym.isUndefined() and std.mem.eql(u8, name, stack_low_symbol)) {
                    found.stack_low = @intCast(index);
                },
                .function => if (!sym.isUndefined() and std.mem.eql(u8, name, shim_symbols.roc_wasm_stack_overflow)) {
                    found.overflow_local = sym.index - module.import_fn_count;
                },
                .section, .event, .table => {},
            }
        }
        return found;
    }
};

/// A function type's parameter and result counts.
const Signature = struct {
    params: u32,
    results: u32,
};

/// The type section's signatures, read from the object bytes directly because
/// a multi-value result count matters here.
fn parseSignatures(arena: Allocator, bytes: []const u8) Error![]Signature {
    var cursor: usize = 8;
    while (cursor < bytes.len) {
        const id = bytes[cursor];
        cursor += 1;
        const size = try WasmModule.readU32(bytes, &cursor);
        const end = cursor + size;
        if (id != @backingInt(WasmModule.SectionId.type_section)) {
            cursor = end;
            continue;
        }
        const count = try WasmModule.readU32(bytes, &cursor);
        const signatures = try arena.alloc(Signature, count);
        for (signatures) |*signature| {
            cursor += 1; // func type marker
            const params = try WasmModule.readU32(bytes, &cursor);
            cursor += params;
            const results = try WasmModule.readU32(bytes, &cursor);
            cursor += results;
            signature.* = .{ .params = params, .results = results };
        }
        return signatures;
    }
    return &.{};
}

/// Who a call reaches.
const Callee = union(enum) {
    /// A function defined in the object, by local index.
    defined: u32,
    /// Any table function of this type.
    indirect: u32,
};

/// One call and the stack depth below the caller's entry its callee starts at.
const Call = struct {
    callee: Callee,
    depth: u64,
};

/// What one function's own code does with the stack.
const FunctionFacts = struct {
    /// The deepest below its entry stack pointer its own frame reaches.
    own_depth: u64,
    calls: []Call,
    /// Whether a check runs before anything else can, so the function covers
    /// every call chain it starts.
    checked: bool,
    /// Indices into `reloc.CODE` of its stack-bound checks.
    check_relocs: []u32,
};

/// A value on the operand stack.
const Operand = union(enum) {
    /// Not derived from the stack pointer, and not a constant this pass follows.
    unknown,
    /// Derived from the stack pointer: its offset from the entry stack pointer,
    /// the most negative one when it can hold several.
    derived: i64,
    constant: i64,

    fn derivedOffset(self: Operand) ?i64 {
        return switch (self) {
            .derived => |offset| offset,
            .unknown, .constant => null,
        };
    }
};

/// The most negative of two possible stack-pointer offsets.
fn lower(a: ?i64, b: ?i64) ?i64 {
    const x = a orelse return b;
    const y = b orelse return a;
    return @min(x, y);
}

/// The most stack a function that never lowers the stack pointer uses below
/// it: the LLVM wasm backend gives a function without calls whose frame fits
/// in this many bytes that frame below the stack pointer instead of lowering
/// it (`WebAssemblyFrameLowering::RedZoneSize`).
const red_zone_bytes = 128;

/// A local holds whatever was ever assigned to it: the pass treats locals as
/// one value for the whole function, so it repeats until no local changes. A
/// local still changing after this many passes, such as a pointer a loop
/// steps through a frame, is no longer followed: lowering the stack pointer to
/// it would leave the frame's size unknown, which the analysis rejects.
const max_local_passes = 4;

fn analyzeFunction(
    arena: Allocator,
    module: *const WasmModule,
    signatures: []const Signature,
    symbols: Symbols,
    relocs: []const WasmLinking.RelocationEntry,
    local: u32,
) Error!FunctionFacts {
    const code = module.code_bytes.items;
    var cursor: usize = module.function_offsets.items[local];
    const body_len = try WasmModule.readU32(code, &cursor);
    const body_end = cursor + body_len;

    var local_count: u32 = signatures[module.func_type_indices.items[local]].params;
    const group_count = try WasmModule.readU32(code, &cursor);
    for (0..group_count) |_| {
        local_count += try WasmModule.readU32(code, &cursor);
        cursor += 1;
    }
    const locals = try arena.alloc(?i64, local_count);
    @memset(locals, null);
    const followed = try arena.alloc(bool, local_count);
    @memset(followed, true);
    const previous = try arena.alloc(?i64, local_count);

    var walker = Walker{
        .arena = arena,
        .module = module,
        .signatures = signatures,
        .symbols = symbols,
        .relocs = relocs,
        .code = code,
        .locals = locals,
        .followed = followed,
    };
    var pass: usize = 0;
    while (true) : (pass += 1) {
        @memcpy(previous, locals);
        try walker.walk(cursor, body_end);
        if (!walker.locals_changed) break;
        if (pass + 1 < max_local_passes) continue;
        for (followed, previous, locals) |*follow, before, after| {
            if (before != after) follow.* = false;
        }
        @memset(locals, null);
        pass = 0;
    }
    var own_depth: u64 = @intCast(-walker.stack_pointer);
    if (walker.stack_pointer == 0 and walker.reads_stack_pointer) own_depth = red_zone_bytes;
    return .{
        .own_depth = own_depth,
        .calls = try arena.dupe(Call, walker.calls.items),
        .checked = walker.checked,
        .check_relocs = try arena.dupe(u32, walker.check_relocs.items),
    };
}

/// One pass over a function body that follows stack-pointer-derived values.
const Walker = struct {
    arena: Allocator,
    module: *const WasmModule,
    signatures: []const Signature,
    symbols: Symbols,
    relocs: []const WasmLinking.RelocationEntry,
    code: []const u8,
    locals: []?i64,
    /// Which locals the pass still follows; see `max_local_passes`.
    followed: []const bool,

    locals_changed: bool = false,
    reads_stack_pointer: bool = false,
    calls: std.ArrayList(Call) = .empty,
    checked: bool = false,
    check_relocs: std.ArrayList(u32) = .empty,

    stack: std.ArrayList(Operand) = .empty,
    frames: std.ArrayList(Frame) = .empty,
    /// The lowest the stack pointer global has been set to so far, as an
    /// offset from the entry stack pointer. A later restore never raises it,
    /// so every call is assumed to start at the deepest point reached before it.
    stack_pointer: i64 = 0,
    /// Whether a call, branch, or return precedes the current instruction, so
    /// a check here might not run before the function reaches anything else.
    left_straight_line: bool = false,

    const Frame = struct {
        kind: Kind,
        height: usize,
        params: u32,
        results: u32,
        unreachable_rest: bool = false,

        const Kind = enum { function, block, loop, @"if" };
    };

    fn walk(self: *Walker, start: usize, end: usize) Error!void {
        self.locals_changed = false;
        self.reads_stack_pointer = false;
        self.calls.clearRetainingCapacity();
        self.checked = false;
        self.check_relocs.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        self.frames.clearRetainingCapacity();
        self.stack_pointer = 0;
        self.left_straight_line = false;
        try self.frames.append(self.arena, .{ .kind = .function, .height = 0, .params = 0, .results = 0 });

        var cursor = start;
        while (cursor < end) {
            const at = cursor;
            const op = self.code[cursor];
            cursor += 1;
            try self.step(op, at, &cursor);
        }
    }

    fn push(self: *Walker, operand: Operand) Allocator.Error!void {
        try self.stack.append(self.arena, operand);
    }

    fn pop(self: *Walker) Operand {
        const frame = self.frames.items[self.frames.items.len - 1];
        if (self.stack.items.len <= frame.height) {
            // Code after an unconditional branch pops values validation
            // supplies from nothing.
            if (frame.unreachable_rest) return .unknown;
            base.invariant("wasm stack room invariant violated: operand stack underflow", .{});
        }
        return self.stack.pop().?;
    }

    fn popN(self: *Walker, count: u32) void {
        for (0..count) |_| _ = self.pop();
    }

    fn pushUnknown(self: *Walker, count: u32) Allocator.Error!void {
        for (0..count) |_| try self.push(.unknown);
    }

    fn effect(self: *Walker, pops: u32, pushes: u32) Allocator.Error!void {
        self.popN(pops);
        try self.pushUnknown(pushes);
    }

    fn markUnreachable(self: *Walker) void {
        const frame = &self.frames.items[self.frames.items.len - 1];
        self.stack.shrinkRetainingCapacity(frame.height);
        frame.unreachable_rest = true;
    }

    fn relocAt(self: *const Walker, offset: usize) ?u32 {
        var low: usize = 0;
        var high: usize = self.relocs.len;
        while (low < high) {
            const mid = (low + high) / 2;
            const mid_offset = self.relocs[mid].getOffset();
            if (mid_offset == offset) return @intCast(mid);
            if (mid_offset < offset) low = mid + 1 else high = mid;
        }
        return null;
    }

    fn requireRelocAt(self: *const Walker, offset: usize) u32 {
        return self.relocAt(offset) orelse
            base.invariant("wasm stack room invariant violated: unrelocated operand at code offset {d}", .{offset});
    }

    fn isStackPointer(self: *const Walker, operand_offset: usize) bool {
        const stack_pointer = self.symbols.stack_pointer orelse return false;
        return self.relocs[self.requireRelocAt(operand_offset)].getSymbolIndex() == stack_pointer;
    }

    fn relocSymbol(self: *const Walker, operand_offset: usize) WasmLinking.SymInfo {
        return self.module.linking.symbol_table.items[self.relocs[self.requireRelocAt(operand_offset)].getSymbolIndex()];
    }

    fn blockSignature(self: *const Walker, cursor: *usize) Error!Signature {
        switch (self.code[cursor.*]) {
            0x40 => {
                cursor.* += 1;
                return .{ .params = 0, .results = 0 };
            },
            0x7F, 0x7E, 0x7D, 0x7C, 0x7B, 0x70, 0x6F => {
                cursor.* += 1;
                return .{ .params = 0, .results = 1 };
            },
            else => return self.signatures[@intCast(try readS33(self.code, cursor))],
        }
    }

    fn enterBlock(self: *Walker, kind: Frame.Kind, signature: Signature) Allocator.Error!void {
        self.popN(signature.params);
        const height = self.stack.items.len;
        try self.frames.append(self.arena, .{ .kind = kind, .height = height, .params = signature.params, .results = signature.results });
        try self.pushUnknown(signature.params);
    }

    fn leaveBlock(self: *Walker) Allocator.Error!void {
        const frame = self.frames.pop().?;
        self.stack.shrinkRetainingCapacity(frame.height);
        try self.pushUnknown(frame.results);
    }

    fn insideFrame(self: *const Walker, kind: Frame.Kind) bool {
        for (self.frames.items) |frame| if (frame.kind == kind) return true;
        return false;
    }

    fn functionSignature(self: *const Walker, function_index: u32) Signature {
        const import_count = self.module.import_fn_count;
        const type_index = if (function_index < import_count)
            self.module.imports.items[function_index].type_idx
        else
            self.module.func_type_indices.items[function_index - import_count];
        return self.signatures[type_index];
    }

    fn recordCall(self: *Walker, callee: Callee) Allocator.Error!void {
        try self.calls.append(self.arena, .{ .callee = callee, .depth = @intCast(-self.stack_pointer) });
    }

    fn setLocal(self: *Walker, index: u32, operand: Operand) void {
        if (!self.followed[index]) return;
        const merged = lower(self.locals[index], operand.derivedOffset());
        if (merged != self.locals[index]) {
            self.locals[index] = merged;
            self.locals_changed = true;
        }
    }

    fn memarg(self: *Walker, cursor: *usize) Error!u64 {
        const flags = try WasmModule.readU32(self.code, cursor);
        if (flags & 0x40 != 0) _ = try WasmModule.readU32(self.code, cursor); // memory index
        return try readU64(self.code, cursor);
    }

    fn load(self: *Walker, cursor: *usize) Error!void {
        _ = try self.memarg(cursor);
        try self.effect(1, 1);
    }

    fn store(self: *Walker, cursor: *usize) Error!void {
        _ = try self.memarg(cursor);
        try self.effect(2, 0);
    }

    fn arith(self: *Walker, op: u8) Allocator.Error!void {
        const right = self.pop();
        const left = self.pop();
        const result: Operand = switch (op) {
            0x6A => switch (left) { // i32.add
                .derived => |d| switch (right) {
                    .constant => |c| .{ .derived = d + c },
                    .derived, .unknown => .unknown,
                },
                .constant => |c| switch (right) {
                    .derived => |d| .{ .derived = d + c },
                    .constant => |r| .{ .constant = c +% r },
                    .unknown => .unknown,
                },
                .unknown => .unknown,
            },
            0x6B => switch (left) { // i32.sub
                .derived => |d| switch (right) {
                    .constant => |c| .{ .derived = d - c },
                    .derived, .unknown => .unknown,
                },
                .constant => |c| switch (right) {
                    .constant => |r| .{ .constant = c -% r },
                    .derived, .unknown => .unknown,
                },
                .unknown => .unknown,
            },
            // An alignment mask rounds an address down by less than its
            // alignment.
            0x71 => switch (left) { // i32.and
                .derived => |d| switch (right) {
                    .constant => |c| if (c < 0) .{ .derived = d + c + 1 } else .unknown,
                    .derived, .unknown => .unknown,
                },
                .constant, .unknown => .unknown,
            },
            else => unreachable, // step dispatches only add, sub, and and here
        };
        try self.push(result);
    }

    fn step(self: *Walker, op: u8, at: usize, cursor: *usize) Error!void {
        switch (op) {
            0x00 => self.markUnreachable(), // unreachable
            0x01 => {}, // nop
            0x02, 0x03 => { // block, loop
                const signature = try self.blockSignature(cursor);
                try self.enterBlock(if (op == 0x02) .block else .loop, signature);
            },
            0x04 => { // if
                const signature = try self.blockSignature(cursor);
                _ = self.pop();
                try self.enterBlock(.@"if", signature);
            },
            0x05 => { // else
                const frame = &self.frames.items[self.frames.items.len - 1];
                self.stack.shrinkRetainingCapacity(frame.height);
                frame.unreachable_rest = false;
                try self.pushUnknown(frame.params);
            },
            0x0B => { // end
                if (self.frames.items.len == 1) {
                    _ = self.frames.pop();
                    return;
                }
                try self.leaveBlock();
            },
            0x0C => { // br
                _ = try WasmModule.readU32(self.code, cursor);
                self.left_straight_line = true;
                self.markUnreachable();
            },
            0x0D => { // br_if
                _ = try WasmModule.readU32(self.code, cursor);
                self.left_straight_line = true;
                _ = self.pop();
            },
            0x0E => { // br_table
                const count = try WasmModule.readU32(self.code, cursor);
                for (0..count + 1) |_| _ = try WasmModule.readU32(self.code, cursor);
                self.left_straight_line = true;
                _ = self.pop();
                self.markUnreachable();
            },
            0x0F => { // return
                self.left_straight_line = true;
                self.markUnreachable();
            },
            0x10, 0x12 => { // call, return_call
                const operand = cursor.*;
                _ = try WasmModule.readU32(self.code, cursor);
                const sym = self.relocSymbol(operand);
                if (sym.kind != .function) base.invariant("wasm stack room invariant violated: call through a non-function symbol", .{});
                const signature = self.functionSignature(sym.index);
                self.left_straight_line = true;
                self.popN(signature.params);
                // A function the host provides is the host's own business.
                if (!sym.isUndefined()) try self.recordCall(.{ .defined = sym.index - self.module.import_fn_count });
                if (op == 0x12) self.markUnreachable() else try self.pushUnknown(signature.results);
            },
            0x11, 0x13 => { // call_indirect, return_call_indirect
                const operand = cursor.*;
                _ = try WasmModule.readU32(self.code, cursor);
                _ = try WasmModule.readU32(self.code, cursor); // table
                const type_index = self.relocs[self.requireRelocAt(operand)].getSymbolIndex();
                const signature = self.signatures[type_index];
                self.left_straight_line = true;
                _ = self.pop(); // table slot
                self.popN(signature.params);
                try self.recordCall(.{ .indirect = type_index });
                if (op == 0x13) self.markUnreachable() else try self.pushUnknown(signature.results);
            },
            0x1A => _ = self.pop(), // drop
            0x1B, 0x1C => { // select, select t*
                if (op == 0x1C) {
                    const count = try WasmModule.readU32(self.code, cursor);
                    cursor.* += count;
                }
                _ = self.pop();
                const b = self.pop().derivedOffset();
                const a = self.pop().derivedOffset();
                try self.push(if (lower(a, b)) |offset| .{ .derived = offset } else .unknown);
            },
            0x20 => { // local.get
                const index = try WasmModule.readU32(self.code, cursor);
                try self.push(if (self.locals[index]) |offset| .{ .derived = offset } else .unknown);
            },
            0x21 => self.setLocal(try WasmModule.readU32(self.code, cursor), self.pop()), // local.set
            0x22 => { // local.tee
                const operand = self.pop();
                self.setLocal(try WasmModule.readU32(self.code, cursor), operand);
                try self.push(operand);
            },
            0x23 => { // global.get
                const operand = cursor.*;
                _ = try WasmModule.readU32(self.code, cursor);
                if (!self.isStackPointer(operand)) return self.push(.unknown);
                self.reads_stack_pointer = true;
                try self.push(.{ .derived = self.stack_pointer });
            },
            0x24 => { // global.set
                const operand = cursor.*;
                _ = try WasmModule.readU32(self.code, cursor);
                const value = self.pop();
                if (!self.isStackPointer(operand)) return;
                const offset = value.derivedOffset() orelse
                    base.invariant("wasm stack room invariant violated: the stack pointer is set to a value not derived from it at code offset {d}", .{at});
                if (offset < self.stack_pointer and self.insideFrame(.loop)) {
                    base.invariant("wasm stack room invariant violated: the stack grows inside a loop", .{});
                }
                self.stack_pointer = @min(self.stack_pointer, offset);
            },
            0x25 => { // table.get
                _ = try WasmModule.readU32(self.code, cursor);
                try self.effect(1, 1);
            },
            0x26 => { // table.set
                _ = try WasmModule.readU32(self.code, cursor);
                try self.effect(2, 0);
            },
            0x28...0x35 => try self.load(cursor),
            0x36...0x3E => try self.store(cursor),
            0x3F => { // memory.size
                _ = try WasmModule.readU32(self.code, cursor);
                try self.push(.unknown);
            },
            0x40 => { // memory.grow
                _ = try WasmModule.readU32(self.code, cursor);
                try self.effect(1, 1);
            },
            0x41 => { // i32.const
                const operand = cursor.*;
                const value = try WasmModule.readI32(self.code, cursor);
                if (self.relocAt(operand)) |reloc_index| {
                    if (self.symbols.stack_low) |stack_low| {
                        if (self.relocs[reloc_index].getSymbolIndex() == stack_low) {
                            try self.check_relocs.append(self.arena, reloc_index);
                            if (!self.left_straight_line and !self.insideFrame(.@"if")) self.checked = true;
                        }
                    }
                    try self.push(.unknown);
                    return;
                }
                try self.push(.{ .constant = value });
            },
            0x42 => { // i64.const
                _ = try readI64(self.code, cursor);
                try self.push(.unknown);
            },
            0x43 => { // f32.const
                cursor.* += 4;
                try self.push(.unknown);
            },
            0x44 => { // f64.const
                cursor.* += 8;
                try self.push(.unknown);
            },
            0x45, 0x50 => try self.effect(1, 1), // eqz
            0x46...0x4F, 0x51...0x66 => try self.effect(2, 1), // comparisons
            0x67...0x69, 0x79...0x7B, 0x8B...0x91, 0x99...0x9F, 0xA7...0xC4 => try self.effect(1, 1), // unary operators and conversions
            0x6A, 0x6B, 0x71 => try self.arith(op), // i32.add, i32.sub, i32.and
            0x6C...0x70, 0x72...0x78, 0x7C...0x8A, 0x92...0x98, 0xA0...0xA6 => try self.effect(2, 1), // binary operators
            0xD0 => { // ref.null
                cursor.* += 1;
                try self.push(.unknown);
            },
            0xD1 => try self.effect(1, 1), // ref.is_null
            0xD2 => { // ref.func
                _ = try WasmModule.readU32(self.code, cursor);
                try self.push(.unknown);
            },
            0xFC => try self.stepMisc(cursor),
            0xFD => try self.stepSimd(cursor),
            else => base.invariant("wasm stack room invariant violated: unknown opcode 0x{x} at code offset {d}", .{ op, at }),
        }
    }

    fn stepMisc(self: *Walker, cursor: *usize) Error!void {
        const sub = try WasmModule.readU32(self.code, cursor);
        switch (sub) {
            0...7 => try self.effect(1, 1), // trunc_sat
            8 => { // memory.init
                _ = try WasmModule.readU32(self.code, cursor);
                _ = try WasmModule.readU32(self.code, cursor);
                self.popN(3);
            },
            9, 13 => _ = try WasmModule.readU32(self.code, cursor), // data.drop, elem.drop
            10 => { // memory.copy
                _ = try WasmModule.readU32(self.code, cursor);
                _ = try WasmModule.readU32(self.code, cursor);
                self.popN(3);
            },
            11 => { // memory.fill
                _ = try WasmModule.readU32(self.code, cursor);
                self.popN(3);
            },
            12, 14 => { // table.init, table.copy
                _ = try WasmModule.readU32(self.code, cursor);
                _ = try WasmModule.readU32(self.code, cursor);
                try self.effect(3, 0);
            },
            15 => { // table.grow
                _ = try WasmModule.readU32(self.code, cursor);
                try self.effect(2, 1);
            },
            16 => { // table.size
                _ = try WasmModule.readU32(self.code, cursor);
                try self.push(.unknown);
            },
            17 => { // table.fill
                _ = try WasmModule.readU32(self.code, cursor);
                try self.effect(3, 0);
            },
            else => base.invariant("wasm stack room invariant violated: unknown 0xFC opcode {d}", .{sub}),
        }
    }

    fn stepSimd(self: *Walker, cursor: *usize) Error!void {
        const sub = try WasmModule.readU32(self.code, cursor);
        switch (sub) {
            0...10, 92, 93 => try self.load(cursor), // loads, splat and zero loads
            11 => try self.store(cursor), // v128.store
            12 => { // v128.const
                cursor.* += 16;
                try self.push(.unknown);
            },
            13 => { // i8x16.shuffle
                cursor.* += 16;
                try self.effect(2, 1);
            },
            21, 22, 24, 25, 27, 29, 31, 33 => { // extract_lane
                cursor.* += 1;
                try self.effect(1, 1);
            },
            23, 26, 28, 30, 32, 34 => { // replace_lane
                cursor.* += 1;
                try self.effect(2, 1);
            },
            84...87 => { // load_lane
                _ = try self.memarg(cursor);
                cursor.* += 1;
                try self.effect(2, 1);
            },
            88...91 => { // store_lane
                _ = try self.memarg(cursor);
                cursor.* += 1;
                try self.effect(2, 0);
            },
            // bitselect, relaxed madd/nmadd/laneselect/dot_add
            82, 261...268, 275 => try self.effect(3, 1),
            // splats, unary lane operations, any_true/all_true/bitmask,
            // conversions, and relaxed truncations
            15...20, 77, 83, 94...100, 103...106, 116, 117, 122, 124...129, 131, 132, 135...138, 148, 160, 161, 163, 164, 167...170, 192, 193, 195, 196, 199...202, 224, 225, 227, 236, 237, 239, 248...255, 257...260 => try self.effect(1, 1),
            // swizzle, comparisons, bitwise and arithmetic binary operators,
            // shifts, narrowing, extended multiplication, dot products, and
            // relaxed swizzle/min/max/q15mulr/dot
            14, 35...76, 78...81, 101, 102, 107...115, 118...121, 123, 130, 133, 134, 139...147, 149...153, 155...159, 171...174, 177, 181...186, 188...191, 203...206, 209, 213...223, 228...235, 240...247, 256, 269...274 => try self.effect(2, 1),
            else => base.invariant("wasm stack room invariant violated: unknown 0xFD opcode {d}", .{sub}),
        }
    }
};

/// The room each function's checks need: the deepest its own code and every
/// chain of unchecked callees reach below its entry stack pointer. A cycle of
/// unchecked functions has no bound unless none of them has a frame, in which
/// case the stack is the same height at every pass around it.
fn computeRooms(
    arena: Allocator,
    module: *const WasmModule,
    facts: []const FunctionFacts,
    relocs: []const WasmLinking.RelocationEntry,
) Allocator.Error![]u64 {
    const function_count = facts.len;
    const import_count = module.import_fn_count;

    // Table functions by type, the possible targets of an indirect call.
    var table_by_type = std.AutoHashMap(u32, std.ArrayList(u32)).init(arena);
    var in_table = try arena.alloc(bool, function_count);
    @memset(in_table, false);
    for ([_][]const WasmLinking.RelocationEntry{ relocs, module.reloc_data.entries.items }) |entries| {
        for (entries) |entry| {
            const index_entry = switch (entry) {
                .index => |index_entry| index_entry,
                .offset => continue,
            };
            switch (index_entry.type_id) {
                .table_index_sleb, .table_index_i32, .table_index_rel_sleb => {},
                .function_index_leb, .type_index_leb, .global_index_leb, .event_index_leb, .global_index_i32, .table_number_leb => continue,
            }
            const sym = module.linking.symbol_table.items[index_entry.symbol_index];
            if (sym.kind != .function or sym.isUndefined()) continue;
            const local = sym.index - import_count;
            if (in_table[local]) continue;
            in_table[local] = true;
            const type_index = module.func_type_indices.items[local];
            const entry_list = try table_by_type.getOrPut(type_index);
            if (!entry_list.found_existing) entry_list.value_ptr.* = .empty;
            try entry_list.value_ptr.append(arena, local);
        }
    }

    // The call graph between unchecked edges: a call into a checked function
    // ends a chain, since that function's own check covers what follows.
    var succ_starts = try arena.alloc(u32, function_count + 1);
    var succs = std.ArrayList(u32).empty;
    var edge_depths = std.ArrayList(u64).empty;
    for (facts, 0..) |function_facts, local| {
        succ_starts[local] = @intCast(succs.items.len);
        for (function_facts.calls) |call| {
            switch (call.callee) {
                .defined => |callee| if (!facts[callee].checked) {
                    try succs.append(arena, callee);
                    try edge_depths.append(arena, call.depth);
                },
                .indirect => |type_index| if (table_by_type.get(type_index)) |targets| {
                    for (targets.items) |callee| if (!facts[callee].checked) {
                        try succs.append(arena, callee);
                        try edge_depths.append(arena, call.depth);
                    };
                },
            }
        }
    }
    succ_starts[function_count] = @intCast(succs.items.len);

    const preds = try collections.csr.transpose(arena, succ_starts, succs.items);
    const components = try collections.scc.compute(arena, succ_starts, succs.items, preds.starts, preds.targets);

    // Components are numbered so no edge leads into a lower-numbered one, so
    // the highest-numbered come first: every callee's room is known before
    // its callers need it.
    const rooms = try arena.alloc(u64, function_count);
    var component = components.count();
    while (component > 0) {
        component -= 1;
        const members = components.membersOf(component);
        var recursive = members.len > 1;
        for (members) |member| {
            for (succs.items[succ_starts[member]..succ_starts[member + 1]]) |callee| {
                if (callee == member) recursive = true;
            }
        }

        var room: u64 = 0;
        for (members) |member| {
            const function_facts = facts[member];
            if (recursive and (function_facts.own_depth != 0 or hasFramedEdge(succs.items, edge_depths.items, succ_starts, member, components.component_of, component))) {
                base.invariant("wasm stack room invariant violated: function {d} grows the stack on a call cycle without a check", .{member + import_count});
            }
            room = @max(room, function_facts.own_depth);
            for (succs.items[succ_starts[member]..succ_starts[member + 1]], edge_depths.items[succ_starts[member]..succ_starts[member + 1]]) |callee, depth| {
                if (components.component_of[callee] == component) continue;
                room = @max(room, depth + rooms[callee]);
            }
        }
        for (members) |member| rooms[member] = room;
    }
    return rooms;
}

/// Whether `member` calls into its own component with a frame on the stack.
fn hasFramedEdge(
    succs: []const u32,
    edge_depths: []const u64,
    succ_starts: []const u32,
    member: u32,
    component_of: []const u32,
    component: usize,
) bool {
    for (succs[succ_starts[member]..succ_starts[member + 1]], edge_depths[succ_starts[member]..succ_starts[member + 1]]) |callee, depth| {
        if (component_of[callee] == component and depth != 0) return true;
    }
    return false;
}

/// Re-encode the object's `reloc.CODE` section with the given addends,
/// keyed by entry index.
fn rewriteCodeRelocAddends(gpa: Allocator, bytes: []const u8, addends: *const std.AutoHashMap(u32, i32)) Error![]u8 {
    var cursor: usize = 8;
    while (cursor < bytes.len) {
        const section_start = cursor;
        const id = bytes[cursor];
        cursor += 1;
        const size = try WasmModule.readU32(bytes, &cursor);
        const body_start = cursor;
        const section_end = cursor + size;
        cursor = section_end;
        if (id != @backingInt(WasmModule.SectionId.custom_section)) continue;

        var name_cursor = body_start;
        const name_len = try WasmModule.readU32(bytes, &name_cursor);
        const name = bytes[name_cursor..][0..name_len];
        if (!std.mem.eql(u8, name, "reloc.CODE")) continue;
        var entry_cursor = name_cursor + name_len;
        const target_section = try WasmModule.readU32(bytes, &entry_cursor);
        const count = try WasmModule.readU32(bytes, &entry_cursor);

        var body = std.ArrayList(u8).empty;
        defer body.deinit(gpa);
        try WasmModule.leb128WriteU32(gpa, &body, name_len);
        try body.appendSlice(gpa, name);
        try WasmModule.leb128WriteU32(gpa, &body, target_section);
        try WasmModule.leb128WriteU32(gpa, &body, count);
        for (0..count) |index| {
            const entry = try WasmLinking.RelocationEntry.parse(bytes, &entry_cursor);
            switch (entry) {
                .index => |index_entry| {
                    try body.append(gpa, @backingInt(index_entry.type_id));
                    try WasmModule.leb128WriteU32(gpa, &body, index_entry.offset);
                    try WasmModule.leb128WriteU32(gpa, &body, index_entry.symbol_index);
                },
                .offset => |offset_entry| {
                    try body.append(gpa, @backingInt(offset_entry.type_id));
                    try WasmModule.leb128WriteU32(gpa, &body, offset_entry.offset);
                    try WasmModule.leb128WriteU32(gpa, &body, offset_entry.symbol_index);
                    try WasmModule.leb128WriteI32(gpa, &body, addends.get(@intCast(index)) orelse offset_entry.addend);
                },
            }
        }

        var out = std.ArrayList(u8).empty;
        errdefer out.deinit(gpa);
        try out.appendSlice(gpa, bytes[0..section_start]);
        try out.append(gpa, id);
        try WasmModule.leb128WriteU32(gpa, &out, @intCast(body.items.len));
        try out.appendSlice(gpa, body.items);
        try out.appendSlice(gpa, bytes[section_end..]);
        return try out.toOwnedSlice(gpa);
    }
    base.invariant("wasm stack room invariant violated: an object with checks has no reloc.CODE section", .{});
}

fn readS33(bytes: []const u8, cursor: *usize) Error!i64 {
    var result: i64 = 0;
    var shift: u6 = 0;
    while (true) {
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const byte = bytes[cursor.*];
        cursor.* += 1;
        result |= @as(i64, byte & 0x7F) << shift;
        shift += 7;
        if (byte & 0x80 == 0) {
            if (shift < 64 and byte & 0x40 != 0) result |= @as(i64, -1) << shift;
            return result;
        }
        if (shift >= 35) return error.InvalidSection;
    }
}

fn readI64(bytes: []const u8, cursor: *usize) Error!i64 {
    var result: i64 = 0;
    var shift: u7 = 0;
    while (true) {
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const byte = bytes[cursor.*];
        cursor.* += 1;
        if (shift < 64) result |= @as(i64, byte & 0x7F) << @intCast(shift);
        shift += 7;
        if (byte & 0x80 == 0) {
            if (shift < 64 and byte & 0x40 != 0) result |= @as(i64, -1) << @intCast(shift);
            return result;
        }
        if (shift >= 70) return error.InvalidSection;
    }
}

fn readU64(bytes: []const u8, cursor: *usize) Error!u64 {
    var result: u64 = 0;
    var shift: u7 = 0;
    while (true) {
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const byte = bytes[cursor.*];
        cursor.* += 1;
        if (shift < 64) result |= @as(u64, byte & 0x7F) << @intCast(shift);
        shift += 7;
        if (byte & 0x80 == 0) return result;
        if (shift >= 70) return error.InvalidSection;
    }
}
