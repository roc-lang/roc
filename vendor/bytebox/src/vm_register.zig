const std = @import("std");
const assert = std.debug.assert;
const AllocError = std.mem.Allocator.Error;

const builtin = @import("builtin");

const common = @import("common.zig");
const StableArray = common.StableArray;

const opcodes = @import("opcode.zig");
const Opcode = opcodes.Opcode;
const WasmOpcode = opcodes.WasmOpcode;

const def = @import("definition.zig");
pub const i8x16 = def.i8x16;
pub const u8x16 = def.u8x16;
pub const i16x8 = def.i16x8;
pub const u16x8 = def.u16x8;
pub const i32x4 = def.i32x4;
pub const u32x4 = def.u32x4;
pub const i64x2 = def.i64x2;
pub const u64x2 = def.u64x2;
pub const f32x4 = def.f32x4;
pub const f64x2 = def.f64x2;
pub const v128 = def.v128;
const BlockImmediates = def.BlockImmediates;
const BranchTableImmediates = def.BranchTableImmediates;
const CallIndirectImmediates = def.CallIndirectImmediates;
const ConstantExpression = def.ConstantExpression;
const DataDefinition = def.DataDefinition;
const ElementDefinition = def.ElementDefinition;
const ElementMode = def.ElementMode;
const FunctionDefinition = def.FunctionDefinition;
const FunctionExport = def.FunctionExport;
const FunctionHandle = def.FunctionHandle;
const FunctionHandleType = def.FunctionHandleType;
const FunctionTypeDefinition = def.FunctionTypeDefinition;
const GlobalDefinition = def.GlobalDefinition;
const GlobalMut = def.GlobalMut;
const IfImmediates = def.IfImmediates;
const ImportNames = def.ImportNames;
const Instruction = def.Instruction;
const Limits = def.Limits;
const MemoryDefinition = def.MemoryDefinition;
const MemoryOffsetAndLaneImmediates = def.MemoryOffsetAndLaneImmediates;
const ModuleDefinition = def.ModuleDefinition;
const NameCustomSection = def.NameCustomSection;
const TableDefinition = def.TableDefinition;
const TablePairImmediates = def.TablePairImmediates;
const Val = def.Val;
const ValType = def.ValType;
const TaggedVal = def.TaggedVal;
const FuncRef = def.FuncRef;

const inst = @import("instance.zig");
const VM = inst.VM;
const InstantiateError = inst.InstantiateError;
const TrapError = inst.TrapError;
const ModuleInstance = inst.ModuleInstance;
const InvokeOpts = inst.InvokeOpts;
const ResumeInvokeOpts = inst.ResumeInvokeOpts;
const DebugTrapInstructionMode = inst.DebugTrapInstructionMode;
const ModuleInstantiateOpts = inst.ModuleInstantiateOpts;

const INVALID_INSTRUCTION_INDEX: u32 = std.math.maxInt(u32);

// High-level strategy:
// 1. Transform the ModuleDefinition's bytecode into a sea-of-nodes type of IR.
// 2. Perform constant folding, and other peephole optimizations.
// 3. Perform register allocation
// 4. Generate new bytecode
// 5. Implement the runtime instructions for the register-based bytecode

const IRNode = struct {
    opcode: Opcode,
    is_phi: bool,
    instruction_index: u32,
    edges_in: ?[*]*IRNode,
    edges_in_count: u32,
    edges_out: ?[*]*IRNode,
    edges_out_count: u32,

    fn createWithInstruction(mir: *ModuleIR, instruction_index: u32) AllocError!*IRNode {
        const node: *IRNode = mir.ir.addOne() catch return AllocError.OutOfMemory;
        node.* = IRNode{
            .opcode = mir.module_def.code.instructions.items[instruction_index].opcode,
            .is_phi = false,
            .instruction_index = instruction_index,
            .edges_in = null,
            .edges_in_count = 0,
            .edges_out = null,
            .edges_out_count = 0,
        };
        return node;
    }

    fn createStandalone(mir: *ModuleIR, opcode: Opcode) AllocError!*IRNode {
        const node: *IRNode = mir.ir.addOne() catch return AllocError.OutOfMemory;
        node.* = IRNode{
            .opcode = opcode,
            .is_phi = false,
            .instruction_index = INVALID_INSTRUCTION_INDEX,
            .edges_in = null,
            .edges_in_count = 0,
            .edges_out = null,
            .edges_out_count = 0,
        };
        return node;
    }

    fn createPhi(mir: *ModuleIR) AllocError!*IRNode {
        const node: *IRNode = mir.ir.addOne() catch return AllocError.OutOfMemory;
        node.* = IRNode{
            .opcode = .Invalid,
            .is_phi = true,
            .instruction_index = 0,
            .edges_in = null,
            .edges_in_count = 0,
            .edges_out = null,
            .edges_out_count = 0,
        };
        return node;
    }

    fn deinit(node: IRNode, allocator: std.mem.Allocator) void {
        if (node.edges_in) |e| allocator.free(e[0..node.edges_in_count]);
        if (node.edges_out) |e| allocator.free(e[0..node.edges_out_count]);
    }

    fn instruction(node: IRNode, module_def: ModuleDefinition) ?*Instruction {
        return if (node.instruction_index != INVALID_INSTRUCTION_INDEX)
            &module_def.code.instructions.items[node.instruction_index]
        else
            null;
    }

    fn edgesIn(node: IRNode) []*IRNode {
        return if (node.edges_in) |e| e[0..node.edges_in_count] else &[0]*IRNode{};
    }

    fn edgesOut(node: IRNode) []*IRNode {
        return if (node.edges_out) |e| e[0..node.edges_out_count] else &[0]*IRNode{};
    }

    const EdgeDirection = enum {
        In,
        Out,
    };

    fn pushEdges(node: *IRNode, comptime direction: EdgeDirection, edges: []*IRNode, allocator: std.mem.Allocator) AllocError!void {
        const existing = if (direction == .In) node.edgesIn() else node.edgesOut();
        var new = try allocator.alloc(*IRNode, existing.len + edges.len);
        @memcpy(new[0..existing.len], existing);
        @memcpy(new[existing.len .. existing.len + edges.len], edges);
        if (existing.len > 0) {
            allocator.free(existing);
        }
        switch (direction) {
            .In => {
                node.edges_in = new.ptr;
                node.edges_in_count = @intCast(new.len);
            },
            .Out => {
                node.edges_out = new.ptr;
                node.edges_out_count = @intCast(new.len);
            },
        }

        if (node.is_phi) {
            std.debug.assert(node.edges_in_count <= 2);
            std.debug.assert(node.edges_out_count <= 1);
        }
    }

    fn hasSideEffects(node: *IRNode) bool {
        // We define a side-effect instruction as any that could affect the Store or control flow
        return switch (node.opcode) {
            .Call => true,
            else => false,
        };
    }

    fn isFlowControl(node: *IRNode) bool {
        return switch (node.opcode) {
            .If,
            .IfNoElse,
            .Else,
            .Return,
            .Branch,
            .Branch_If,
            .Branch_Table,
            => true,
            else => false,
        };
    }

    fn needsRegisterSlot(node: *IRNode) bool {
        // TODO fill this out
        return switch (node.opcode) {
            .If,
            .IfNoElse,
            .Else,
            .Return,
            .Branch,
            .Branch_If,
            .Branch_Table,
            => false,
            else => true,
        };
    }

    fn numRegisterSlots(node: *IRNode) u32 {
        return switch (node.opcode) {
            .If,
            .IfNoElse,
            .Else,
            .Return,
            .Branch,
            .Branch_If,
            .Branch_Table,
            => 0,
            else => 1,
        };
    }

    // a node that has no out edges to instructions with side effects or control flow
    fn isIsland(node: *IRNode, unvisited: *std.array_list.Managed(*IRNode)) AllocError!bool {
        if (node.opcode == .Return) {
            return false;
        }

        unvisited.clearRetainingCapacity();

        for (node.edgesOut()) |edge| {
            try unvisited.append(edge);
        }

        while (unvisited.items.len > 0) {
            var next: *IRNode = unvisited.pop();
            if (next.opcode == .Return or next.hasSideEffects() or node.isFlowControl()) {
                return false;
            }
            for (next.edgesOut()) |edge| {
                try unvisited.append(edge);
            }
        }

        unvisited.clearRetainingCapacity();

        return true;
    }
};

const RegisterSlots = struct {
    const Slot = struct {
        node: ?*IRNode,
        prev: ?u32,
    };

    slots: std.array_list.Managed(Slot),
    last_free: ?u32,

    fn init(allocator: std.mem.Allocator) RegisterSlots {
        return RegisterSlots{
            .slots = std.array_list.Managed(Slot).init(allocator),
            .last_free = null,
        };
    }

    fn deinit(self: *RegisterSlots) void {
        self.slots.deinit();
    }

    fn alloc(self: *RegisterSlots, node: *IRNode) AllocError!u32 {
        if (self.last_free == null) {
            self.last_free = @intCast(self.slots.items.len);
            try self.slots.append(Slot{
                .node = null,
                .prev = null,
            });
        }

        const index = self.last_free.?;
        var slot: *Slot = &self.slots.items[index];
        self.last_free = slot.prev;
        slot.node = node;
        slot.prev = null;

        std.debug.print("pushed node {*} with opcode {} to index {}\n", .{ node, node.opcode, index });

        return index;
    }

    fn freeAt(self: *RegisterSlots, node: *IRNode, index: u32) void {
        var succes: bool = false;
        var slot: *Slot = &self.slots.items[index];
        if (slot.node == node) {
            slot.node = null;
            slot.prev = self.last_free;
            self.last_free = index;
            succes = true;
        }

        std.debug.print("attempting to free node {*} with opcode {} at index {}: {}\n", .{ node, node.opcode, index, succes });
    }
};

const IRFunction = struct {
    definition_index: usize,
    ir_root: *IRNode,

    register_map: std.AutoHashMap(*const IRNode, u32),

    fn init(definition_index: u32, ir_root: *IRNode, allocator: std.mem.Allocator) IRFunction {
        return IRFunction{
            .definition_index = definition_index,
            .ir_root = ir_root,
            .register_map = std.AutoHashMap(*const IRNode, u32).init(allocator),
        };
    }

    fn deinit(self: *IRFunction) void {
        self.register_map.deinit();
    }

    fn definition(func: IRFunction, module_def: ModuleDefinition) *FunctionDefinition {
        return &module_def.functions.items[func.definition_index];
    }

    fn regalloc(func: *IRFunction, allocator: std.mem.Allocator) AllocError!void {
        std.debug.assert(func.ir_root.opcode == .Return); // TODO need to update other places in the code to ensure this is a thing

        var slots = RegisterSlots.init(allocator);
        defer slots.deinit();

        var visit_queue = std.array_list.Managed(*IRNode).init(allocator);
        defer visit_queue.deinit();
        try visit_queue.append(func.ir_root);

        var visited = std.AutoHashMap(*IRNode, void).init(allocator);
        defer visited.deinit();

        while (visit_queue.items.len > 0) {
            var node: *IRNode = visit_queue.orderedRemove(0); // visit the graph in breadth-first order (FIFO queue)
            try visited.put(node, {});

            // mark output node slots as free - this is safe because the dataflow graph flows one way and the
            // output can't be reused higher up in the graph
            for (node.edgesOut()) |output_node| {
                if (func.register_map.get(output_node)) |index| {
                    slots.freeAt(output_node, index);
                }
            }

            // allocate slots for this instruction
            // TODO handle multiple output slots (e.g. results of a function call)
            if (node.needsRegisterSlot()) {
                const index: u32 = try slots.alloc(node);
                try func.register_map.put(node, index);
            }

            // add inputs to the FIFO visit queue
            for (node.edgesIn()) |input_node| {
                if (visited.contains(input_node) == false) {
                    try visit_queue.append(input_node);
                }
            }
        }
    }

    fn codegen(func: *IRFunction, instructions: *std.array_list.Managed(RegInstruction), module_def: ModuleDefinition, allocator: std.mem.Allocator) AllocError!void {
        // walk the graph in breadth-first order

        // when a node is visited, emit its instruction
        // reverse the instructions array when finished (alternatively just emit in reverse order if we have the node count from regalloc)

        const start_instruction_offset = instructions.items.len;

        var visit_queue = std.array_list.Managed(*IRNode).init(allocator);
        defer visit_queue.deinit();
        try visit_queue.append(func.ir_root);

        var visited = std.AutoHashMap(*IRNode, void).init(allocator);
        defer visited.deinit();

        while (visit_queue.items.len > 0) {
            var node: *IRNode = visit_queue.orderedRemove(0); // visit the graph in breadth-first order (FIFO queue)

            // only emit an instruction once all its out edges have been visited - this ensures all dependent instructions
            // will be executed after this one
            var all_out_edges_visited: bool = true;
            for (node.edgesOut()) |output_node| {
                if (visited.contains(output_node) == false) {
                    all_out_edges_visited = false;
                    break;
                }
            }

            if (all_out_edges_visited) {
                try visited.put(node, {});

                instructions.append(RegInstruction{
                    .registerSlotOffset = if (func.register_map.get(node)) |slot_index| slot_index else 0,
                    .opcode = node.opcode,
                    .immediate = node.instruction(module_def).?.immediate,
                });
            }

            for (node.edgesIn()) |input_node| {
                if (!visited.contains(input_node)) { // TODO do we need this?
                    try visit_queue.append(input_node);
                }
            }
        }

        const end_instruction_offset = instructions.items.len;
        const emitted_instructions = instructions.items[start_instruction_offset..end_instruction_offset];

        std.mem.reverse(RegInstruction, emitted_instructions);
    }

    fn dumpVizGraph(func: IRFunction, path: []u8, module_def: ModuleDefinition, allocator: std.mem.Allocator) !void {
        var graph_txt = std.array_list.Managed(u8).init(allocator);
        defer graph_txt.deinit();
        try graph_txt.ensureTotalCapacity(1024 * 16);

        var writer = graph_txt.writer();
        _ = try writer.write("digraph {\n");

        var nodes = std.array_list.Managed(*const IRNode).init(allocator);
        defer nodes.deinit();
        try nodes.ensureTotalCapacity(1024);
        nodes.appendAssumeCapacity(func.ir_root);

        var visited = std.AutoHashMap(*IRNode, void).init(allocator);
        defer visited.deinit();
        try visited.put(func.ir_root, {});

        while (nodes.items.len > 0) {
            const n: *const IRNode = nodes.pop();
            const opcode: Opcode = n.opcode;
            const instruction = n.instruction(module_def);

            var label_buffer: [256]u8 = undefined;
            const label = switch (opcode) {
                .I32_Const => std.fmt.bufPrint(&label_buffer, ": {}", .{instruction.?.immediate.ValueI32}) catch unreachable,
                .I64_Const => std.fmt.bufPrint(&label_buffer, ": {}", .{instruction.?.immediate.ValueI64}) catch unreachable,
                .F32_Const => std.fmt.bufPrint(&label_buffer, ": {}", .{instruction.?.immediate.ValueF32}) catch unreachable,
                .F64_Const => std.fmt.bufPrint(&label_buffer, ": {}", .{instruction.?.immediate.ValueF64}) catch unreachable,
                .Call => std.fmt.bufPrint(&label_buffer, ": func {}", .{instruction.?.immediate.Index}) catch unreachable,
                .Local_Get, .Local_Set, .Local_Tee => std.fmt.bufPrint(&label_buffer, ": {}", .{instruction.?.immediate.Index}) catch unreachable,
                else => &[0]u8{},
            };

            var register_buffer: [64]u8 = undefined;
            const register = blk: {
                if (func.register_map.get(n)) |slot| {
                    break :blk std.fmt.bufPrint(&register_buffer, " @reg {}", .{slot}) catch unreachable;
                } else {
                    break :blk &[0]u8{};
                }
            };

            try writer.print("\"{*}\" [label=\"{}{s}{s}\"]\n", .{ n, opcode, label, register });

            for (n.edgesOut()) |e| {
                try writer.print("\"{*}\" -> \"{*}\"\n", .{ n, e });

                if (!visited.contains(e)) {
                    try nodes.append(e);
                    try visited.put(e, {});
                }
            }

            for (n.edgesIn()) |e| {
                if (!visited.contains(e)) {
                    try nodes.append(e);
                    try visited.put(e, {});
                }
            }
        }

        _ = try writer.write("}\n");

        try std.fs.cwd().writeFile(path, graph_txt.items);
    }
};

const ModuleIR = struct {
    const BlockStack = struct {
        const Block = struct {
            node_start_index: u32,
            continuation: u32, // in instruction index space
            phi_nodes: []*IRNode,
        };

        nodes: std.array_list.Managed(*IRNode),
        blocks: std.array_list.Managed(Block),
        phi_nodes: std.array_list.Managed(*IRNode),

        // const ContinuationType = enum {
        //     .Normal,
        //     .Loop,
        // };

        fn init(allocator: std.mem.Allocator) BlockStack {
            return BlockStack{
                .nodes = std.array_list.Managed(*IRNode).init(allocator),
                .blocks = std.array_list.Managed(Block).init(allocator),
                .phi_nodes = std.array_list.Managed(*IRNode).init(allocator),
            };
        }

        fn deinit(self: BlockStack) void {
            self.nodes.deinit();
            self.blocks.deinit();
        }

        fn pushBlock(self: *BlockStack, continuation: u32) AllocError!void {
            try self.blocks.append(Block{
                .node_start_index = @intCast(self.nodes.items.len),
                .continuation = continuation,
                .phi_nodes = &[_]*IRNode{},
            });
        }

        fn pushBlockWithPhi(self: *BlockStack, continuation: u32, phi_nodes: []*IRNode) AllocError!void {
            const start_slice_index = self.phi_nodes.items.len;
            try self.phi_nodes.appendSlice(phi_nodes);

            try self.blocks.append(Block{
                .node_start_index = @intCast(self.nodes.items.len),
                .continuation = continuation,
                .phi_nodes = self.phi_nodes.items[start_slice_index..],
            });
        }

        fn pushNode(self: *BlockStack, node: *IRNode) AllocError!void {
            try self.nodes.append(node);
        }

        fn popBlock(self: *BlockStack) void {
            const block: Block = self.blocks.pop();

            std.debug.assert(block.node_start_index <= self.nodes.items.len);

            // should never grow these arrays
            self.nodes.resize(block.node_start_index) catch unreachable;
            self.phi_nodes.resize(self.phi_nodes.items.len - block.phi_nodes.len) catch unreachable;
        }

        fn currentBlockNodes(self: *BlockStack) []*IRNode {
            // std.debug.print(">>>>>>>> num block: {}\n", .{self.blocks.items.len});
            const index: u32 = self.blocks.items[self.blocks.items.len - 1].node_start_index;
            return self.nodes.items[index..];
        }

        fn reset(self: *BlockStack) void {
            self.nodes.clearRetainingCapacity();
            self.blocks.clearRetainingCapacity();
        }
    };

    const IntermediateCompileData = struct {
        const UniqueValueToIRNodeMap = std.HashMap(TaggedVal, *IRNode, TaggedVal.HashMapContext, std.hash_map.default_max_load_percentage);

        const PendingContinuationEdge = struct {
            continuation: u32,
            node: *IRNode,
        };

        allocator: std.mem.Allocator,

        // all_nodes: std.array_list.Managed(*IRNode),

        blocks: BlockStack,

        // This stack is a record of the nodes to push values onto the stack. If an instruction would push
        // multiple values onto the stack, it would be in this list as many times as values it pushed. Note
        // that we don't have to do any type checking here because the module has already been validated.
        value_stack: std.array_list.Managed(*IRNode),

        // records the current block continuation
        // label_continuations: std.array_list.Managed(u32),

        pending_continuation_edges: std.array_list.Managed(PendingContinuationEdge),

        // when hitting an unconditional control transfer, we need to mark the rest of the stack values as unreachable just like in validation
        is_unreachable: bool,

        // This is a bit weird - since the Local_* instructions serve to just manipulate the locals into the stack,
        // we need a way to represent what's in the locals slot as an SSA node. This array lets us do that. We also
        // reuse the Local_Get instructions to indicate the "initial value" of the slot. Since our IRNode only stores
        // indices to instructions, we'll just lazily set these when they're fetched for the first time.
        locals: std.array_list.Managed(?*IRNode),

        // Lets us collapse multiple const IR nodes with the same type/value into a single one
        unique_constants: UniqueValueToIRNodeMap,

        scratch_node_list_1: std.array_list.Managed(*IRNode),
        scratch_node_list_2: std.array_list.Managed(*IRNode),

        fn init(allocator: std.mem.Allocator) IntermediateCompileData {
            return IntermediateCompileData{
                .allocator = allocator,
                // .all_nodes = std.array_list.Managed(*IRNode).init(allocator),
                .blocks = BlockStack.init(allocator),
                .value_stack = std.array_list.Managed(*IRNode).init(allocator),
                // .label_continuations = std.array_list.Managed(u32).init(allocator),
                .pending_continuation_edges = std.array_list.Managed(PendingContinuationEdge).init(allocator),
                .is_unreachable = false,
                .locals = std.array_list.Managed(?*IRNode).init(allocator),
                .unique_constants = UniqueValueToIRNodeMap.init(allocator),
                .scratch_node_list_1 = std.array_list.Managed(*IRNode).init(allocator),
                .scratch_node_list_2 = std.array_list.Managed(*IRNode).init(allocator),
            };
        }

        fn warmup(self: *IntermediateCompileData, func_def: FunctionDefinition, module_def: ModuleDefinition) AllocError!void {
            try self.locals.appendNTimes(null, func_def.numParamsAndLocals(module_def));
            try self.scratch_node_list_1.ensureTotalCapacity(4096);
            try self.scratch_node_list_2.ensureTotalCapacity(4096);
            // try self.label_continuations.append(func_def.continuation);
            self.is_unreachable = false;
        }

        fn reset(self: *IntermediateCompileData) void {
            // self.all_nodes.clearRetainingCapacity();
            self.blocks.reset();
            self.value_stack.clearRetainingCapacity();
            // self.label_continuations.clearRetainingCapacity();
            self.pending_continuation_edges.clearRetainingCapacity();
            self.locals.clearRetainingCapacity();
            self.unique_constants.clearRetainingCapacity();
            self.scratch_node_list_1.clearRetainingCapacity();
            self.scratch_node_list_2.clearRetainingCapacity();
        }

        fn deinit(self: *IntermediateCompileData) void {
            // self.all_nodes.deinit();
            self.blocks.deinit();
            self.value_stack.deinit();
            // self.label_continuations.deinit();
            self.pending_continuation_edges.deinit();
            self.locals.deinit();
            self.unique_constants.deinit();
            self.scratch_node_list_1.deinit();
            self.scratch_node_list_2.deinit();
        }

        fn popPushValueStackNodes(self: *IntermediateCompileData, node: *IRNode, num_consumed: usize, num_pushed: usize) AllocError!void {
            if (self.is_unreachable) {
                return;
            }

            var edges_buffer: [8]*IRNode = undefined; // 8 should be more stack slots than any one instruction can pop
            std.debug.assert(num_consumed <= edges_buffer.len);

            const edges = edges_buffer[0..num_consumed];
            for (edges) |*e| {
                e.* = self.value_stack.pop();
            }
            try node.pushEdges(.In, edges, self.allocator);
            for (edges) |e| {
                var consumer_edges = [_]*IRNode{node};
                try e.pushEdges(.Out, &consumer_edges, self.allocator);
            }
            try self.value_stack.appendNTimes(node, num_pushed);
        }

        fn foldConstant(self: *IntermediateCompileData, mir: *ModuleIR, comptime valtype: ValType, instruction_index: u32, instruction: Instruction) AllocError!*IRNode {
            var val: TaggedVal = undefined;
            val.type = valtype;
            val.val = switch (valtype) {
                .I32 => Val{ .I32 = instruction.immediate.ValueI32 },
                .I64 => Val{ .I64 = instruction.immediate.ValueI64 },
                .F32 => Val{ .F32 = instruction.immediate.ValueF32 },
                .F64 => Val{ .F64 = instruction.immediate.ValueF64 },
                .V128 => blk: {
                    const v: v128 = mir.module_def.v128_immediates.items[instruction.immediate.Index];
                    break :blk Val{ .V128 = v };
                },
                else => @compileError("Unsupported const instruction"),
            };

            const res = try self.unique_constants.getOrPut(val);
            if (res.found_existing == false) {
                const node = try IRNode.createWithInstruction(mir, instruction_index);
                res.value_ptr.* = node;
            }
            if (self.is_unreachable == false) {
                try self.value_stack.append(res.value_ptr.*);
            }
            return res.value_ptr.*;
        }

        fn addPendingEdgeLabel(self: *IntermediateCompileData, node: *IRNode, label_id: u32) !void {
            const last_block_index = self.blocks.blocks.items.len - 1;
            const continuation: u32 = self.blocks.blocks.items[last_block_index - label_id].continuation;
            try self.pending_continuation_edges.append(PendingContinuationEdge{
                .node = node,
                .continuation = continuation,
            });
        }

        fn addPendingEdgeContinuation(self: *IntermediateCompileData, node: *IRNode, continuation: u32) !void {
            try self.pending_continuation_edges.append(PendingContinuationEdge{
                .node = node,
                .continuation = continuation,
            });
        }
    };

    allocator: std.mem.Allocator,
    module_def: *const ModuleDefinition,
    functions: std.array_list.Managed(IRFunction),
    ir: StableArray(IRNode),

    // instructions: std.array_list.Managed(RegInstruction),

    fn init(allocator: std.mem.Allocator, module_def: *const ModuleDefinition) ModuleIR {
        return ModuleIR{
            .allocator = allocator,
            .module_def = module_def,
            .functions = std.array_list.Managed(IRFunction).init(allocator),
            .ir = StableArray(IRNode).init(1024 * 1024 * 8),
        };
    }

    fn deinit(mir: *ModuleIR) void {
        for (mir.functions.items) |*func| {
            func.deinit();
        }
        mir.functions.deinit();
        for (mir.ir.items) |node| {
            node.deinit(mir.allocator);
        }
        mir.ir.deinit();
    }

    fn compile(mir: *ModuleIR) AllocError!void {
        var compile_data = IntermediateCompileData.init(mir.allocator);
        defer compile_data.deinit();

        for (0..mir.module_def.functions.items.len) |i| {
            std.debug.print("mir.module_def.functions.items.len: {}, i: {}\n\n", .{ mir.module_def.functions.items.len, i });
            try mir.compileFunc(i, &compile_data);

            compile_data.reset();
        }
    }

    fn compileFunc(mir: *ModuleIR, index: usize, compile_data: *IntermediateCompileData) AllocError!void {
        const UniqueValueToIRNodeMap = std.HashMap(TaggedVal, *IRNode, TaggedVal.HashMapContext, std.hash_map.default_max_load_percentage);

        const Helpers = struct {
            fn opcodeHasDefaultIRMapping(opcode: Opcode) bool {
                return switch (opcode) {
                    .Noop,
                    .Block,
                    .Loop,
                    .End,
                    .Drop,
                    .I32_Const,
                    .I64_Const,
                    .F32_Const,
                    .F64_Const,
                    .Local_Get,
                    .Local_Set,
                    .Local_Tee,
                    => false,
                    else => true,
                };
            }
        };

        const func: *const FunctionDefinition = &mir.module_def.functions.items[index];
        const func_type: *const FunctionTypeDefinition = func.typeDefinition(mir.module_def.*);

        std.debug.print("compiling func index {}\n", .{index});

        try compile_data.warmup(func.*, mir.module_def.*);

        try compile_data.blocks.pushBlock(func.continuation);

        var locals = compile_data.locals.items; // for convenience later

        // Lets us collapse multiple const IR nodes with the same type/value into a single one
        var unique_constants = UniqueValueToIRNodeMap.init(mir.allocator);
        defer unique_constants.deinit();

        const instructions: []Instruction = func.instructions(mir.module_def.*);
        if (instructions.len == 0) {
            std.log.warn("Skipping function with no instructions (index {}).", .{index});
            return;
        }

        var ir_root: ?*IRNode = null;

        for (instructions, 0..) |instruction, local_instruction_index| {
            const instruction_index: u32 = @intCast(func.instructions_begin + local_instruction_index);

            var node: ?*IRNode = null;
            if (Helpers.opcodeHasDefaultIRMapping(instruction.opcode)) {
                node = try IRNode.createWithInstruction(mir, instruction_index);
            }

            std.debug.print("opcode: {}\n", .{instruction.opcode});

            switch (instruction.opcode) {
                // .Loop => {
                //     instruction.
                // },
                // .If => {},
                .Block => {
                    // compile_data.label_stack += 1;

                    // try compile_data.label_stack.append(node);
                    // try compile_data.label_continuations.append(instruction.immediate.Block.continuation);
                    try compile_data.blocks.pushBlock(instruction.immediate.Block.continuation);
                },
                .Loop => {
                    // compile_data.label_stack += 1;
                    // compile_data.label_stack.append(node);
                    // try compile_data.label_continuations.append(instruction.immediate.Block.continuation);
                    try compile_data.blocks.pushBlock(instruction.immediate.Block.continuation); // TODO record the kind of block so we know this is a loop?
                },
                .If => {
                    var phi_nodes: *std.array_list.Managed(*IRNode) = &compile_data.scratch_node_list_1;
                    defer compile_data.scratch_node_list_1.clearRetainingCapacity();

                    std.debug.assert(phi_nodes.items.len == 0);

                    for (0..instruction.immediate.If.num_returns) |_| {
                        try phi_nodes.append(try IRNode.createPhi(mir));
                    }

                    try compile_data.blocks.pushBlockWithPhi(instruction.immediate.If.end_continuation, phi_nodes.items[0..]);
                    try compile_data.addPendingEdgeContinuation(node.?, instruction.immediate.If.end_continuation + 1);
                    try compile_data.addPendingEdgeContinuation(node.?, instruction.immediate.If.else_continuation);

                    try compile_data.popPushValueStackNodes(node.?, 1, 0);

                    // after the if consumes the value it needs, push the phi nodes on since these will be the return values
                    // of the block
                    try compile_data.value_stack.appendSlice(phi_nodes.items);
                },
                .IfNoElse => {
                    try compile_data.blocks.pushBlock(instruction.immediate.If.end_continuation);
                    try compile_data.addPendingEdgeContinuation(node.?, instruction.immediate.If.end_continuation + 1);
                    try compile_data.addPendingEdgeContinuation(node.?, instruction.immediate.If.else_continuation);
                    try compile_data.popPushValueStackNodes(node.?, 1, 0);

                    // TODO figure out if there needs to be any phi nodes and if so what two inputs they have
                },
                .Else => {
                    try compile_data.addPendingEdgeContinuation(node.?, instruction.immediate.If.end_continuation + 1);
                    try compile_data.addPendingEdgeContinuation(node.?, instruction.immediate.If.else_continuation);

                    // TODO hook up the phi nodes with the stuffs
                },
                .End => {
                    // TODO finish up anything with phi nodes?

                    // the last End opcode returns the values on the stack
                    // if (compile_data.label_continuations.items.len == 1) {
                    if (compile_data.blocks.blocks.items.len == 1) {
                        node = try IRNode.createStandalone(mir, .Return);
                        try compile_data.popPushValueStackNodes(node.?, func_type.getReturns().len, 0);
                        // _ = compile_data.label_continuations.pop();
                    }

                    // At the end of every block, we ensure all nodes with side effects are still in the graph. Order matters
                    // since mutations to the Store or control flow changes must happen in the order of the original instructions.
                    {
                        var nodes_with_side_effects: *std.array_list.Managed(*IRNode) = &compile_data.scratch_node_list_1;
                        defer nodes_with_side_effects.clearRetainingCapacity();

                        const current_block_nodes: []*IRNode = compile_data.blocks.currentBlockNodes();

                        for (current_block_nodes) |block_node| {
                            if (block_node.hasSideEffects() or block_node.isFlowControl()) {
                                try nodes_with_side_effects.append(block_node);
                            }
                        }

                        if (nodes_with_side_effects.items.len >= 2) {
                            var i: i32 = @intCast(nodes_with_side_effects.items.len - 2);
                            while (i >= 0) : (i -= 1) {
                                const ii: u32 = @intCast(i);
                                var node_a: *IRNode = nodes_with_side_effects.items[ii];
                                if (try node_a.isIsland(&compile_data.scratch_node_list_2)) {
                                    var node_b: *IRNode = nodes_with_side_effects.items[ii + 1];

                                    var in_edges = [_]*IRNode{node_b};
                                    try node_a.pushEdges(.Out, &in_edges, compile_data.allocator);

                                    var out_edges = [_]*IRNode{node_a};
                                    try node_b.pushEdges(.In, &out_edges, compile_data.allocator);
                                }
                            }
                        }
                    }

                    compile_data.blocks.popBlock();
                },
                .Branch => {
                    try compile_data.addPendingEdgeLabel(node.?, instruction.immediate.LabelId);
                    compile_data.is_unreachable = true;
                },
                .Branch_If => {
                    try compile_data.popPushValueStackNodes(node.?, 1, 0);
                },
                .Branch_Table => {
                    assert(node != null);

                    try compile_data.popPushValueStackNodes(node.?, 1, 0);

                    // var continuation_edges: std.array_list.Managed(*IRNode).init(allocator);
                    // defer continuation_edges.deinit();

                    const immediates: *const BranchTableImmediates = &mir.module_def.code.branch_table.items[instruction.immediate.Index];

                    try compile_data.addPendingEdgeLabel(node.?, immediates.fallback_id);
                    for (immediates.label_ids.items) |continuation| {
                        try compile_data.addPendingEdgeLabel(node.?, continuation);
                    }

                    compile_data.is_unreachable = true;

                    // try label_ids.append(immediates.fallback_id);
                    // try label_ids.appendSlice(immediates.label_ids.items);

                    // node.pushEdges(.Out, )
                    // TODO need to somehow connect to the various labels it wants to jump to?
                },
                .Return => {
                    try compile_data.popPushValueStackNodes(node.?, func_type.getReturns().len, 0);
                    compile_data.is_unreachable = true;
                },
                .Call => {
                    const calling_func_def: *const FunctionDefinition = &mir.module_def.functions.items[index];
                    const calling_func_type: *const FunctionTypeDefinition = calling_func_def.typeDefinition(mir.module_def.*);
                    const num_returns: usize = calling_func_type.getReturns().len;
                    const num_params: usize = calling_func_type.getParams().len;

                    try compile_data.popPushValueStackNodes(node.?, num_params, num_returns);
                },
                // .Call_Indirect
                .Drop => {
                    if (compile_data.is_unreachable == false) {
                        _ = compile_data.value_stack.pop();
                    }
                },
                .I32_Const => {
                    assert(node == null);
                    node = try compile_data.foldConstant(mir, .I32, instruction_index, instruction);
                },
                .I64_Const => {
                    assert(node == null);
                    node = try compile_data.foldConstant(mir, .I64, instruction_index, instruction);
                },
                .F32_Const => {
                    assert(node == null);
                    node = try compile_data.foldConstant(mir, .F32, instruction_index, instruction);
                },
                .F64_Const => {
                    assert(node == null);
                    node = try compile_data.foldConstant(mir, .F64, instruction_index, instruction);
                },
                .I32_Eq,
                .I32_NE,
                .I32_LT_S,
                .I32_LT_U,
                .I32_GT_S,
                .I32_GT_U,
                .I32_LE_S,
                .I32_LE_U,
                .I32_GE_S,
                .I32_GE_U,
                .I32_Add,
                .I32_Sub,
                .I32_Mul,
                .I32_Div_S,
                .I32_Div_U,
                .I32_Rem_S,
                .I32_Rem_U,
                .I32_And,
                .I32_Or,
                .I32_Xor,
                .I32_Shl,
                .I32_Shr_S,
                .I32_Shr_U,
                .I32_Rotl,
                .I32_Rotr,
                // TODO add a lot more of these simpler opcodes
                => {
                    try compile_data.popPushValueStackNodes(node.?, 2, 1);
                },
                .I32_Eqz,
                .I32_Clz,
                .I32_Ctz,
                .I32_Popcnt,
                .I32_Extend8_S,
                .I32_Extend16_S,
                .I64_Clz,
                .I64_Ctz,
                .I64_Popcnt,
                .F32_Neg,
                .F64_Neg,
                => {
                    try compile_data.popPushValueStackNodes(node.?, 1, 1);
                },
                .Local_Get => {
                    assert(node == null);

                    if (compile_data.is_unreachable == false) {
                        const local: *?*IRNode = &locals[instruction.immediate.Index];
                        if (local.* == null) {
                            local.* = try IRNode.createWithInstruction(mir, instruction_index);
                        }
                        node = local.*;
                        try compile_data.value_stack.append(node.?);
                    }
                },
                .Local_Set => {
                    assert(node == null);

                    if (compile_data.is_unreachable == false) {
                        const n: *IRNode = compile_data.value_stack.pop();
                        locals[instruction.immediate.Index] = n;
                    }
                },
                .Local_Tee => {
                    assert(node == null);
                    if (compile_data.is_unreachable == false) {
                        const n: *IRNode = compile_data.value_stack.items[compile_data.value_stack.items.len - 1];
                        locals[instruction.immediate.Index] = n;
                    }
                },
                else => {
                    std.log.warn("skipping node {}", .{instruction.opcode});
                },
            }

            // resolve any pending continuations with the current node.
            if (node) |current_node| {
                var i: usize = 0;
                while (i < compile_data.pending_continuation_edges.items.len) {
                    var pending: *IntermediateCompileData.PendingContinuationEdge = &compile_data.pending_continuation_edges.items[i];

                    if (pending.continuation == instruction_index) {
                        var out_edges = [_]*IRNode{current_node};
                        try pending.node.pushEdges(.Out, &out_edges, compile_data.allocator);

                        var in_edges = [_]*IRNode{pending.node};
                        try current_node.pushEdges(.In, &in_edges, compile_data.allocator);

                        _ = compile_data.pending_continuation_edges.swapRemove(i);
                    } else {
                        i += 1;
                    }
                }

                // try compile_data.all_nodes.append(current_node);

                try compile_data.blocks.pushNode(current_node);
            }

            // TODO don't assume only one return node - there can be multiple in real functions
            if (node) |n| {
                if (n.opcode == .Return) {
                    std.debug.assert(ir_root == null);
                    ir_root = node;
                }
            }
        }

        // resolve any nodes that have side effects that somehow became isolated
        // TODO will have to stress test this with a bunch of different cases of nodes
        // for (compile_data.all_nodes.items[0 .. compile_data.all_nodes.items.len - 1]) |node| {
        //     if (node.hasSideEffects()) {
        //         if (try node.isIsland(&compile_data.scratch_node_list_1)) {
        //             var last_node: *IRNode = compile_data.all_nodes.items[compile_data.all_nodes.items.len - 1];

        //             var out_edges = [_]*IRNode{last_node};
        //             try node.pushEdges(.Out, &out_edges, compile_data.allocator);

        //             var in_edges = [_]*IRNode{node};
        //             try last_node.pushEdges(.In, &in_edges, compile_data.allocator);
        //         }
        //     }
        // }

        try mir.functions.append(IRFunction.init(
            @intCast(index),
            ir_root.?,
            mir.allocator,
        ));

        try mir.functions.items[mir.functions.items.len - 1].regalloc(mir.allocator);
    }
};

pub const RegisterVM = struct {
    pub fn init(vm: *VM) void {
        _ = vm;
    }

    pub fn deinit(vm: *VM) void {
        _ = vm;
    }

    pub fn instantiate(vm: *VM, module: *ModuleInstance, opts: ModuleInstantiateOpts) InstantiateError!void {
        _ = vm;
        _ = module;
        _ = opts;
        unreachable;
    }

    pub fn invoke(vm: *VM, module: *ModuleInstance, handle: FunctionHandle, params: [*]const Val, returns: [*]Val, opts: InvokeOpts) TrapError!void {
        _ = vm;
        _ = module;
        _ = handle;
        _ = params;
        _ = returns;
        _ = opts;
        unreachable;
    }

    pub fn invokeWithIndex(vm: *VM, module: *ModuleInstance, func_index: usize, params: [*]const Val, returns: [*]Val) TrapError!void {
        _ = vm;
        _ = module;
        _ = func_index;
        _ = params;
        _ = returns;
        unreachable;
    }

    pub fn resumeInvoke(vm: *VM, module: *ModuleInstance, returns: []Val, opts: ResumeInvokeOpts) TrapError!void {
        _ = vm;
        _ = module;
        _ = returns;
        _ = opts;
        unreachable;
    }

    pub fn step(vm: *VM, module: *ModuleInstance, returns: []Val) TrapError!void {
        _ = vm;
        _ = module;
        _ = returns;
        unreachable;
    }

    pub fn setDebugTrap(vm: *VM, module: *ModuleInstance, wasm_address: u32, mode: DebugTrapInstructionMode) AllocError!bool {
        _ = vm;
        _ = module;
        _ = wasm_address;
        _ = mode;
        unreachable;
    }

    pub fn formatBacktrace(vm: *VM, indent: u8, allocator: std.mem.Allocator) anyerror!std.array_list.Managed(u8) {
        _ = vm;
        _ = indent;
        _ = allocator;
        unreachable;
    }

    pub fn findFuncTypeDef(vm: *VM, module: *ModuleInstance, local_func_index: usize) *const FunctionTypeDefinition {
        _ = vm;
        _ = module;
        _ = local_func_index;
        return &dummy_func_type_def;
    }

    pub fn resolveFuncRef(vm: *VM, func: FuncRef) FuncRef {
        _ = vm;
        _ = func;
        return FuncRef.nullRef();
    }

    pub fn compile(vm: *RegisterVM, module_def: ModuleDefinition) AllocError!void {
        var mir = ModuleIR.init(vm.allocator, module_def);
        defer mir.deinit();

        try mir.compile();

        // wasm bytecode -> IR graph -> register-assigned IR graph ->
    }
};

const dummy_func_type_def = FunctionTypeDefinition{
    .types = undefined,
    .num_params = 0,
};

// register instructions get a slice of the overall set of register slots, which are pointers to actual
// registers (?)

const RegInstruction = struct {
    registerSlotOffset: u32, // offset within the function register slot space to start
    opcode: Opcode,
    immediate: def.InstructionImmediates,

    fn numRegisters(self: RegInstruction) u4 {
        switch (self.opcode) {}
    }

    fn registers(self: RegInstruction, register_slice: []Val) []Val {
        return register_slice[self.registerOffset .. self.registerOffset + self.numRegisters()];
    }
};

fn runTestWithViz(wasm_filepath: []const u8, viz_dir: []const u8) !void {
    var allocator = std.testing.allocator;

    var cwd = std.fs.cwd();
    const wasm_data: []u8 = try cwd.readFileAlloc(allocator, wasm_filepath, 1024 * 1024 * 128);
    defer allocator.free(wasm_data);

    const module_def_opts = def.ModuleDefinitionOpts{
        .debug_name = std.fs.path.basename(wasm_filepath),
    };
    var module_def = try ModuleDefinition.create(allocator, module_def_opts);
    defer module_def.destroy();

    try module_def.decode(wasm_data);

    var mir = ModuleIR.init(allocator, module_def);
    defer mir.deinit();
    try mir.compile();
    for (mir.functions.items, 0..) |func, i| {
        var viz_path_buffer: [256]u8 = undefined;
        const viz_path = std.fmt.bufPrint(&viz_path_buffer, "{s}\\viz_{}.txt", .{ viz_dir, i }) catch unreachable;
        std.debug.print("gen graph for func {}\n", .{i});
        try func.dumpVizGraph(viz_path, module_def.*, std.testing.allocator);
    }
}

// test "ir1" {
//     const filename =
//         // \\E:\Dev\zig_projects\bytebox\test\wasm\br_table\br_table.0.wasm
//         \\E:\Dev\zig_projects\bytebox\test\wasm\return\return.0.wasm
//         // \\E:\Dev\third_party\zware\test\fact.wasm
//         // \\E:\Dev\zig_projects\bytebox\test\wasm\i32\i32.0.wasm
//     ;
//     const viz_dir =
//         \\E:\Dev\zig_projects\bytebox\viz
//     ;
//     try runTestWithViz(filename, viz_dir);

//     // var allocator = std.testing.allocator;

//     // var cwd = std.fs.cwd();
//     // var wasm_data: []u8 = try cwd.readFileAlloc(allocator, filename, 1024 * 1024 * 128);
//     // defer allocator.free(wasm_data);

//     // const module_def_opts = def.ModuleDefinitionOpts{
//     //     .debug_name = std.fs.path.basename(filename),
//     // };
//     // var module_def = ModuleDefinition.init(allocator, module_def_opts);
//     // defer module_def.deinit();

//     // try module_def.decode(wasm_data);

//     // var mir = ModuleIR.init(allocator, &module_def);
//     // defer mir.deinit();
//     // try mir.compile();
//     // for (mir.functions.items, 0..) |func, i| {
//     //     var viz_path_buffer: [256]u8 = undefined;
//     //     const path_format =
//     //         \\E:\Dev\zig_projects\bytebox\viz\viz_{}.txt
//     //     ;
//     //     const viz_path = std.fmt.bufPrint(&viz_path_buffer, path_format, .{i}) catch unreachable;
//     //     std.debug.print("gen graph for func {}\n", .{i});
//     //     try func.dumpVizGraph(viz_path, module_def, std.testing.allocator);
//     // }
// }

// test "ir2" {
//     const filename =
//         // \\E:\Dev\zig_projects\bytebox\test\wasm\br_table\br_table.0.wasm
//         \\E:\Dev\zig_projects\bytebox\test\reg\add.wasm
//         // \\E:\Dev\third_party\zware\test\fact.wasm
//         // \\E:\Dev\zig_projects\bytebox\test\wasm\i32\i32.0.wasm
//     ;
//     const viz_dir =
//         \\E:\Dev\zig_projects\bytebox\test\reg\
//     ;
//     try runTestWithViz(filename, viz_dir);
// }
