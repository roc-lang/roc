const std = @import("std");
const bytebox = @import("bytebox");
const config = bytebox.config;
const ValType = bytebox.ValType;
const Val = bytebox.Val;
const TaggedVal = bytebox.TaggedVal;
const VmType = bytebox.VmType;
const v128 = bytebox.v128;
const i8x16 = bytebox.i8x16;
const i16x8 = bytebox.i16x8;
const i32x4 = bytebox.i32x4;
const i64x2 = bytebox.i64x2;
const f32x4 = bytebox.f32x4;
const f64x2 = bytebox.f64x2;

const print = std.debug.print;

var g_verbose_logging = false;

fn logVerbose(comptime msg: []const u8, params: anytype) void {
    if (g_verbose_logging) {
        print(msg, params);
    }
}

const TestSuiteError = error{
    Fail,
};

const CommandType = enum {
    DecodeModule,
    Register,
    AssertReturn,
    AssertTrap,
    AssertMalformed,
    AssertInvalid,
    AssertUnlinkable,
    AssertUninstantiable,
};

const ActionType = enum {
    Invocation,
    Get,
};

const Action = struct {
    type: ActionType,
    module: []const u8,
    field: []const u8,
    args: std.array_list.Managed(LaneTypedVal),
};

const BadModuleError = struct {
    module: []const u8,
    expected_error: []const u8,
};

const CommandDecodeModule = struct {
    module_filename: []const u8,
    module_name: []const u8,
};

const CommandRegister = struct {
    module_filename: []const u8,
    module_name: []const u8,
    import_name: []const u8,
};

const CommandAssertReturn = struct {
    action: Action,
    expected_returns: ?std.array_list.Managed(LaneTypedVal),
};

const CommandAssertTrap = struct {
    action: Action,
    expected_error: []const u8,
};

const CommandAssertMalformed = struct {
    err: BadModuleError,
};

const CommandAssertInvalid = struct {
    err: BadModuleError,
};

const CommandAssertUnlinkable = struct {
    err: BadModuleError,
};

const CommandAssertUninstantiable = struct {
    err: BadModuleError,
};

const Command = union(CommandType) {
    DecodeModule: CommandDecodeModule,
    Register: CommandRegister,
    AssertReturn: CommandAssertReturn,
    AssertTrap: CommandAssertTrap,
    AssertMalformed: CommandAssertMalformed,
    AssertInvalid: CommandAssertInvalid,
    AssertUnlinkable: CommandAssertUnlinkable,
    AssertUninstantiable: CommandAssertUninstantiable,

    fn getCommandName(self: *const Command) []const u8 {
        return switch (self.*) {
            .DecodeModule => "decode_module",
            .Register => "register",
            .AssertReturn => "assert_return",
            .AssertTrap => "assert_trap",
            .AssertMalformed => "assert_malformed",
            .AssertInvalid => "assert_invalid",
            .AssertUnlinkable => "assert_unlinkable",
            .AssertUninstantiable => "assert_uninstantiable",
        };
    }

    fn getModuleFilename(self: *const Command) []const u8 {
        return switch (self.*) {
            .DecodeModule => |c| c.module_filename,
            .Register => |c| c.module_filename,
            else => return getModuleName(self),
        };
    }

    fn getModuleName(self: *const Command) []const u8 {
        return switch (self.*) {
            .DecodeModule => |c| c.module_name,
            .Register => |c| c.module_name,
            .AssertReturn => |c| c.action.module,
            .AssertTrap => |c| c.action.module,
            .AssertMalformed => |c| c.err.module,
            .AssertInvalid => |c| c.err.module,
            .AssertUnlinkable => |c| c.err.module,
            .AssertUninstantiable => |c| c.err.module,
        };
    }

    fn deinitAction(action: *Action, allocator: std.mem.Allocator) void {
        allocator.free(action.module);
        allocator.free(action.field);
        action.args.deinit();
    }

    fn deinitBadModuleError(err: *BadModuleError, allocator: std.mem.Allocator) void {
        allocator.free(err.module);
        allocator.free(err.expected_error);
    }

    fn deinit(self: *Command, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .DecodeModule => |*v| {
                allocator.free(v.module_filename);
                allocator.free(v.module_name);
            },
            .Register => |*v| {
                allocator.free(v.module_filename);
                allocator.free(v.module_name);
                allocator.free(v.import_name);
            },
            .AssertReturn => |*v| {
                deinitAction(&v.action, allocator);
                if (v.expected_returns) |returns| {
                    returns.deinit();
                }
            },
            .AssertTrap => |*v| {
                deinitAction(&v.action, allocator);
                allocator.free(v.expected_error);
            },
            .AssertMalformed => |*v| {
                deinitBadModuleError(&v.err, allocator);
            },
            .AssertInvalid => |*v| {
                deinitBadModuleError(&v.err, allocator);
            },
            .AssertUnlinkable => |*v| {
                deinitBadModuleError(&v.err, allocator);
            },
            .AssertUninstantiable => |*v| {
                deinitBadModuleError(&v.err, allocator);
            },
        }
    }
};

fn strcmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

fn parseVal(obj: std.json.ObjectMap) !TaggedVal {
    const Helpers = struct {
        fn parseI8(str: []const u8) !i8 {
            return std.fmt.parseInt(i8, str, 10) catch @as(i8, @bitCast(try std.fmt.parseInt(u8, str, 10)));
        }

        fn parseI16(str: []const u8) !i16 {
            return std.fmt.parseInt(i16, str, 10) catch @as(i16, @bitCast(try std.fmt.parseInt(u16, str, 10)));
        }

        fn parseI32(str: []const u8) !i32 {
            return std.fmt.parseInt(i32, str, 10) catch @as(i32, @bitCast(try std.fmt.parseInt(u32, str, 10)));
        }

        fn parseI64(str: []const u8) !i64 {
            return std.fmt.parseInt(i64, str, 10) catch @as(i64, @bitCast(try std.fmt.parseInt(u64, str, 10)));
        }

        fn parseF32(str: []const u8) !f32 {
            if (std.mem.startsWith(u8, str, "nan:")) {
                return std.math.nan(f32); // don't differentiate between arithmetic/canonical nan
            } else {
                const int = try std.fmt.parseInt(u32, str, 10);
                return @as(f32, @bitCast(int));
            }
        }

        fn parseF64(str: []const u8) !f64 {
            if (std.mem.startsWith(u8, str, "nan:")) {
                return std.math.nan(f64); // don't differentiate between arithmetic/canonical nan
            } else {
                const int = try std.fmt.parseInt(u64, str, 10);
                return @as(f64, @bitCast(int));
            }
        }

        fn parseValuesIntoVec(comptime T: type, json_strings: []std.json.Value) !v128 {
            std.debug.assert(json_strings.len * @sizeOf(T) == @sizeOf(v128));
            const num_items = @sizeOf(v128) / @sizeOf(T);
            var parsed_values: [num_items]T = undefined;

            const parse_func = switch (T) {
                i8 => parseI8,
                i16 => parseI16,
                i32 => parseI32,
                i64 => parseI64,
                f32 => parseF32,
                f64 => parseF64,
                else => unreachable,
            };

            for (&parsed_values, 0..) |*v, i| {
                v.* = try parse_func(json_strings[i].string);
            }

            const parsed_bytes = std.mem.sliceAsBytes(&parsed_values);
            var bytes: [16]u8 = undefined;
            @memcpy(&bytes, parsed_bytes);
            return std.mem.bytesToValue(v128, &bytes);
        }
    };

    const json_type = obj.get("type").?;
    const json_value = obj.get("value").?;

    if (strcmp("i32", json_type.string)) {
        const int = try Helpers.parseI32(json_value.string);
        return TaggedVal{ .type = .I32, .val = Val{ .I32 = int } };
    } else if (strcmp("i64", json_type.string)) {
        const int = try Helpers.parseI64(json_value.string);
        return TaggedVal{ .type = .I64, .val = Val{ .I64 = int } };
    } else if (strcmp("f32", json_type.string)) {
        const float: f32 = try Helpers.parseF32(json_value.string);
        return TaggedVal{ .type = .F32, .val = Val{ .F32 = float } };
    } else if (strcmp("f64", json_type.string)) {
        const float: f64 = try Helpers.parseF64(json_value.string);
        return TaggedVal{ .type = .F64, .val = Val{ .F64 = float } };
    } else if (strcmp("v128", json_type.string)) {
        const json_lane_type = obj.get("lane_type").?;
        var vec: v128 = undefined;
        if (strcmp("i8", json_lane_type.string)) {
            vec = try Helpers.parseValuesIntoVec(i8, json_value.array.items);
        } else if (strcmp("i16", json_lane_type.string)) {
            vec = try Helpers.parseValuesIntoVec(i16, json_value.array.items);
        } else if (strcmp("i32", json_lane_type.string)) {
            vec = try Helpers.parseValuesIntoVec(i32, json_value.array.items);
        } else if (strcmp("i64", json_lane_type.string)) {
            vec = try Helpers.parseValuesIntoVec(i64, json_value.array.items);
        } else if (strcmp("f32", json_lane_type.string)) {
            vec = try Helpers.parseValuesIntoVec(f32, json_value.array.items);
        } else if (strcmp("f64", json_lane_type.string)) {
            vec = try Helpers.parseValuesIntoVec(f64, json_value.array.items);
        } else {
            unreachable;
        }

        return TaggedVal{ .type = .V128, .val = Val{ .V128 = vec } };
    } else if (strcmp("externref", json_type.string)) {
        const val: Val = blk: {
            if (strcmp("null", json_value.string)) {
                break :blk Val.nullRef(ValType.ExternRef).?;
            } else {
                const int = try std.fmt.parseInt(u32, json_value.string, 10);
                break :blk Val{ .ExternRef = int };
            }
        };
        return TaggedVal{ .type = .ExternRef, .val = val };
    } else if (strcmp("funcref", json_type.string) and strcmp("null", json_value.string)) {
        return TaggedVal{ .type = .FuncRef, .val = Val.nullRef(ValType.FuncRef).? };
    } else {
        print("Failed to parse value of type '{s}' with value '{s}'\n", .{ json_type.string, json_value.string });
    }

    unreachable;
}

const V128LaneType = enum(u8) {
    I8x16,
    I16x8,
    I32x4,
    I64x2,
    F32x4,
    F64x2,

    fn fromString(str: []const u8) V128LaneType {
        if (strcmp("i8", str)) {
            return .I8x16;
        } else if (strcmp("i16", str)) {
            return .I16x8;
        } else if (strcmp("i32", str)) {
            return .I32x4;
        } else if (strcmp("i64", str)) {
            return .I64x2;
        } else if (strcmp("f32", str)) {
            return .F32x4;
        } else if (strcmp("f64", str)) {
            return .F64x2;
        }

        unreachable;
    }
};

const LaneTypedVal = struct {
    v: TaggedVal,
    lane_type: V128LaneType, // only valid when v contains a V128

    fn toValArrayList(typed: []const LaneTypedVal, allocator: std.mem.Allocator) !std.array_list.Managed(Val) {
        var vals = std.array_list.Managed(Val).init(allocator);
        try vals.ensureTotalCapacityPrecise(typed.len);
        for (typed) |v| {
            try vals.append(v.v.val);
        }
        return vals;
    }
};

fn parseLaneTypedVal(obj: std.json.ObjectMap) !LaneTypedVal {
    const v: TaggedVal = try parseVal(obj);
    var lane_type = V128LaneType.I8x16;
    if (v.type == .V128) {
        const json_lane_type = obj.get("lane_type").?;
        lane_type = V128LaneType.fromString(json_lane_type.string);
    }

    return LaneTypedVal{
        .v = v,
        .lane_type = lane_type,
    };
}

fn isSameError(err: anyerror, err_string: []const u8) bool {
    return switch (err) {
        bytebox.MalformedError.MalformedMagicSignature => strcmp(err_string, "magic header not detected"),
        bytebox.MalformedError.MalformedUnexpectedEnd => strcmp(err_string, "unexpected end") or
            strcmp(err_string, "unexpected end of section or function") or
            strcmp(err_string, "length out of bounds") or
            strcmp(err_string, "malformed section id"),
        bytebox.MalformedError.MalformedUnsupportedWasmVersion => strcmp(err_string, "unknown binary version"),
        bytebox.MalformedError.MalformedSectionId => strcmp(err_string, "malformed section id"),
        bytebox.MalformedError.MalformedTypeSentinel => strcmp(err_string, "integer representation too long") or strcmp(err_string, "integer too large"),
        bytebox.MalformedError.MalformedLEB128 => strcmp(err_string, "integer representation too long") or strcmp(err_string, "integer too large"),
        bytebox.MalformedError.MalformedMissingZeroByte => strcmp(err_string, "zero flag expected"),
        bytebox.MalformedError.MalformedTooManyLocals => strcmp(err_string, "too many locals"),
        bytebox.MalformedError.MalformedFunctionCodeSectionMismatch => strcmp(err_string, "function and code section have inconsistent lengths"),
        bytebox.MalformedError.MalformedMissingDataCountSection => strcmp(err_string, "data count section required") or strcmp(err_string, "unknown data segment"),
        bytebox.MalformedError.MalformedDataCountMismatch => strcmp(err_string, "data count and data section have inconsistent lengths"),
        bytebox.MalformedError.MalformedDataType => strcmp(err_string, "integer representation too long") or strcmp(err_string, "integer too large"),
        bytebox.MalformedError.MalformedIllegalOpcode => strcmp(err_string, "illegal opcode") or strcmp(err_string, "integer representation too long"),
        bytebox.MalformedError.MalformedReferenceType => strcmp(err_string, "malformed reference type"),
        bytebox.MalformedError.MalformedSectionSizeMismatch => strcmp(err_string, "section size mismatch") or
            strcmp(err_string, "malformed section id") or
            strcmp(err_string, "function and code section have inconsistent lengths") or // this one is a bit of a hack to resolve custom.8.wasm
            strcmp(err_string, "zero flag expected"), // the memory64 binary tests don't seem to be up to date with the reference types spec
        bytebox.MalformedError.MalformedInvalidImport => strcmp(err_string, "malformed import kind"),
        bytebox.MalformedError.MalformedLimits => strcmp(err_string, "integer too large") or strcmp(err_string, "integer representation too long") or strcmp(err_string, "malformed limits flags"),
        bytebox.MalformedError.MalformedMultipleStartSections => strcmp(err_string, "multiple start sections") or
            strcmp(err_string, "junk after last section"),
        bytebox.MalformedError.MalformedElementType => strcmp(err_string, "integer representation too long") or strcmp(err_string, "integer too large"),
        bytebox.MalformedError.MalformedUTF8Encoding => strcmp(err_string, "malformed UTF-8 encoding"),
        bytebox.MalformedError.MalformedMutability => strcmp(err_string, "malformed mutability"),
        // ValidationTypeMismatch: result arity handles select.2.wasm which is the exact same binary as select.1.wasm but the test expects a different error :/
        bytebox.ValidationError.ValidationTypeMismatch => strcmp(err_string, "type mismatch") or strcmp(err_string, "invalid result arity"),
        bytebox.ValidationError.ValidationTypeMustBeNumeric => strcmp(err_string, "type mismatch"),
        bytebox.ValidationError.ValidationUnknownType => strcmp(err_string, "unknown type"),
        bytebox.ValidationError.ValidationUnknownFunction => std.mem.startsWith(u8, err_string, "unknown function"),
        bytebox.ValidationError.ValidationUnknownGlobal => std.mem.startsWith(u8, err_string, "unknown global"),
        bytebox.ValidationError.ValidationUnknownLocal => std.mem.startsWith(u8, err_string, "unknown local"),
        bytebox.ValidationError.ValidationUnknownTable => std.mem.startsWith(u8, err_string, "unknown table") or
            strcmp(err_string, "zero flag expected"), // the memory64 binary tests don't seem to be up to date with the reference types spec
        bytebox.ValidationError.ValidationUnknownMemory => std.mem.startsWith(u8, err_string, "unknown memory"),
        bytebox.ValidationError.ValidationUnknownElement => strcmp(err_string, "unknown element") or std.mem.startsWith(u8, err_string, "unknown elem segment"),
        bytebox.ValidationError.ValidationUnknownData => strcmp(err_string, "unknown data") or std.mem.startsWith(u8, err_string, "unknown data segment"),
        bytebox.ValidationError.ValidationTypeStackHeightMismatch => strcmp(err_string, "type mismatch"),
        bytebox.ValidationError.ValidationBadAlignment => strcmp(err_string, "alignment must not be larger than natural"),
        bytebox.ValidationError.ValidationUnknownLabel => strcmp(err_string, "unknown label"),
        bytebox.ValidationError.ValidationImmutableGlobal => strcmp(err_string, "global is immutable"),
        bytebox.ValidationError.ValidationBadConstantExpression => strcmp(err_string, "constant expression required") or strcmp(err_string, "type mismatch"),
        bytebox.ValidationError.ValidationGlobalReferencingMutableGlobal => strcmp(err_string, "constant expression required"),
        bytebox.ValidationError.ValidationUnknownBlockTypeIndex => strcmp(err_string, "type mismatch") or
            strcmp(err_string, "unexpected end"), // bit of a hack for binary.166.wasm
        bytebox.ValidationError.ValidationSelectArity => strcmp(err_string, "invalid result arity"),
        bytebox.ValidationError.ValidationMultipleMemories => strcmp(err_string, "multiple memories"),
        bytebox.ValidationError.ValidationMemoryInvalidMaxLimit => strcmp(err_string, "size minimum must not be greater than maximum"),
        bytebox.ValidationError.ValidationMemoryMaxPagesExceeded => strcmp(err_string, "memory size must be at most 65536 pages (4GiB)"),
        bytebox.ValidationError.ValidationConstantExpressionGlobalMustBeImport => strcmp(err_string, "unknown global"),
        bytebox.ValidationError.ValidationConstantExpressionGlobalMustBeImmutable => strcmp(err_string, "constant expression required"),
        bytebox.ValidationError.ValidationStartFunctionType => strcmp(err_string, "start function"),
        bytebox.ValidationError.ValidationLimitsMinMustNotBeLargerThanMax => strcmp(err_string, "size minimum must not be greater than maximum"),
        bytebox.ValidationError.ValidationConstantExpressionTypeMismatch => strcmp(err_string, "type mismatch") or strcmp(err_string, "constant expression required"),
        bytebox.ValidationError.ValidationDuplicateExportName => strcmp(err_string, "duplicate export name"),
        bytebox.ValidationError.ValidationFuncRefUndeclared => strcmp(err_string, "undeclared function reference"),
        bytebox.ValidationError.ValidationIfElseMismatch => strcmp(err_string, "END opcode expected"),
        bytebox.ValidationError.ValidationInvalidLaneIndex => strcmp(err_string, "invalid lane index"),

        bytebox.UnlinkableError.UnlinkableUnknownImport => strcmp(err_string, "unknown import"),
        bytebox.UnlinkableError.UnlinkableIncompatibleImportType => strcmp(err_string, "incompatible import type"),

        bytebox.UninstantiableError.UninstantiableOutOfBoundsTableAccess => strcmp(err_string, "out of bounds table access"),
        bytebox.UninstantiableError.UninstantiableOutOfBoundsMemoryAccess => strcmp(err_string, "out of bounds memory access"),

        bytebox.TrapError.TrapIntegerDivisionByZero => strcmp(err_string, "integer divide by zero"),
        bytebox.TrapError.TrapIntegerOverflow => strcmp(err_string, "integer overflow"),
        bytebox.TrapError.TrapIndirectCallTypeMismatch => strcmp(err_string, "indirect call type mismatch"),
        bytebox.TrapError.TrapInvalidIntegerConversion => strcmp(err_string, "invalid conversion to integer"),
        bytebox.TrapError.TrapOutOfBoundsMemoryAccess => strcmp(err_string, "out of bounds memory access"),
        bytebox.TrapError.TrapUndefinedElement => strcmp(err_string, "undefined element"),
        bytebox.TrapError.TrapUninitializedElement => std.mem.startsWith(u8, err_string, "uninitialized element"),
        bytebox.TrapError.TrapOutOfBoundsTableAccess => strcmp(err_string, "out of bounds table access"),
        bytebox.TrapError.TrapStackExhausted => strcmp(err_string, "call stack exhausted"),
        bytebox.TrapError.TrapUnreachable => strcmp(err_string, "unreachable"),

        else => false,
    };
}

fn parseCommands(json_path: []const u8, allocator: std.mem.Allocator) !std.array_list.Managed(Command) {
    const Helpers = struct {
        fn parseAction(json_action: *std.json.Value, fallback_module: []const u8, _allocator: std.mem.Allocator) !Action {
            const json_type = json_action.object.getPtr("type").?;
            var action_type: ActionType = undefined;
            if (strcmp("invoke", json_type.string)) {
                action_type = .Invocation;
            } else if (strcmp("get", json_type.string)) {
                action_type = .Get;
            } else {
                unreachable;
            }

            const json_field = json_action.object.getPtr("field").?;

            const json_args_or_null = json_action.object.getPtr("args");
            var args = std.array_list.Managed(LaneTypedVal).init(_allocator);
            if (json_args_or_null) |json_args| {
                for (json_args.array.items) |item| {
                    const val: LaneTypedVal = try parseLaneTypedVal(item.object);
                    try args.append(val);
                }
            }

            var module: []const u8 = try _allocator.dupe(u8, fallback_module);
            const json_module_or_null = json_action.object.getPtr("module");
            if (json_module_or_null) |json_module| {
                module = try _allocator.dupe(u8, json_module.string);
            }

            return Action{
                .type = action_type,
                .module = module,
                .field = try _allocator.dupe(u8, json_field.string),
                .args = args,
            };
        }

        fn parseBadModuleError(json_command: *const std.json.Value, _allocator: std.mem.Allocator) !BadModuleError {
            const json_filename = json_command.object.get("filename").?;
            const json_expected = json_command.object.get("text").?;

            return BadModuleError{
                .module = try _allocator.dupe(u8, json_filename.string),
                .expected_error = try _allocator.dupe(u8, json_expected.string),
            };
        }
    };

    // print("json_path: {s}\n", .{json_path});
    const json_data = try std.fs.cwd().readFileAlloc(allocator, json_path, 1024 * 1024 * 8);
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_data, .{});

    var fallback_module: []const u8 = "";
    defer allocator.free(fallback_module);

    var commands = std.array_list.Managed(Command).init(allocator);

    const json_commands = parsed.value.object.getPtr("commands").?;
    for (json_commands.array.items) |json_command| {
        const json_command_type = json_command.object.getPtr("type").?;

        if (strcmp("module", json_command_type.string)) {
            const json_filename = json_command.object.getPtr("filename").?;
            const filename: []const u8 = try allocator.dupe(u8, json_filename.string);
            fallback_module = filename;

            var name = try allocator.dupe(u8, filename);
            if (json_command.object.getPtr("name")) |json_module_name| {
                name = try allocator.dupe(u8, json_module_name.string);
            }

            const command = Command{
                .DecodeModule = CommandDecodeModule{
                    .module_filename = try allocator.dupe(u8, filename),
                    .module_name = name,
                },
            };
            try commands.append(command);
        } else if (strcmp("register", json_command_type.string)) {
            const json_as = json_command.object.getPtr("as").?;
            const json_import_name: []const u8 = json_as.string;
            var json_module_name: []const u8 = fallback_module;
            if (json_command.object.getPtr("name")) |json_name| {
                json_module_name = json_name.string;
            }

            // print("json_module_name: {s}, json_import_name: {s}\n", .{ json_module_name, json_import_name });

            const command = Command{
                .Register = CommandRegister{
                    .module_filename = try allocator.dupe(u8, fallback_module),
                    .module_name = try allocator.dupe(u8, json_module_name),
                    .import_name = try allocator.dupe(u8, json_import_name),
                },
            };
            try commands.append(command);
        } else if (strcmp("assert_return", json_command_type.string) or strcmp("action", json_command_type.string)) {
            const json_action = json_command.object.getPtr("action").?;

            const action = try Helpers.parseAction(json_action, fallback_module, allocator);

            var expected_returns_or_null: ?std.array_list.Managed(LaneTypedVal) = null;
            const json_expected_or_null = json_command.object.getPtr("expected");
            if (json_expected_or_null) |json_expected| {
                var expected_returns = std.array_list.Managed(LaneTypedVal).init(allocator);
                for (json_expected.array.items) |item| {
                    try expected_returns.append(try parseLaneTypedVal(item.object));
                }
                expected_returns_or_null = expected_returns;
            }

            const command = Command{
                .AssertReturn = CommandAssertReturn{
                    .action = action,
                    .expected_returns = expected_returns_or_null,
                },
            };
            try commands.append(command);
        } else if (strcmp("assert_trap", json_command_type.string) or strcmp("assert_exhaustion", json_command_type.string)) {
            const json_action = json_command.object.getPtr("action").?;

            const action = try Helpers.parseAction(json_action, fallback_module, allocator);

            const json_text = json_command.object.getPtr("text").?;

            const command = Command{
                .AssertTrap = CommandAssertTrap{
                    .action = action,
                    .expected_error = try allocator.dupe(u8, json_text.string),
                },
            };
            try commands.append(command);
        } else if (strcmp("assert_malformed", json_command_type.string)) {
            const command = Command{
                .AssertMalformed = CommandAssertMalformed{
                    .err = try Helpers.parseBadModuleError(&json_command, allocator),
                },
            };
            if (std.mem.endsWith(u8, command.AssertMalformed.err.module, ".wasm")) {
                try commands.append(command);
            }
        } else if (strcmp("assert_invalid", json_command_type.string)) {
            const command = Command{
                .AssertInvalid = CommandAssertInvalid{
                    .err = try Helpers.parseBadModuleError(&json_command, allocator),
                },
            };
            if (std.mem.endsWith(u8, command.AssertInvalid.err.module, ".wasm")) {
                try commands.append(command);
            }
        } else if (strcmp("assert_unlinkable", json_command_type.string)) {
            const command = Command{
                .AssertUnlinkable = CommandAssertUnlinkable{
                    .err = try Helpers.parseBadModuleError(&json_command, allocator),
                },
            };
            try commands.append(command);
        } else if (strcmp("assert_uninstantiable", json_command_type.string)) {
            const command = Command{
                .AssertUninstantiable = CommandAssertUninstantiable{
                    .err = try Helpers.parseBadModuleError(&json_command, allocator),
                },
            };
            try commands.append(command);
        } else {
            print("unknown command type: {s}\n", .{json_command_type.string});
            unreachable;
        }
    }

    return commands;
}

const Module = struct {
    filename: []const u8 = "",
    def: ?*bytebox.ModuleDefinition = null,
    inst: ?*bytebox.ModuleInstance = null,
};

const TestOpts = struct {
    vm_type: VmType = .Stack,
    suite_filter_or_null: ?[]const u8 = null,
    test_filter_or_null: ?[]const u8 = null,
    command_filter_or_null: ?[]const u8 = null,
    module_filter_or_null: ?[]const u8 = null,
    trace_mode: bytebox.DebugTrace.Mode = .None,
    force_wasm_regen_only: bool = false,
    log_suite: bool = false,
    log: bytebox.Logger = bytebox.Logger.empty(),
};

fn makeSpectestImports(allocator: std.mem.Allocator) !bytebox.ModuleImportPackage {
    const Functions = struct {
        fn printI32(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const Val, _: [*]Val) error{}!void {
            // std.debug.print("{}", .{params[0].I32});
        }

        fn printI64(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const Val, _: [*]Val) error{}!void {
            // std.debug.print("{}", .{params[0].I64});
        }

        fn printF32(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const Val, _: [*]Val) error{}!void {
            // std.debug.print("{}", .{params[0].F32});
        }

        fn printF64(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const Val, _: [*]Val) error{}!void {
            // std.debug.print("{}", .{params[0].F64});
        }

        fn printI32F32(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const Val, _: [*]Val) error{}!void {
            // std.debug.print("{} {}", .{ params[0].I32, params[1].F32 });
        }

        fn printF64F64(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const Val, _: [*]Val) error{}!void {
            // std.debug.print("{} {}", .{ params[0].F64, params[1].F64 });
        }

        fn print(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const Val, _: [*]Val) error{}!void {
            // std.debug.print("\n", .{});
        }
    };

    const Helpers = struct {
        fn addGlobal(imports: *bytebox.ModuleImportPackage, _allocator: std.mem.Allocator, mut: bytebox.GlobalMut, comptime T: type, value: T, name: []const u8) !void {
            const valtype = switch (T) {
                i32 => ValType.I32,
                i64 => ValType.I64,
                f32 => ValType.F32,
                f64 => ValType.F64,
                else => unreachable,
            };
            const val: Val = switch (T) {
                i32 => Val{ .I32 = value },
                i64 => Val{ .I64 = value },
                f32 => Val{ .F32 = value },
                f64 => Val{ .F64 = value },
                else => unreachable,
            };
            const global_definition = try _allocator.create(bytebox.GlobalDefinition);
            global_definition.* = bytebox.GlobalDefinition{
                .valtype = valtype,
                .mut = mut,
                .expr = undefined, // unused
            };
            const global_instance = try _allocator.create(bytebox.GlobalInstance);
            global_instance.* = bytebox.GlobalInstance{
                .def = global_definition,
                .value = val,
            };
            try imports.globals.append(bytebox.GlobalImport{
                .name = try _allocator.dupe(u8, name),
                .data = .{ .Host = global_instance },
            });
        }
    };
    var imports: bytebox.ModuleImportPackage = try bytebox.ModuleImportPackage.init("spectest", null, null, allocator);

    const no_returns = &[0]ValType{};

    try imports.addHostFunction("print_i32", &[_]ValType{.I32}, no_returns, Functions.printI32, null);
    try imports.addHostFunction("print_i64", &[_]ValType{.I64}, no_returns, Functions.printI64, null);
    try imports.addHostFunction("print_f32", &[_]ValType{.F32}, no_returns, Functions.printF32, null);
    try imports.addHostFunction("print_f64", &[_]ValType{.F64}, no_returns, Functions.printF64, null);
    try imports.addHostFunction("print_i32_f32", &[_]ValType{ .I32, .F32 }, no_returns, Functions.printI32F32, null);
    try imports.addHostFunction("print_f64_f64", &[_]ValType{ .F64, .F64 }, no_returns, Functions.printF64F64, null);
    try imports.addHostFunction("print_f64_f64", &[_]ValType{ .F64, .F64 }, no_returns, Functions.printF64F64, null);
    try imports.addHostFunction("print", &[_]ValType{}, no_returns, Functions.print, null);

    const TableInstance = bytebox.TableInstance;

    const table = try allocator.create(TableInstance);
    table.* = try TableInstance.init(ValType.FuncRef, bytebox.Limits{ .min = 10, .max = 20, .limit_type = 1 }, allocator);
    try imports.tables.append(bytebox.TableImport{
        .name = try allocator.dupe(u8, "table"),
        .data = .{ .Host = table },
    });

    const MemoryInstance = bytebox.MemoryInstance;

    var memory = try allocator.create(MemoryInstance);
    memory.* = try MemoryInstance.init(bytebox.Limits{
        .min = 1,
        .max = 2,
        .limit_type = 1,
    }, null);
    _ = memory.grow(1);
    try imports.memories.append(bytebox.MemoryImport{
        .name = try allocator.dupe(u8, "memory"),
        .data = .{ .Host = memory },
    });

    try Helpers.addGlobal(&imports, allocator, bytebox.GlobalMut.Immutable, i32, 666, "global_i32");
    try Helpers.addGlobal(&imports, allocator, bytebox.GlobalMut.Immutable, i64, 666, "global_i64");
    try Helpers.addGlobal(&imports, allocator, bytebox.GlobalMut.Immutable, f32, 666.6, "global_f32");
    try Helpers.addGlobal(&imports, allocator, bytebox.GlobalMut.Immutable, f64, 666.6, "global_f64");
    try Helpers.addGlobal(&imports, allocator, bytebox.GlobalMut.Immutable, i32, 0, "global-i32");
    try Helpers.addGlobal(&imports, allocator, bytebox.GlobalMut.Immutable, f32, 0, "global-f32");
    try Helpers.addGlobal(&imports, allocator, bytebox.GlobalMut.Mutable, i32, 0, "global-mut-i32");
    try Helpers.addGlobal(&imports, allocator, bytebox.GlobalMut.Mutable, i64, 0, "global-mut-i64");

    return imports;
}

fn run(allocator: std.mem.Allocator, suite_path: []const u8, opts: *const TestOpts) !bool {
    var did_fail_any_test: bool = false;

    var commands: std.array_list.Managed(Command) = try parseCommands(suite_path, allocator);
    defer {
        for (commands.items) |*command| {
            command.deinit(allocator);
        }
        commands.deinit();
    }

    const suite_dir = std.fs.path.dirname(suite_path).?;

    var name_to_module = std.StringHashMap(Module).init(allocator);
    defer {
        var name_to_module_iter = name_to_module.iterator();
        while (name_to_module_iter.next()) |kv| {
            // key memory is owned by commands list, so no need to free

            allocator.free(kv.value_ptr.filename); // ^^^
            if (kv.value_ptr.def) |def| {
                def.destroy();
            }
            if (kv.value_ptr.inst) |inst| {
                inst.destroy();
            }
        }
        name_to_module.deinit();
    }

    // this should be enough to avoid resizing, just bump it up if it's not
    // note that module instance uses the pointer to the stored struct so it's important that the stored instances never move
    name_to_module.ensureTotalCapacity(256) catch unreachable;

    // NOTE this shares the same copies of the import arrays, since the modules must share instances
    var imports = std.array_list.Managed(bytebox.ModuleImportPackage).init(allocator);
    defer {
        const spectest_imports = imports.items[0];
        for (spectest_imports.tables.items) |*item| {
            allocator.free(item.name);
            item.data.Host.deinit();
            allocator.destroy(item.data.Host);
        }
        for (spectest_imports.memories.items) |*item| {
            allocator.free(item.name);
            item.data.Host.deinit();
            allocator.destroy(item.data.Host);
        }
        for (spectest_imports.globals.items) |*item| {
            allocator.free(item.name);
            allocator.destroy(item.data.Host.def);
            allocator.destroy(item.data.Host);
        }

        for (imports.items[1..]) |*item| {
            item.deinit();
        }
        imports.deinit();
    }

    try imports.append(try makeSpectestImports(allocator));

    for (commands.items) |*command| {
        const module_filename = command.getModuleFilename();
        const module_name = command.getModuleName();
        if (opts.module_filter_or_null) |filter| {
            if (strcmp(filter, module_filename) == false) {
                continue;
            }
        }
        // std.debug.print("looking for (name/filename) {s}:{s}\n", .{ module_name, module_filename });

        const entry = name_to_module.getOrPutAssumeCapacity(module_name);
        var module: *Module = entry.value_ptr;
        if (entry.found_existing == false) {
            module.* = Module{};
        }

        switch (command.*) {
            .AssertReturn => {},
            .AssertTrap => {},
            else => logVerbose("{s}: {s}|{s}\n", .{ command.getCommandName(), module_name, module_filename }),
        }

        switch (command.*) {
            .Register => |c| {
                if (module.inst == null) {
                    print(
                        "Register: module instance {s}|{s} was not found in the cache by the name '{s}'. Is the wast malformed?\n",
                        .{ c.module_name, module_filename, module_name },
                    );
                    did_fail_any_test = true;
                    continue;
                }

                logVerbose("\tSetting export module name to {s}\n", .{c.import_name});

                const module_imports: bytebox.ModuleImportPackage = try (module.inst.?).exports(c.import_name);
                try imports.append(module_imports);
                continue;
            },
            else => {},
        }

        if (module.inst == null) {
            const module_path = try std.fs.path.join(allocator, &[_][]const u8{ suite_dir, module_filename });

            var cwd = std.fs.cwd();
            const module_data = try cwd.readFileAlloc(allocator, module_path, 1024 * 1024 * 8);

            var decode_expected_error: ?[]const u8 = null;
            switch (command.*) {
                .AssertMalformed => |c| {
                    decode_expected_error = c.err.expected_error;
                },
                else => {},
            }

            var validate_expected_error: ?[]const u8 = null;
            switch (command.*) {
                .AssertInvalid => |c| {
                    validate_expected_error = c.err.expected_error;
                },
                else => {},
            }

            module.filename = try allocator.dupe(u8, module_filename);

            const module_def_opts = bytebox.ModuleDefinitionOpts{
                .debug_name = std.fs.path.basename(module_filename),
                .log = opts.log,
            };
            module.def = try bytebox.createModuleDefinition(allocator, module_def_opts);
            (module.def.?).decode(module_data) catch |e| {
                var expected_str_or_null: ?[]const u8 = null;
                if (decode_expected_error) |unwrapped_expected| {
                    expected_str_or_null = unwrapped_expected;
                }
                if (expected_str_or_null == null) {
                    if (validate_expected_error) |unwrapped_expected| {
                        expected_str_or_null = unwrapped_expected;
                    }
                }

                if (expected_str_or_null) |expected_str| {
                    if (isSameError(e, expected_str)) {
                        logVerbose("\tSuccess!\n", .{});
                    } else {
                        if (!g_verbose_logging) {
                            print("{s}: {s}\n", .{ command.getCommandName(), module.filename });
                        }
                        print("\tFail: module init failed with error {}, but expected '{s}'\n", .{ e, expected_str });
                        did_fail_any_test = true;
                    }
                } else {
                    if (!g_verbose_logging) {
                        print("{s}: {s}\n", .{ command.getCommandName(), module.filename });
                    }
                    print("\tDecode failed with error: {}\n", .{e});
                    did_fail_any_test = true;
                    return e;
                }
                continue;
            };

            if (decode_expected_error) |expected| {
                if (!g_verbose_logging) {
                    print("{s}: {s}\n", .{ command.getCommandName(), module.filename });
                }
                print("\tFail: module init succeeded, but it should have failed with error '{s}'\n", .{expected});
                did_fail_any_test = true;
                continue;
            }

            if (validate_expected_error) |expected| {
                if (!g_verbose_logging) {
                    print("{s}: {s}\n", .{ command.getCommandName(), module.filename });
                }
                print("\tFail: module init succeeded, but it should have failed with error '{s}'\n", .{expected});
                did_fail_any_test = true;
                continue;
            }

            var instantiate_expected_error: ?[]const u8 = null;
            switch (command.*) {
                .AssertUninstantiable => |c| {
                    instantiate_expected_error = c.err.expected_error;
                },
                .AssertUnlinkable => |c| {
                    instantiate_expected_error = c.err.expected_error;
                },
                else => {},
            }

            module.inst = try bytebox.createModuleInstance(opts.vm_type, module.def.?, allocator);

            const instantiate_opts = bytebox.ModuleInstantiateOpts{
                .imports = imports.items,
                .log = opts.log,
            };
            (module.inst.?).instantiate(instantiate_opts) catch |e| {
                if (instantiate_expected_error) |expected_str| {
                    if (isSameError(e, expected_str)) {
                        logVerbose("\tSuccess!\n", .{});
                    } else {
                        if (!g_verbose_logging) {
                            print("{s}: {s}\n", .{ command.getCommandName(), module.filename });
                        }
                        print("\tFail: instantiate failed with error {}, but expected '{s}'\n", .{ e, expected_str });
                        did_fail_any_test = true;
                    }
                } else {
                    if (!g_verbose_logging) {
                        print("{s}: {s}\n", .{ command.getCommandName(), module.filename });
                    }
                    print("\tInstantiate failed with error: {}\n", .{e});
                    did_fail_any_test = true;
                }
                continue;
            };

            if (instantiate_expected_error) |expected_str| {
                if (!g_verbose_logging) {
                    print("{s}: {s}\n", .{ command.getCommandName(), module.filename });
                }
                print("\tFail: instantiate succeeded, but it should have failed with error '{s}'\n", .{expected_str});
                did_fail_any_test = true;
                continue;
            }
        }

        switch (command.*) {
            .AssertReturn => |c| {
                const PrintTestHelper = struct {
                    fn logVerbose(filename: []const u8, field: []const u8, values: []LaneTypedVal) void {
                        if (g_verbose_logging) {
                            log(filename, field, values);
                        }
                    }

                    fn log(filename: []const u8, field: []const u8, values: []LaneTypedVal) void {
                        print("assert_return: {s}:{s}(", .{ filename, field });
                        for (values) |v| {
                            switch (v.v.type) {
                                .V128 => {
                                    printVector(v.lane_type, v.v.val.V128);
                                },
                                else => print("{}", .{v}),
                            }
                        }
                        print(")\n", .{});
                    }

                    fn printVector(lane_type: V128LaneType, vec: v128) void {
                        switch (lane_type) {
                            .I8x16 => print("{}, ", .{@as(i8x16, @bitCast(vec))}),
                            .I16x8 => print("{}, ", .{@as(i16x8, @bitCast(vec))}),
                            .I32x4 => print("{}, ", .{@as(i32x4, @bitCast(vec))}),
                            .I64x2 => print("{}, ", .{@as(i64x2, @bitCast(vec))}),
                            .F32x4 => print("{}, ", .{@as(f32x4, @bitCast(vec))}),
                            .F64x2 => print("{}, ", .{@as(f64x2, @bitCast(vec))}),
                        }
                    }
                };

                if (opts.command_filter_or_null) |filter| {
                    if (strcmp("assert_return", filter) == false) {
                        continue;
                    }
                }

                if (opts.test_filter_or_null) |filter| {
                    if (strcmp(filter, c.action.field) == false) {
                        logVerbose("\tskipped {s}...\n", .{c.action.field});
                        continue;
                    }
                }

                const num_expected_returns = if (c.expected_returns) |returns| returns.items.len else 0;
                var returns_placeholder = std.array_list.Managed(bytebox.Val).init(allocator);
                defer returns_placeholder.deinit();

                try returns_placeholder.resize(num_expected_returns);
                var returns = returns_placeholder.items;
                var return_types: ?[]ValType = null;
                defer if (return_types) |types| allocator.free(types);

                PrintTestHelper.logVerbose(module.filename, c.action.field, c.action.args.items);
                // logVerbose("assert_return: {s}:{s}({any})\n", .{ module.filename, c.action.field, c.action.args.items });

                var action_succeeded = true;
                switch (c.action.type) {
                    .Invocation => {
                        var vals = try LaneTypedVal.toValArrayList(c.action.args.items, allocator);
                        defer vals.deinit();

                        const func_handle: bytebox.FunctionHandle = try (module.inst.?).getFunctionHandle(c.action.field);
                        (module.inst.?).invoke(func_handle, vals.items.ptr, returns.ptr, .{}) catch |e| {
                            if (!g_verbose_logging) {
                                PrintTestHelper.log(module.filename, c.action.field, c.action.args.items);
                                // print("assert_return: {s}:{s}({any})\n", .{ module.filename, c.action.field, c.action.args.items });
                            }
                            print("\tInvoke fail with error: {}\n", .{e});
                            action_succeeded = false;
                        };

                        const func_export = module.inst.?.module_def.getFunctionExport(func_handle);
                        std.debug.assert(func_export != null);
                        if (func_export) |f| {
                            return_types = try allocator.dupe(ValType, f.returns);
                        }
                    },
                    .Get => {
                        if ((module.inst.?).getGlobalExport(c.action.field)) |global_export| {
                            returns[0] = global_export.val.*;
                            return_types = try allocator.dupe(ValType, &[_]ValType{global_export.valtype});
                        } else |e| {
                            if (!g_verbose_logging) {
                                PrintTestHelper.log(module.filename, c.action.field, c.action.args.items);
                                // print("assert_return: {s}:{s}({any})\n", .{ module.filename, c.action.field, c.action.args.items });
                            }
                            print("\tGet fail with error: {}\n", .{e});
                            action_succeeded = false;
                        }
                    },
                }

                if (action_succeeded) {
                    if (c.expected_returns) |expected| {
                        for (returns, 0..) |r, i| {
                            var pass = false;

                            const return_type: ValType = return_types.?[i];

                            const expected_value: TaggedVal = expected.items[i].v;
                            if (expected_value.type != .V128) {
                                if (expected_value.type == .F32 and std.math.isNan(expected_value.val.F32)) {
                                    pass = return_type == .F32 and std.math.isNan(r.F32);
                                } else if (expected_value.type == .F64 and std.math.isNan(expected_value.val.F64)) {
                                    pass = return_type == .F64 and std.math.isNan(r.F64);
                                } else {
                                    pass = Val.eql(expected_value.type, r, expected_value.val);
                                }

                                if (pass == false) {
                                    if (!g_verbose_logging) {
                                        print("assert_return: {s}:{s}({any})\n", .{ module.filename, c.action.field, c.action.args.items });
                                    }

                                    const format_str = "\tFail on return {}/{}. Expected: {}, Actual: {}\n";
                                    switch (expected_value.type) {
                                        .I32 => print(format_str, .{ i + 1, returns.len, expected_value.val.I32, r.I32 }),
                                        .I64 => print(format_str, .{ i + 1, returns.len, expected_value.val.I64, r.I64 }),
                                        .F32 => print(format_str, .{ i + 1, returns.len, expected_value.val.F32, r.F32 }),
                                        .F64 => print(format_str, .{ i + 1, returns.len, expected_value.val.F64, r.F64 }),
                                        else => unreachable,
                                    }
                                    action_succeeded = false;
                                }
                            } else {
                                const V128ExpectHelper = struct {
                                    fn expect(comptime VectorType: type, actual_value: v128, expected_value_: v128, return_index: usize, returns_length: usize, module_: *const Module, command_: *const CommandAssertReturn) bool {
                                        const actual_typed = @as(VectorType, @bitCast(actual_value));
                                        const expected_typed = @as(VectorType, @bitCast(expected_value_));

                                        var is_equal = true;
                                        const child_type = @typeInfo(VectorType).vector.child;
                                        switch (child_type) {
                                            i8, i16, i32, i64 => {
                                                is_equal = std.meta.eql(actual_typed, expected_typed);
                                            },
                                            f32, f64 => {
                                                const len = @typeInfo(VectorType).vector.len;
                                                var vec_i: u32 = 0;
                                                while (vec_i < len) : (vec_i += 1) {
                                                    if (std.math.isNan(expected_typed[vec_i])) {
                                                        is_equal = is_equal and std.math.isNan(actual_typed[vec_i]);
                                                    } else {
                                                        is_equal = is_equal and expected_typed[vec_i] == actual_typed[vec_i];
                                                    }
                                                }
                                            },
                                            else => @compileError("unsupported vector child type"),
                                        }

                                        if (is_equal == false) {
                                            if (!g_verbose_logging) {
                                                print("assert_return: {s}:{s}({any})\n", .{ module_.filename, command_.action.field, command_.action.args.items });
                                            }

                                            print("\tFail on return {}/{}. Expected: {}, Actual: {}\n", .{ return_index + 1, returns_length, expected_typed, actual_typed });
                                        }
                                        return is_equal;
                                    }
                                };

                                switch (expected.items[i].lane_type) {
                                    .I8x16 => {
                                        action_succeeded = V128ExpectHelper.expect(i8x16, r.V128, expected_value.val.V128, i, returns.len, module, &c);
                                    },
                                    .I16x8 => {
                                        action_succeeded = V128ExpectHelper.expect(i16x8, r.V128, expected_value.val.V128, i, returns.len, module, &c);
                                    },
                                    .I32x4 => {
                                        action_succeeded = V128ExpectHelper.expect(i32x4, r.V128, expected_value.val.V128, i, returns.len, module, &c);
                                    },
                                    .I64x2 => {
                                        action_succeeded = V128ExpectHelper.expect(i64x2, r.V128, expected_value.val.V128, i, returns.len, module, &c);
                                    },
                                    .F32x4 => {
                                        action_succeeded = V128ExpectHelper.expect(f32x4, r.V128, expected_value.val.V128, i, returns.len, module, &c);
                                    },
                                    .F64x2 => {
                                        action_succeeded = V128ExpectHelper.expect(f64x2, r.V128, expected_value.val.V128, i, returns.len, module, &c);
                                    },
                                }
                            }
                        }
                    }
                }

                if (action_succeeded) {
                    logVerbose("\tSuccess!\n", .{});
                } else {
                    did_fail_any_test = true;
                }
            },
            .AssertTrap => |c| {
                if (opts.command_filter_or_null) |filter| {
                    if (strcmp("assert_trap", filter) == false) {
                        continue;
                    }
                }

                if (opts.test_filter_or_null) |filter| {
                    if (strcmp(filter, c.action.field) == false) {
                        logVerbose("assert_return: skipping {s}:{s}\n", .{ module.filename, c.action.field });
                        continue;
                    }
                }

                logVerbose("assert_trap: {s}:{s}({any})\n", .{ module.filename, c.action.field, c.action.args.items });

                var returns_placeholder: [8]Val = undefined;
                var returns = returns_placeholder[0..];

                var action_failed = false;
                var action_failed_with_correct_trap = false;
                var caught_error: ?anyerror = null;

                switch (c.action.type) {
                    .Invocation => {
                        var vals = try LaneTypedVal.toValArrayList(c.action.args.items, allocator);
                        defer vals.deinit();

                        const func_handle: bytebox.FunctionHandle = try (module.inst.?).getFunctionHandle(c.action.field);
                        (module.inst.?).invoke(func_handle, vals.items.ptr, returns.ptr, .{}) catch |e| {
                            action_failed = true;
                            caught_error = e;

                            if (isSameError(e, c.expected_error)) {
                                action_failed_with_correct_trap = true;
                            }
                        };
                    },
                    .Get => {
                        if ((module.inst.?).getGlobalExport(c.action.field)) |global_export| {
                            returns[0] = global_export.val.*;
                        } else |e| {
                            action_failed = true;
                            caught_error = e;

                            if (isSameError(e, c.expected_error)) {
                                action_failed_with_correct_trap = true;
                            }
                        }
                    },
                }

                if (action_failed and action_failed_with_correct_trap) {
                    logVerbose("\tSuccess!\n", .{});
                } else {
                    if (!g_verbose_logging) {
                        print("assert_trap: {s}:{s}({any})\n", .{ module.filename, c.action.field, c.action.args.items });
                    }
                    if (action_failed_with_correct_trap == false) {
                        print("\tInvoke trapped, but got error '{}'' instead of expected '{s}':\n", .{ caught_error.?, c.expected_error });
                        did_fail_any_test = true;
                    } else {
                        print("\tInvoke succeeded instead of trapping on expected {s}:\n", .{c.expected_error});
                        did_fail_any_test = true;
                    }
                }
            },
            else => {},
        }
    }

    return !did_fail_any_test;
}

pub fn parseVmType(backend_str: []const u8) VmType {
    if (strcmp("stack", backend_str)) {
        return .Stack;
    } else if (strcmp("register", backend_str)) {
        return .Register;
    } else {
        print("Failed parsing backend string '{s}'. Expected 'stack' or 'register'.", .{backend_str});
        return .Stack;
    }
}

fn pathExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{ .mode = .read_only }) catch |e| {
        return switch (e) {
            error.PermissionDenied,
            error.FileBusy,
            error.ReadOnlyFileSystem,
            => true,

            error.FileNotFound => false,

            // unknown status, but we'll count it as a fail
            error.NameTooLong,
            error.InputOutput,
            error.SystemResources,
            error.BadPathName,
            error.SymLinkLoop,
            error.InvalidUtf8,
            => false,
            else => false,
        };
    };

    return true;
}

fn getNextArg(args: []const []const u8, index: *usize, print_help: *bool) ?[]const u8 {
    index.* += 1;
    if (index.* < args.len) {
        return args[index.*];
    }
    print_help.* = true;
    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator: std.mem.Allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var opts = TestOpts{};

    var print_help: bool = false;

    var args_index: usize = 1; // skip program name
    while (args_index < args.len) : (args_index += 1) {
        const arg = args[args_index];
        if (strcmp("--help", arg) or strcmp("-h", arg) or strcmp("help", arg)) {
            print_help = true;
        } else if (strcmp("--backend", arg)) {
            if (getNextArg(args, &args_index, &print_help)) |vm_type| {
                opts.vm_type = parseVmType(vm_type);
            }
        } else if (strcmp("--suite", arg)) {
            if (getNextArg(args, &args_index, &print_help)) |filter| {
                opts.suite_filter_or_null = filter;
                print("found suite filter: {s}\n", .{opts.suite_filter_or_null.?});
            }
        } else if (strcmp("--module", arg)) {
            if (getNextArg(args, &args_index, &print_help)) |filter| {
                opts.module_filter_or_null = filter;
                print("found module filter: {s}\n", .{opts.module_filter_or_null.?});
            }
        } else if (strcmp("--command", arg)) {
            if (getNextArg(args, &args_index, &print_help)) |filter| {
                opts.command_filter_or_null = filter;
                print("found command filter: {s}\n", .{opts.command_filter_or_null.?});
            }
        } else if (strcmp("--test", arg)) {
            if (getNextArg(args, &args_index, &print_help)) |filter| {
                opts.test_filter_or_null = filter;
                print("found test filter: {s}\n", .{opts.test_filter_or_null.?});
            }
        } else if (strcmp("--trace", arg)) {
            if (getNextArg(args, &args_index, &print_help)) |trace_mode_str| {
                if (config.enable_debug_trace == false) {
                    print("Debug tracing must be enabled at compile time -Ddebug_trace=true\n", .{});
                } else if (bytebox.DebugTrace.parseMode(trace_mode_str)) |mode| {
                    bytebox.DebugTrace.setMode(mode);
                } else {
                    print("got invalid trace mode '{s}', check help for allowed options", .{trace_mode_str});
                    return;
                }
            }
        } else if (strcmp("--force-wasm-regen-only", arg)) {
            opts.force_wasm_regen_only = true;
            print("Force-regenerating wasm files and driver .json, skipping test run\n", .{});
        } else if (strcmp("--log-suite", arg)) {
            opts.log_suite = true;
        } else if (strcmp("--module-logging", arg)) {
            opts.log = bytebox.Logger.default();
        } else if (strcmp("--verbose", arg) or strcmp("-v", arg)) {
            g_verbose_logging = true;
            print("verbose logging: on\n", .{});
        } else {
            print_help = true;
        }
    }

    if (print_help) {
        const help_text =
            \\
            \\Usage: {s} [OPTION]...
            \\    --backend <type>
            \\      Options are: stack (default), register
            \\
            \\    --suite <suitename>
            \\      Only run tests belonging to the given suite. Examples: i32, br_if,
            \\      utf8-import-field, unwind
            \\
            \\    --module <filename>
            \\      Only decode and initialize the given module. Only tests belonging to the
            \\      given module file are run.
            \\
            \\    --command <type>
            \\      Only run tests with the given command type. Examples: assert_return
            \\      assert_trap, assert_invalid
            \\
            \\    --test <testname>
            \\      Run all tests where the 'field' in the json driver matches this filter.
            \\
            \\    --trace <level>
            \\      Print debug traces while executing the test at the given level. <level> can
            \\      be: none (default), function, instruction
            \\
            \\    --force-wasm-regen-only
            \\      By default, if a given testsuite can't find its' .json file driver, it will
            \\      regenerate the wasm files and json driver, then run the test. This command
            \\      will force regeneration of said files and skip running all tests.
            \\
            \\    --log-suite
            \\      Log the name of each suite and aggregate test result.
            \\
            \\    --module-logging
            \\      Enables logging from inside the module when reporting errors.
            \\
            \\    --verbose
            \\      Turn on verbose logging for each step of the test suite run.
            \\
            \\
        ;
        print(help_text, .{args[0]});
        return;
    }

    const all_suites = [_][]const u8{
        "address",
        "align",
        "binary",
        "binary-leb128",
        "block",
        "br",
        "br_if",
        "br_table",
        "bulk",
        "call",
        "call_indirect",
        // "comments", // wabt seems to error on this
        "const",
        "conversions",
        "custom",
        "data",
        "elem",
        "endianness",
        "exports",
        "f32",
        "f32_bitwise",
        "f32_cmp",
        "f64",
        "f64_bitwise",
        "f64_cmp",
        "fac",
        "float_exprs",
        "float_literals",
        "float_memory",
        "float_misc",
        "forward",
        "func",
        "func_ptrs",
        "global",
        "i32",
        "i64",
        "if",
        "imports",
        "inline-module",
        "int_exprs",
        "int_literals",
        "labels",
        "left-to-right",
        "linking",
        "load",
        "local_get",
        "local_set",
        "local_tee",
        "loop",
        "memory",
        "memory_copy",
        "memory_fill",
        "memory_grow",
        "memory_init",
        "memory_redundancy",
        "memory_size",
        "memory_trap",
        "names",
        "nop",
        "ref_func",
        "ref_is_null",
        "ref_null",
        "return",
        "select",
        "simd_address",
        "simd_align",
        "simd_bitwise",
        "simd_bit_shift",
        "simd_boolean",
        "simd_const",
        "simd_conversions",
        "simd_f32x4",
        "simd_f32x4_arith",
        "simd_f32x4_cmp",
        "simd_f32x4_pmin_pmax",
        "simd_f32x4_rounding",
        "simd_f64x2",
        "simd_f64x2_arith",
        "simd_f64x2_cmp",
        "simd_f64x2_pmin_pmax",
        "simd_f64x2_rounding",
        "simd_i16x8_arith",
        "simd_i16x8_arith2",
        "simd_i16x8_cmp",
        "simd_i16x8_extadd_pairwise_i8x16",
        "simd_i16x8_extmul_i8x16",
        "simd_i16x8_q15mulr_sat_s",
        "simd_i16x8_sat_arith",
        "simd_i32x4_arith",
        "simd_i32x4_arith2",
        "simd_i32x4_cmp",
        "simd_i32x4_dot_i16x8",
        "simd_i32x4_extadd_pairwise_i16x8",
        "simd_i32x4_extmul_i16x8",
        "simd_i32x4_trunc_sat_f32x4",
        "simd_i32x4_trunc_sat_f64x2",
        "simd_i64x2_arith",
        "simd_i64x2_arith2",
        "simd_i64x2_cmp",
        "simd_i64x2_extmul_i32x4",
        "simd_i8x16_arith",
        "simd_i8x16_arith2",
        "simd_i8x16_cmp",
        "simd_i8x16_sat_arith",
        "simd_int_to_int_extend",
        "simd_lane",
        "simd_load",
        "simd_load16_lane",
        "simd_load32_lane",
        "simd_load64_lane",
        "simd_load8_lane",
        "simd_load_extend",
        "simd_load_splat",
        "simd_load_zero",
        "simd_splat",
        "simd_store",
        "simd_store16_lane",
        "simd_store32_lane",
        "simd_store64_lane",
        "simd_store8_lane",
        "skip-stack-guard-page",
        "stack",
        "start",
        "store",
        "switch",
        "table",
        "table-sub",
        "table_copy",
        "table_fill",
        "table_get",
        "table_grow",
        "table_init",
        "table_set",
        "table_size",
        "token",
        "traps",
        "type",
        "unreachable",
        "unreached-invalid",
        "unreached-valid",
        "unwind",
        "utf8-custom-section-id",
        "utf8-import-field",
        "utf8-import-module",
        "utf8-invalid-encoding",
    };

    var did_all_succeed: bool = true;

    for (all_suites) |suite| {
        if (opts.suite_filter_or_null) |filter| {
            if (strcmp(filter, suite) == false) {
                continue;
            }
        }

        // determine if there is a memory64 version of the test - if so, run that one
        const suite_wast_base_path_no_extension: []const u8 = try std.fs.path.join(allocator, &[_][]const u8{ "test", "wasm", "wasm-testsuite", suite });
        defer allocator.free(suite_wast_base_path_no_extension);
        const suite_wast_base_path: []u8 = try std.mem.join(allocator, "", &[_][]const u8{ suite_wast_base_path_no_extension, ".wast" });
        defer allocator.free(suite_wast_base_path);

        const suite_wast_mem64_path_no_extension: []const u8 = try std.fs.path.join(allocator, &[_][]const u8{ "test", "wasm", "wasm-testsuite", "proposals", "memory64", suite });
        defer allocator.free(suite_wast_mem64_path_no_extension);
        const suite_wast_mem64_path: []u8 = try std.mem.join(allocator, "", &[_][]const u8{ suite_wast_mem64_path_no_extension, ".wast" });
        defer allocator.free(suite_wast_mem64_path);

        const suite_wast_path = blk: {
            const is_64bit_arch = @sizeOf(usize) >= @sizeOf(u64);
            if (is_64bit_arch and pathExists(suite_wast_mem64_path)) {
                if (opts.log_suite) {
                    print("Using memory64 for suite {s}\n", .{suite});
                }
                break :blk suite_wast_mem64_path;
            } else {
                break :blk suite_wast_base_path;
            }
        };

        // wasm path
        const suite_path_no_extension: []const u8 = try std.fs.path.join(allocator, &[_][]const u8{ "test", "wasm", "wasm-generated", suite, suite });
        defer allocator.free(suite_path_no_extension);

        const suite_path = try std.mem.join(allocator, "", &[_][]const u8{ suite_path_no_extension, ".json" });
        defer allocator.free(suite_path);

        var needs_regen: bool = false;
        if (opts.force_wasm_regen_only) {
            needs_regen = true;
        } else {
            needs_regen = pathExists(suite_path) == false;
        }

        if (needs_regen) {
            logVerbose("Regenerating wasm and json driver for suite {s}\n", .{suite});

            // need to navigate back to repo root because the wast2json process will be running in a subdir
            const suite_wast_path_relative = try std.fs.path.join(allocator, &[_][]const u8{ "../../../../", suite_wast_path });
            defer allocator.free(suite_wast_path_relative);

            const suite_json_filename: []const u8 = try std.mem.join(allocator, "", &[_][]const u8{ suite, ".json" });
            defer allocator.free(suite_json_filename);

            const suite_wasm_folder: []const u8 = try std.fs.path.join(allocator, &[_][]const u8{ "test", "wasm", "wasm-generated", suite });
            defer allocator.free(suite_wasm_folder);

            std.fs.cwd().makeDir("test/wasm/wasm-generated") catch |e| {
                if (e != error.PathAlreadyExists) {
                    return e;
                }
            };

            std.fs.cwd().makeDir(suite_wasm_folder) catch |e| {
                if (e != error.PathAlreadyExists) {
                    return e;
                }
            };

            var process = std.process.Child.init(&[_][]const u8{ "wasm-tools", "json-from-wast", "--pretty", "-o", suite_json_filename, suite_wast_path_relative }, allocator);

            process.cwd = suite_wasm_folder;

            _ = try process.spawnAndWait();
        }

        if (opts.force_wasm_regen_only == false) {
            if (opts.log_suite or g_verbose_logging) {
                print("Running test suite: {s}\n", .{suite});
            }

            const success: bool = try run(allocator, suite_path, &opts);
            did_all_succeed = did_all_succeed and success;

            if (success and opts.log_suite and !g_verbose_logging) {
                print("\tSuccess\n", .{});
            }
        }
    }

    if (did_all_succeed == false) {
        std.process.exit(1);
    }
}
