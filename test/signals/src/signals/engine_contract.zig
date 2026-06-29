const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const erased_calls = @import("erased_calls.zig");
const render = @import("render_commands.zig");
const hv = @import("host_values.zig");
const engine_metrics = @import("engine_metrics.zig");

pub const HostValue = u64;
pub const HostValueCapability = hv.HostValueCapabilityHandle;

const RenderTextField = render.TextField;
const RenderBoolField = render.BoolField;
const RenderEventKind = render.EventKind;
const EventPayloadKind = render.EventPayloadKind;
const EventPayloadAccessor = render.EventPayloadAccessor;

fn verifyDeclFn(comptime owner_name: []const u8, comptime Owner: type, comptime decl_name: []const u8, comptime params: anytype, comptime return_type: type) void {
    if (!@hasDecl(Owner, decl_name)) {
        @compileError(owner_name ++ " is missing " ++ decl_name);
    }

    const fn_type = @TypeOf(@field(Owner, decl_name));
    const type_info = @typeInfo(fn_type);
    if (type_info != .@"fn") {
        @compileError(owner_name ++ "." ++ decl_name ++ " must be a function");
    }

    const fn_info = type_info.@"fn";
    if (fn_info.params.len != params.len) {
        @compileError(owner_name ++ "." ++ decl_name ++ " has the wrong parameter count");
    }
    inline for (params, 0..) |expected, index| {
        const actual = fn_info.params[index].type orelse {
            @compileError(owner_name ++ "." ++ decl_name ++ " must not use anytype parameters");
        };
        if (actual != expected) {
            @compileError(owner_name ++ "." ++ decl_name ++ " has an incompatible parameter type");
        }
    }
    const actual_return = fn_info.return_type orelse void;
    if (actual_return != return_type) {
        @compileError(owner_name ++ "." ++ decl_name ++ " has an incompatible return type");
    }
}

fn verifyTypeDecl(comptime owner_name: []const u8, comptime Owner: type, comptime decl_name: []const u8) void {
    if (!@hasDecl(Owner, decl_name)) {
        @compileError(owner_name ++ " is missing " ++ decl_name);
    }
    if (@TypeOf(@field(Owner, decl_name)) != type) {
        @compileError(owner_name ++ "." ++ decl_name ++ " must be a type");
    }
}

pub fn verifyRegistryOps(comptime Ops: type) void {
    verifyDeclFn("engine RegistryOps", Ops, "retainCapability", .{ Ops, HostValueCapability }, void);
    verifyDeclFn("engine RegistryOps", Ops, "releaseCapability", .{ Ops, HostValueCapability }, void);
    verifyDeclFn("engine RegistryOps", Ops, "capabilitiesMatch", .{ Ops, HostValueCapability, HostValueCapability }, bool);
    verifyDeclFn("engine RegistryOps", Ops, "capabilityIsActive", .{ Ops, HostValueCapability }, bool);
    verifyDeclFn("engine RegistryOps", Ops, "cloneValueWithCapability", .{ Ops, HostValue, HostValueCapability }, HostValue);
    verifyDeclFn("engine RegistryOps", Ops, "callHostValueToHostValueWithCapability", .{ Ops, HostValueCapability, abi.RocErasedCallable, HostValue }, HostValue);
    verifyDeclFn("engine RegistryOps", Ops, "splitBoxWithSplit", .{ Ops, abi.RocBox, abi.RocErasedCallable }, erased_calls.RocBoxPair);
}

pub fn verifySink(comptime Sink: type) void {
    verifyDeclFn("engine Sink", Sink, "reset", .{Sink}, void);
    verifyDeclFn("engine Sink", Sink, "appendNode", .{ Sink, u64, u64, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "ensureNode", .{ Sink, u64, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "removeNode", .{ Sink, u64 }, void);
    verifyDeclFn("engine Sink", Sink, "replaceChildren", .{ Sink, u64, []const u64 }, void);
    verifyDeclFn("engine Sink", Sink, "replaceChildrenForMoves", .{ Sink, u64, []const u64 }, void);
    verifyDeclFn("engine Sink", Sink, "applyTextField", .{ Sink, u64, RenderTextField, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "applyTextAttr", .{ Sink, u64, []const u8, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "applyBoolField", .{ Sink, u64, RenderBoolField, bool }, void);
    verifyDeclFn("engine Sink", Sink, "clearTextField", .{ Sink, u64, RenderTextField }, void);
    verifyDeclFn("engine Sink", Sink, "clearTextAttr", .{ Sink, u64, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "clearBoolField", .{ Sink, u64, RenderBoolField }, void);
    verifyDeclFn("engine Sink", Sink, "bindEventKind", .{ Sink, u64, RenderEventKind, u64, EventPayloadAccessor }, void);
    verifyDeclFn("engine Sink", Sink, "clearEvent", .{ Sink, u64, RenderEventKind }, void);
    verifyDeclFn("engine Sink", Sink, "bindEventName", .{ Sink, u64, []const u8, u64, u32, EventPayloadKind, EventPayloadAccessor }, void);
    verifyDeclFn("engine Sink", Sink, "clearEventName", .{ Sink, u64, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "startInterval", .{ Sink, u64, u64 }, void);
    verifyDeclFn("engine Sink", Sink, "cancelInterval", .{ Sink, u64 }, void);
    verifyDeclFn("engine Sink", Sink, "startTask", .{ Sink, u64, []const u8, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "cancelTask", .{ Sink, u64 }, void);
    verifyDeclFn("engine Sink", Sink, "debugAssertNode", .{ Sink, u64, bool, ?[]const u8, ?u64, []const u64, ?u64, ?u64, ?u64, ?u64, ?u64, ?u64, ?u64 }, void);
}

pub fn verifyMetrics(comptime Metrics: type) void {
    verifyDeclFn("engine Metrics", Metrics, "bump", .{ *Metrics, engine_metrics.RuntimeMetrics.Field, u64 }, void);
}

pub fn verifyCtx(comptime Ctx: type) void {
    verifyTypeDecl("engine Ctx", Ctx, "Handle");
    verifyTypeDecl("engine Ctx", Ctx, "RegistryOps");
    verifyTypeDecl("engine Ctx", Ctx, "Metrics");
    verifyTypeDecl("engine Ctx", Ctx, "Sink");

    verifyDeclFn("engine Ctx", Ctx, "zeroMetrics", .{}, Ctx.Metrics);
    verifyDeclFn("engine Ctx", Ctx, "allocator", .{Ctx.Handle}, std.mem.Allocator);
    verifyDeclFn("engine Ctx", Ctx, "cloneHostValue", .{ Ctx.Handle, HostValue }, HostValue);
    verifyDeclFn("engine Ctx", Ctx, "pushHostValueCapabilities", .{ Ctx.Handle, []const HostValueCapability }, void);
    verifyDeclFn("engine Ctx", Ctx, "popHostValueCapabilities", .{Ctx.Handle}, void);
    verifyDeclFn("engine Ctx", Ctx, "stateValueByNodeId", .{ Ctx.Handle, u64 }, HostValue);
    verifyDeclFn("engine Ctx", Ctx, "stateCapability", .{ Ctx.Handle, u64 }, HostValueCapability);
    verifyDeclFn("engine Ctx", Ctx, "sink", .{Ctx.Handle}, Ctx.Sink);
    verifyRegistryOps(Ctx.RegistryOps);
    verifyMetrics(Ctx.Metrics);
    verifySink(Ctx.Sink);
}

const VerifySink = struct {
    pub fn reset(_: VerifySink) void {}
    pub fn appendNode(_: VerifySink, _: u64, _: u64, _: []const u8) void {}
    pub fn ensureNode(_: VerifySink, _: u64, _: []const u8) void {}
    pub fn removeNode(_: VerifySink, _: u64) void {}
    pub fn replaceChildren(_: VerifySink, _: u64, _: []const u64) void {}
    pub fn replaceChildrenForMoves(_: VerifySink, _: u64, _: []const u64) void {}
    pub fn applyTextField(_: VerifySink, _: u64, _: RenderTextField, _: []const u8) void {}
    pub fn applyTextAttr(_: VerifySink, _: u64, _: []const u8, _: []const u8) void {}
    pub fn applyBoolField(_: VerifySink, _: u64, _: RenderBoolField, _: bool) void {}
    pub fn clearTextField(_: VerifySink, _: u64, _: RenderTextField) void {}
    pub fn clearTextAttr(_: VerifySink, _: u64, _: []const u8) void {}
    pub fn clearBoolField(_: VerifySink, _: u64, _: RenderBoolField) void {}
    pub fn bindEventKind(_: VerifySink, _: u64, _: RenderEventKind, _: u64, _: EventPayloadAccessor) void {}
    pub fn clearEvent(_: VerifySink, _: u64, _: RenderEventKind) void {}
    pub fn bindEventName(_: VerifySink, _: u64, _: []const u8, _: u64, _: u32, _: EventPayloadKind, _: EventPayloadAccessor) void {}
    pub fn clearEventName(_: VerifySink, _: u64, _: []const u8) void {}
    pub fn startInterval(_: VerifySink, _: u64, _: u64) void {}
    pub fn cancelInterval(_: VerifySink, _: u64) void {}
    pub fn startTask(_: VerifySink, _: u64, _: []const u8, _: []const u8) void {}
    pub fn cancelTask(_: VerifySink, _: u64) void {}
    pub fn debugAssertNode(_: VerifySink, _: u64, _: bool, _: ?[]const u8, _: ?u64, _: []const u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64) void {}
};

const VerifyCtxHost = struct {};

const VerifyCtx = struct {
    pub const Handle = *VerifyCtxHost;
    pub const RegistryOps = hv.RegistryOps();
    pub const Metrics = engine_metrics.RuntimeMetrics;
    pub const Sink = VerifySink;

    pub fn zeroMetrics() Metrics {
        return engine_metrics.zeroRuntimeMetrics();
    }

    pub fn allocator(_: Handle) std.mem.Allocator {
        return std.heap.page_allocator;
    }

    pub fn cloneHostValue(_: Handle, value: HostValue) HostValue {
        return value;
    }

    pub fn pushHostValueCapabilities(_: Handle, _: []const HostValueCapability) void {}

    pub fn popHostValueCapabilities(_: Handle) void {}

    pub fn stateValueByNodeId(_: Handle, _: u64) HostValue {
        return 0;
    }

    pub fn stateCapability(_: Handle, _: u64) HostValueCapability {
        return undefined;
    }

    pub fn sink(_: Handle) Sink {
        return .{};
    }
};

test "verifyCtx accepts a complete signals engine context" {
    comptime verifyCtx(VerifyCtx);
}
