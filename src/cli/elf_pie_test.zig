//! End-to-end ELF prelink/patch coverage. Startup must relocate both the
//! platform's pointers and the app's appended pointers before calling the app.
const std = @import("std");
const builtin = @import("builtin");
const backend = @import("backend");
const host_prelink = @import("host_prelink.zig");
const fast = @import("fast_dev_link.zig");
const patch = @import("patch_link.zig");
const cli = @import("CliCtx.zig");

const provides = [_][]const u8{ "app_entry", "app_scalar", "app_value" };
const layouts = [_]host_prelink.Provide{
    .{ .name = "app_entry", .kind = .procedure, .size = 0 },
    .{ .name = "app_scalar", .kind = .data, .size = 8 },
    .{ .name = "app_value", .kind = .data, .size = 16 },
};
const keep = [_][]const u8{ "host_callback", "host_value" };

fn command(a: std.mem.Allocator, argv: []const []const u8) (std.process.RunError || error{CommandFailed})![]const u8 {
    const result = try std.process.run(a, std.testing.io, .{ .argv = argv });
    if (result.term != .exited or result.term.exited != 0) {
        std.debug.print("command {s}: {any}\n{s}\n{s}\n", .{ argv[0], result.term, result.stdout, result.stderr });
        return error.CommandFailed;
    }
    return result.stdout;
}

fn read16(bytes: []const u8, offset: usize) u16 {
    return std.mem.readInt(u16, bytes[offset..][0..2], .little);
}

fn read64(bytes: []const u8, offset: usize) u64 {
    return std.mem.readInt(u64, bytes[offset..][0..8], .little);
}

test "ELF PIE prelink and patch relocate host app and provided pointers at startup" {
    if (builtin.os.tag != .linux) return error.SkipZigTest;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const dir = try tmp.dir.realPathFileAlloc(io, ".", a);
    const host_c = try std.fs.path.join(a, &.{ dir, "host.c" });
    const app_c = try std.fs.path.join(a, &.{ dir, "app.c" });
    const host_o = try std.fs.path.join(a, &.{ dir, "host.o" });
    const app_o = try std.fs.path.join(a, &.{ dir, "app.o" });
    const windows_o = try std.fs.path.join(a, &.{ dir, "windows.o" });
    const linked_host = try std.fs.path.join(a, &.{ dir, "linked_host" });
    const dest = try std.fs.path.join(a, &.{ dir, "program" });
    try tmp.dir.writeFile(io, .{ .sub_path = "host.c", .data = host_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "app.c", .data = app_source });

    var cli_io = cli.Io.create(io);
    var ctx = cli.CliCtx.init(std.testing.allocator, a, &cli_io, .build);
    ctx.initIo();
    defer ctx.deinit();
    for ([_]bool{ false, true }) |arm64| {
        const target = if (arm64) "aarch64-linux-musl" else "x86_64-linux-musl";
        _ = try command(a, &.{ "zig", "cc", "-target", target, "-fPIE", "-O1", "-fno-asynchronous-unwind-tables", "-c", app_c, "-o", app_o });
        const object = try tmp.dir.readFileAlloc(io, "app.o", a, .unlimited);
        const image = try backend.dev.UnitImage.build(a, &.{object}, .{ .keep_debug = false, .atoms = .whole });
        const view = try backend.dev.UnitImage.View.init(image);
        const empty_object = try host_prelink.linkObject(a, arm64, &.{});
        const empty_image = try backend.dev.UnitImage.build(a, &.{empty_object}, .{ .keep_debug = false, .atoms = .whole });
        const support_view = try backend.dev.UnitImage.View.init(empty_image);
        const Case = struct { kind: host_prelink.Kind, musl_startup: bool = false };
        for ([_]Case{ .{ .kind = .pie }, .{ .kind = .executable }, .{ .kind = .shared }, .{ .kind = .pie, .musl_startup = true } }) |case| {
            const kind = case.kind;
            _ = try command(a, &.{ "zig", "cc", "-target", target, if (case.musl_startup) "-DMUSL_STARTUP" else "-UMUSL_STARTUP", if (kind == .executable) "-DFIXED_STARTUP" else "-UFIXED_STARTUP", "-fPIC", "-O1", "-fno-stack-protector", "-fno-asynchronous-unwind-tables", "-c", host_c, "-o", host_o });
            const spec: fast.HostSpec = .{
                .format = .elf,
                .target = if (arm64) .arm64musl else .x64musl,
                .link_abi = .freestanding,
                .platform_files_pre = &.{host_o},
                .platform_files_post = &.{},
                .platform_files_dir = dir,
                .link_items = &.{ "host.o", "app" },
                .bundle_hash = null,
                .provides = &provides,
                .keep = &keep,
                .host_required = &keep,
                .shared = kind == .shared,
                .pie = kind == .pie,
                .exports = if (kind == .shared) &provides else &.{},
            };
            const windows = if (kind.positionIndependent())
                try host_prelink.linkPositionIndependentObject(a, arm64, &layouts)
            else
                try host_prelink.linkObject(a, arm64, &provides);
            try tmp.dir.writeFile(io, .{ .sub_path = "windows.o", .data = windows });
            // Unit tests do not link the embedded LLD entrypoints. Invoke
            // the same link arguments through Zig's LLD subprocess instead.
            if (case.musl_startup) {
                // Zig supplies musl rcrt1.o and libc.a for this static PIE.
                // Unlike ordinary crt1.o, rcrt1 applies our extended RELA table.
                _ = try command(a, &.{ "zig", "cc", "-target", target, "-pie", "-Wl,--build-id", "-Wl,--gc-sections", "-Wl,-u,host_callback", "-Wl,-u,host_value", host_o, windows_o, "-o", linked_host });
            } else {
                const args = try @import("linker.zig").buildLinkArgs(&ctx, fast.elfLinkConfig(&spec, &.{windows_o}, linked_host, dir));
                var argv: std.ArrayList([]const u8) = .empty;
                try argv.append(a, "zig");
                try argv.appendSlice(a, args.items);
                _ = try command(a, argv.items);
            }
            const linked = try tmp.dir.readFileAlloc(io, "linked_host", a, .unlimited);
            const record = try host_prelink.finish(std.testing.allocator, linked, &provides, &keep, kind, if (kind.positionIndependent()) &layouts else &.{});
            defer std.testing.allocator.free(record);
            // Old records cannot be admitted under the new relocation policy.
            std.mem.writeInt(u32, record[4..8], host_prelink.format_version - 1, .little);
            try std.testing.expectError(error.InvalidRecord, host_prelink.View.init(record));
            std.mem.writeInt(u32, record[4..8], host_prelink.format_version, .little);
            const host = try host_prelink.View.init(record);
            try std.testing.expectEqual(kind == .shared, host.shared());
            try std.testing.expectEqual(kind.positionIndependent(), host.positionIndependent());
            try std.testing.expectEqual(kind.positionIndependent(), host.hasWindowSections());
            var output = try fast.OutputFile.create(a, dest);
            defer output.discard();
            try output.writeStart(host.host());
            var timing: patch.Timing = .{};
            var outcome: patch.Outcome = .{};
            try patch.link(std.testing.allocator, a, io, .{
                .host = host,
                .support = .{ .view = support_view },
                .units = &.{.{ .view = view }},
                .root = 0,
                .host_required = &keep,
            }, &output, null, &timing, &outcome);
            try output.publish(a);
            const exe = try tmp.dir.readFileAlloc(io, "program", a, .unlimited);
            try std.testing.expectEqual(@as(u16, if (kind.positionIndependent()) 3 else 2), read16(exe, 16));
            if (kind != .shared) try std.testing.expect(read64(exe, 24) != 0);
            const phoff: usize = @intCast(read64(exe, 32));
            for (0..read16(exe, 56)) |index| {
                const ph = phoff + index * 56;
                // All test workflows are static: no PT_INTERP.
                try std.testing.expect(std.mem.readInt(u32, exe[ph..][0..4], .little) != 3);
            }
            if (kind.positionIndependent()) {
                const dynamic: usize = @intCast(host.header.dynamic_offset);
                var at = dynamic;
                var pie_flag = false;
                while (read64(exe, at) != 0) : (at += 16) {
                    const tag = read64(exe, at);
                    try std.testing.expect(tag != 1 and tag != 22); // no DT_NEEDED or DT_TEXTREL
                    if (tag == 0x6ffffffb) pie_flag = read64(exe, at + 8) & 0x08000000 != 0; // DF_1_PIE
                }
                try std.testing.expectEqual(kind == .pie, pie_flag);
                const rela_size = read64(exe, dynamic + host.header.relasz_entry * 16 + 8);
                // Host pointers survive, and the table grows for both the
                // appended image and the pointer-bearing provides copy.
                try std.testing.expect(rela_size >= host.header.rela_size + 4 * 24);
                const rela_address = read64(exe, dynamic + host.header.rela_entry * 16 + 8);
                const first_app_ph = phoff + host.header.first_spare * 56;
                const rela_offset: usize = @intCast(read64(exe, first_app_ph + 8) + rela_address - read64(exe, first_app_ph + 16));
                try std.testing.expectEqualSlices(u8, host.host()[@intCast(host.header.rela_offset)..][0..@intCast(host.header.rela_size)], exe[rela_offset..][0..@intCast(host.header.rela_size)]);
                const rela_section: usize = @intCast(read64(exe, 40) + host.header.rela_section * 64);
                try std.testing.expectEqual(rela_address, read64(exe, rela_section + 16));
                try std.testing.expectEqual(rela_size, read64(exe, rela_section + 32));
                for (0..@intCast(rela_size / 24)) |index| {
                    const address = read64(exe, rela_offset + index * 24);
                    var writable = false;
                    for (0..read16(exe, 56)) |ph_index| {
                        const ph = phoff + ph_index * 56;
                        if (std.mem.readInt(u32, exe[ph..][0..4], .little) != 1) continue;
                        const flags = std.mem.readInt(u32, exe[ph + 4 ..][0..4], .little);
                        const start = read64(exe, ph + 16);
                        const end = start + read64(exe, ph + 40);
                        writable = writable or (flags & 2 != 0 and address >= start and address + 8 <= end);
                    }
                    try std.testing.expect(writable);
                }
            }
            if (kind != .shared and arm64 == (builtin.cpu.arch == .aarch64)) {
                var first: ?[]const u8 = null;
                var changed = false;
                for (0..5) |_| {
                    const address = try command(a, &.{dest});
                    try std.testing.expectEqual(@as(usize, 8), address.len);
                    if (first) |previous| {
                        changed = changed or !std.mem.eql(u8, previous, address);
                    } else first = address;
                }
                // The function-address seed follows executable ASLR.
                try std.testing.expectEqual(kind == .pie, changed);
            } else if (kind != .shared and arm64 and builtin.cpu.arch == .x86_64) {
                // Native CI covers ASLR; when QEMU is installed, also execute
                // the other architecture's startup and relocation path.
                const run = std.process.run(a, io, .{ .argv = &.{ "qemu-aarch64", dest } }) catch |err| switch (err) {
                    error.FileNotFound => continue,
                    else => return err,
                };
                if (run.term != .exited or run.term.exited != 0)
                    std.debug.print("AArch64 {s}, musl startup={}: {any}\n{s}\n", .{ @tagName(kind), case.musl_startup, run.term, run.stderr });
                try std.testing.expectEqual(std.process.Child.Term{ .exited = 0 }, run.term);
                try std.testing.expectEqual(@as(usize, 8), run.stdout.len);
            }
        }
    }
}

// A deliberately small, libc-free equivalent of rcrt1: no absolute pointer
// is used until it applies DT_RELA. This is an explicit startup input, not
// startup synthesized by the linker. Syscalls make the test freestanding.
const host_source =
    \\typedef unsigned long U;
    \\struct Value { const U *ptr; U (*call)(void); };
    \\extern U app_entry(void);
    \\extern const U app_scalar;
    \\extern struct Value app_value;
    \\U host_value = 17;
    \\U host_callback(void) { return 5; }
    \\static U (*volatile entry)(void) = app_entry;
    \\static const U *volatile scalar = &app_scalar;
    \\static struct Value *volatile value = &app_value;
    \\extern const unsigned char __ehdr_start[] __attribute__((visibility("hidden")));
    \\extern U _DYNAMIC[] __attribute__((visibility("hidden")));
    \\static long syscall3(long n, long a, long b, long c) {
    \\#if defined(__x86_64__)
    \\    register long rax __asm__("rax") = n;
    \\    __asm__ volatile("syscall" : "+a"(rax) : "D"(a), "S"(b), "d"(c) : "rcx", "r11", "memory");
    \\    return rax;
    \\#else
    \\    register long x8 __asm__("x8") = n;
    \\    register long x0 __asm__("x0") = a;
    \\    register long x1 __asm__("x1") = b;
    \\    register long x2 __asm__("x2") = c;
    \\    __asm__ volatile("svc 0" : "+r"(x0) : "r"(x8), "r"(x1), "r"(x2) : "memory");
    \\    return x0;
    \\#endif
    \\}
    \\static int host_main(void) {
    \\    int ok = entry() == 42 && *scalar == 7 && *value->ptr == 20 && value->call() == 5;
    \\    U address = (U)entry;
    \\#if defined(__x86_64__)
    \\    syscall3(1, 1, (long)&address, 8);
    \\#else
    \\    syscall3(64, 1, (long)&address, 8);
    \\#endif
    \\    return !ok;
    \\}
    \\#ifdef MUSL_STARTUP
    \\int main(void) { return host_main(); }
    \\#else
    \\__attribute__((noreturn, used)) void start_c(void) {
    \\#ifndef FIXED_STARTUP
    \\    U base = (U)__ehdr_start, rela = 0, size = 0;
    \\    if (__ehdr_start[16] == 3) {
    \\        for (U *d = _DYNAMIC; d[0]; d += 2) {
    \\            if (d[0] == 7) rela = d[1];
    \\            if (d[0] == 8) size = d[1];
    \\        }
    \\        for (U *r = (U *)(base + rela); (U)r < base + rela + size; r += 3)
    \\            *(U *)(base + r[0]) = base + r[2];
    \\    }
    \\#endif
    \\    int status = host_main();
    \\#if defined(__x86_64__)
    \\    syscall3(60, status, 0, 0);
    \\#else
    \\    syscall3(93, status, 0, 0);
    \\#endif
    \\    __builtin_unreachable();
    \\}
    \\#if defined(__x86_64__)
    \\__asm__(".global _start\n_start:\nxor %ebp,%ebp\nand $-16,%rsp\ncall start_c");
    \\#else
    \\__asm__(".global _start\n_start:\nbl start_c");
    \\#endif
    \\#endif
;

// The data prefix makes a provided value's image offset exceed its host
// window address, exercising relocation copying without unsigned underflow.
const app_source =
    \\typedef unsigned long U;
    \\struct Value { const U *ptr; U (*call)(void); };
    \\extern U host_value;
    \\extern U host_callback(void);
    \\static const U payload = 20;
    \\const U app_scalar = 7;
    \\static U padding[8192] __attribute__((used)) = { 1 };
    \\__attribute__((visibility("protected"))) struct Value app_value = { &payload, host_callback };
    \\static U *volatile host_ptr = &host_value;
    \\U app_entry(void) { return *host_ptr + *app_value.ptr + app_value.call(); }
;
