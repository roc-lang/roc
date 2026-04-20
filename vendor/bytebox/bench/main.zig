const std = @import("std");
const bytebox = @import("bytebox");
const Val = bytebox.Val;
const Timer = std.time.Timer;

pub const std_options: std.Options = .{
    .log_level = .info,
};

const Benchmark = struct {
    name: []const u8,
    filename: []const u8,
    param: i32,
};

fn lapTimerMs(timer: *std.time.Timer) f64 {
    const ns_elapsed: f64 = @as(f64, @floatFromInt(timer.read()));
    const ms_elapsed = ns_elapsed / 1000000.0;
    timer.reset();
    return ms_elapsed;
}

fn run(allocator: std.mem.Allocator, benchmark: Benchmark) !void {
    var cwd = std.fs.cwd();
    const wasm_data: []u8 = try cwd.readFileAlloc(allocator, benchmark.filename, 1024 * 1024 * 1);

    var timer = try Timer.start();

    var module_def = try bytebox.createModuleDefinition(allocator, .{});
    defer module_def.destroy();
    try module_def.decode(wasm_data);
    const ms_elapsed_decode: f64 = lapTimerMs(&timer);

    var module_instance = try bytebox.createModuleInstance(.Stack, module_def, allocator);
    defer module_instance.destroy();
    try module_instance.instantiate(.{});
    const ms_elapsed_instantiate: f64 = lapTimerMs(&timer);

    const handle = try module_instance.getFunctionHandle("run");
    var input = [1]Val{.{ .I32 = benchmark.param }};
    var output = [1]Val{.{ .I32 = 0 }};
    try module_instance.invoke(handle, &input, &output, .{});

    const ms_elapsed_invoke: f64 = lapTimerMs(&timer);
    std.log.info("{s}\n\tdecode: {d}ms\n\tinstantiate: {d}ms\n\trun took {d}ms\n", .{
        benchmark.name,
        ms_elapsed_decode,
        ms_elapsed_instantiate,
        ms_elapsed_invoke,
    });
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = gpa.allocator();

    const benchmarks = [_]Benchmark{
        .{
            // minimum decode+instantiate overhead
            .name = "add-one",
            .filename = "zig-out/bin/add-one.wasm",
            .param = 123456789,
        },
        .{
            // small code size execution with a few instructions
            .name = "fibonacci",
            .filename = "zig-out/bin/fibonacci.wasm",
            .param = 39,
        },
        .{
            // basic memory ops
            .name = "mandelbrot",
            .filename = "zig-out/bin/mandelbrot.wasm",
            .param = 512,
        },
        .{
            // generates a json document, parses it, then hashes the in-memory contents using stdlib
            .name = "json",
            .filename = "zig-out/bin/json.wasm",
            .param = 8192,
        },
    };

    for (benchmarks) |benchmark| {
        run(allocator, benchmark) catch |e| {
            std.log.err("{s} 'run' invocation failed with error: {}\n", .{ benchmark.name, e });
            return e;
        };
    }
}
