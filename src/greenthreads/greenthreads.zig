const std = @import("std");
const builtin = @import("builtin");
const os = std.posix;

const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const CONTEXT_SIZE = if (builtin.cpu.arch == .aarch64) 21 else 7;
const STACK_ALIGN = 16;

const PAGE_SIZE = switch (builtin.os.tag) {
    .macos => if (builtin.cpu.arch == .aarch64) 16384 else 4096,
    .linux => 4096,
    .windows => 4096,
    else => 4096,
};

pub const Config = struct {
    stack_size: usize = 1024 * 1024,
};

extern fn switch_context_impl(current: [*]u64, target: [*]u64) void;
comptime {
    if (builtin.cpu.arch == .aarch64) {
        asm (@embedFile("switch_context_aarch64.s"));
    } else if (builtin.cpu.arch == .x86_64) {
        asm (@embedFile("switch_context_x64.s"));
    }
}

threadlocal var global_store: ?*ThreadStore = null;

pub const GreenThread = struct {
    context: [CONTEXT_SIZE]u64,
    stack_memory: []u8,
    func: ?ThreadFunc = null,
    input: ?*anyopaque = null,
    output: ?*anyopaque = null,
    state: State = .ready,
};

pub const State = enum {
    ready,
    running,
    yielded,
    done,
};

pub const ThreadFunc = *const fn (*anyopaque, *anyopaque) callconv(.C) void;

pub const ThreadStore = struct {
    threads: ArrayListUnmanaged(GreenThread) = .{},
    allocator: Allocator,
    current_thread: ?usize = null,
    main_context: [CONTEXT_SIZE]u64 = std.mem.zeroes([CONTEXT_SIZE]u64),
    config: Config,

    pub fn init(allocator: Allocator) ThreadStore {
        return initWithConfig(allocator, .{});
    }
    
    pub fn initWithConfig(allocator: Allocator, config: Config) ThreadStore {
        return .{
            .allocator = allocator,
            .config = config,
        };
    }

    pub fn deinit(self: *ThreadStore) void {
        for (self.threads.items) |thread| {
            switch (builtin.os.tag) {
                .windows => {
                    const windows = std.os.windows;
                    windows.VirtualFree(thread.stack_memory.ptr, 0, windows.MEM_RELEASE);
                },
                else => os.munmap(@alignCast(thread.stack_memory)),
            }
        }
        self.threads.deinit(self.allocator);
    }

    pub fn spawn(self: *ThreadStore, func: ThreadFunc, input: *anyopaque, output: *anyopaque) !void {
        const guard_size = PAGE_SIZE;
        const stack_size = self.config.stack_size;
        const total_size = stack_size + guard_size;
        
        const memory = switch (builtin.os.tag) {
            .windows => blk: {
                const windows = std.os.windows;
                const mem = try windows.VirtualAlloc(
                    null,
                    total_size,
                    windows.MEM_COMMIT | windows.MEM_RESERVE,
                    windows.PAGE_READWRITE,
                );
                break :blk @as([*]u8, @ptrCast(mem))[0..total_size];
            },
            else => try os.mmap(
                null,
                total_size,
                os.PROT.READ | os.PROT.WRITE,
                .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
                -1,
                0,
            ),
        };
        errdefer switch (builtin.os.tag) {
            .windows => {
                const windows = std.os.windows;
                windows.VirtualFree(memory.ptr, 0, windows.MEM_RELEASE);
            },
            else => os.munmap(memory),
        };

        switch (builtin.os.tag) {
            .windows => {
                const windows = std.os.windows;
                var old_protect: windows.DWORD = undefined;
                const result = windows.VirtualProtect(
                    memory.ptr,
                    guard_size,
                    windows.PAGE_NOACCESS,
                    &old_protect,
                );
                if (result == 0) return error.MemoryProtectionFailed;
            },
            else => try os.mprotect(memory[0..guard_size], os.PROT.NONE),
        }

        var thread = GreenThread{
            .context = std.mem.zeroes([CONTEXT_SIZE]u64),
            .stack_memory = memory,
            .func = func,
            .input = input,
            .output = output,
            .state = .ready,
        };

        setupInitialContext(&thread);

        try self.threads.append(self.allocator, thread);
        const thread_index = self.threads.items.len - 1;

        self.runThread(thread_index);
    }

    fn setupInitialContext(thread: *GreenThread) void {
        const stack_end = thread.stack_memory.ptr + thread.stack_memory.len;
        var sp = @as([*]u8, @alignCast(stack_end));
        
        sp = @ptrFromInt(@intFromPtr(sp) & ~@as(usize, STACK_ALIGN - 1));

        if (builtin.cpu.arch == .aarch64) {
            const x19_index = 0;
            const fp_index = 18;
            const lr_index = 19;
            const sp_index = 20;
            
            thread.context[x19_index] = @intFromPtr(thread);
            thread.context[sp_index] = @intFromPtr(sp);
            thread.context[fp_index] = @intFromPtr(sp);
            thread.context[lr_index] = @intFromPtr(&threadWrapper);
        } else if (builtin.cpu.arch == .x86_64) {
            const rbx_index = 4;
            const bp_index = 5;
            const sp_index = 6;
            
            sp -= @sizeOf(u64);
            const return_addr_ptr = @as(*u64, @alignCast(@ptrCast(sp)));
            return_addr_ptr.* = @intFromPtr(&threadWrapper);
            
            thread.context[rbx_index] = @intFromPtr(thread);
            thread.context[sp_index] = @intFromPtr(sp);
            thread.context[bp_index] = @intFromPtr(sp);
        }
    }

    fn threadWrapper() callconv(.C) void {
        var thread: *GreenThread = undefined;
        if (builtin.cpu.arch == .aarch64) {
            asm volatile ("mov %[thread], x19" 
                : [thread] "=r" (thread)
            );
        } else if (builtin.cpu.arch == .x86_64) {
            asm volatile ("mov %%rbx, %[thread]" 
                : [thread] "=r" (thread)
            );
        }
        
        if (thread.func) |func| {
            func(thread.input.?, thread.output.?);
        }
        
        thread.state = .done;
        
        if (global_store) |store| {
            store.current_thread = null;
            switch_context_impl(&thread.context, &store.main_context);
        }
        
        unreachable;
    }

    fn runThread(self: *ThreadStore, thread_index: usize) void {
        if (thread_index >= self.threads.items.len) return;
        
        const thread = &self.threads.items[thread_index];
        thread.state = .running;
        
        global_store = self;
        
        const prev_thread = self.current_thread;
        self.current_thread = thread_index;
        
        if (prev_thread) |prev_idx| {
            if (prev_idx < self.threads.items.len) {
                switch_context_impl(&self.threads.items[prev_idx].context, &thread.context);
            }
        } else {
            switch_context_impl(&self.main_context, &thread.context);
        }
        
        self.current_thread = prev_thread;
    }

    pub fn yield(self: *ThreadStore) void {
        if (self.current_thread == null) return;
        
        const current_idx = self.current_thread.?;
        if (current_idx >= self.threads.items.len) return;
        
        const thread = &self.threads.items[current_idx];
        thread.state = .yielded;
        
        self.current_thread = null;
        switch_context_impl(&thread.context, &self.main_context);
    }

    pub fn resumeThread(self: *ThreadStore, thread_index: usize) void {
        if (thread_index >= self.threads.items.len) return;
        
        const thread = &self.threads.items[thread_index];
        if (thread.state != .yielded) return;
        
        self.runThread(thread_index);
    }
};

const testing = std.testing;

test "basic stack switching" {
    var store = ThreadStore.init(testing.allocator);
    defer store.deinit();

    const Output = struct {
        value: i32,
    };
    
    var output = Output{ .value = 0 };

    const testFunc = struct {
        fn compute(in: *anyopaque, out: *anyopaque) callconv(.C) void {
            _ = in;
            const typed_out = @as(*Output, @ptrCast(@alignCast(out)));
            typed_out.value = 42;
        }
    }.compute;

    var dummy: i32 = 0;
    try store.spawn(testFunc, @ptrCast(&dummy), @ptrCast(&output));
    
    try testing.expectEqual(@as(i32, 42), output.value);
}

test "thread with yield" {
    var store = ThreadStore.init(testing.allocator);
    defer store.deinit();

    const Context = struct {
        store: *ThreadStore,
        step: i32,
    };
    
    var ctx = Context{ 
        .store = &store,
        .step = 0,
    };

    const testFunc = struct {
        fn compute(in: *anyopaque, out: *anyopaque) callconv(.C) void {
            const context = @as(*Context, @ptrCast(@alignCast(in)));
            const output = @as(*i32, @ptrCast(@alignCast(out)));
            
            context.step = 1;
            output.* = 1;
            context.store.yield();
            
            context.step = 2;
            output.* = 2;
        }
    }.compute;

    var result: i32 = 0;
    try store.spawn(testFunc, @ptrCast(&ctx), @ptrCast(&result));
    
    // After spawn, thread should have yielded
    try testing.expectEqual(@as(i32, 1), ctx.step);
    try testing.expectEqual(@as(i32, 1), result);
    
    // Resume the thread
    store.resumeThread(0);
    try testing.expectEqual(@as(i32, 2), ctx.step);
    try testing.expectEqual(@as(i32, 2), result);
}

test "guard page catches stack overflow" {
    // Skip on unsupported platforms
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    
    const c = @cImport({
        @cInclude("signal.h");
        @cInclude("setjmp.h");
    });
    
    const TestState = struct {
        var jmp_buf: c.jmp_buf = undefined;
        var guard_hit: bool = false;
        
        fn handleFault(sig: c_int) callconv(.C) void {
            _ = sig;
            guard_hit = true;
            _ = c.longjmp(&jmp_buf, 1);
        }
    };
    
    // Install signal handlers
    var old_segv: c.struct_sigaction = undefined;
    var old_bus: c.struct_sigaction = undefined;
    
    var new_action: c.struct_sigaction = undefined;
    if (builtin.os.tag == .macos) {
        new_action = c.struct_sigaction{
            .__sigaction_u = .{ .__sa_handler = TestState.handleFault },
            .sa_flags = 0,
            .sa_mask = undefined,
        };
    } else {
        new_action = c.struct_sigaction{
            .sa_handler = TestState.handleFault,
            .sa_flags = 0,
            .sa_mask = undefined,
        };
    }
    
    _ = c.sigemptyset(&new_action.sa_mask);
    
    _ = c.sigaction(c.SIGSEGV, &new_action, &old_segv);
    defer _ = c.sigaction(c.SIGSEGV, &old_segv, null);
    
    _ = c.sigaction(c.SIGBUS, &new_action, &old_bus);
    defer _ = c.sigaction(c.SIGBUS, &old_bus, null);
    
    if (c.setjmp(&TestState.jmp_buf) == 0) {
        // Directly test our guard page setup
        const guard_size = PAGE_SIZE;
        const stack_size = 32 * 1024;
        const total_size = stack_size + guard_size;
        
        const memory = try os.mmap(
            null,
            total_size,
            os.PROT.READ | os.PROT.WRITE,
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        );
        defer os.munmap(@alignCast(memory));
        
        // Protect the guard page
        try os.mprotect(memory[0..guard_size], os.PROT.NONE);
        
        // Try to write to the guard page - should fault
        memory[0] = 0xFF;
        
        // Should not reach here
        try testing.expect(false);
    } else {
        // Guard page worked!
        try testing.expect(TestState.guard_hit);
    }
}

test "smaller stacks work" {
    const small_config = Config{ .stack_size = 64 * 1024 };
    var store = ThreadStore.initWithConfig(testing.allocator, small_config);
    defer store.deinit();

    var counter: i32 = 0;
    const testFunc = struct {
        fn compute(in: *anyopaque, out: *anyopaque) callconv(.C) void {
            _ = in;
            const typed_out = @as(*i32, @ptrCast(@alignCast(out)));
            typed_out.* += 1;
        }
    }.compute;

    for (0..5) |_| {
        var dummy: i32 = 0;
        try store.spawn(testFunc, @ptrCast(&dummy), @ptrCast(&counter));
    }
    
    try testing.expectEqual(@as(i32, 5), counter);
}

test "multiple yields and resumes" {
    var store = ThreadStore.init(testing.allocator);
    defer store.deinit();

    const Context = struct {
        store: *ThreadStore,
        counter: i32,
    };
    
    var ctx = Context{ 
        .store = &store,
        .counter = 0,
    };

    const testFunc = struct {
        fn compute(in: *anyopaque, out: *anyopaque) callconv(.C) void {
            const context = @as(*Context, @ptrCast(@alignCast(in)));
            const output = @as(*i32, @ptrCast(@alignCast(out)));
            
            context.counter += 1;
            output.* = context.counter;
            context.store.yield();
            
            context.counter += 10;
            output.* = context.counter;
            context.store.yield();
            
            context.counter += 100;
            output.* = context.counter;
        }
    }.compute;

    var result: i32 = 0;
    try store.spawn(testFunc, @ptrCast(&ctx), @ptrCast(&result));
    
    try testing.expectEqual(@as(i32, 1), ctx.counter);
    try testing.expectEqual(@as(i32, 1), result);
    
    store.resumeThread(0);
    try testing.expectEqual(@as(i32, 11), ctx.counter);
    try testing.expectEqual(@as(i32, 11), result);
    
    store.resumeThread(0);
    try testing.expectEqual(@as(i32, 111), ctx.counter);
    try testing.expectEqual(@as(i32, 111), result);
}