//! Builder abstraction for loading and building Roc modules
//! Supports single-threaded, multi-threaded, and test modes

const std = @import("std");
const base = @import("../base.zig");
const cache = @import("../cache/mod.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const Filesystem = @import("../fs/Filesystem.zig");
const ModuleEnv = base.ModuleEnv;
const CacheModule = cache.CacheModule;
const CacheManager = cache.CacheManager;
const Worker = @import("Worker.zig");
const Task = @import("Task.zig");
const coordinate_simple = @import("../coordinate_simple.zig");
const reporting = @import("../reporting.zig");
const check_types = @import("../check/check_types.zig");
const types = @import("../types.zig");
const problem = @import("../check/check_types/problem.zig");

const Allocator = std.mem.Allocator;

/// The mode in which the builder operates
pub const Mode = enum {
    /// Single-threaded mode with no synchronization primitives
    single_threaded,
    /// Multi-threaded mode with real threads
    multi_threaded,
    // Test mode where we can control thread execution
    test_mode,
};

/// Configuration for the builder
pub const Config = struct {
    /// The allocator to use for all allocations
    allocator: Allocator,
    /// The filesystem abstraction to use
    filesystem: Filesystem,
    /// The mode to operate in
    mode: Mode,
    /// Number of worker threads (ignored in single-threaded mode)
    thread_count: usize = 4,
    /// Cache configuration
    cache_config: cache.CacheConfig,
};

/// The main builder structure
const Self = @This();

/// Configuration
config: Config,
/// Cache manager for module caching
cache_manager: CacheManager,
/// Task queue for work items
task_queue: *TaskQueue,
/// === Builder State ===
/// Modules indexed by module ID
modules: std.AutoHashMap(ModuleId, Module),
/// Workers (empty in single-threaded mode)
workers: []Worker,
/// Next module ID to assign
next_module_id: ModuleId = 0,
/// Mutex for thread-safe operations (null in single-threaded mode)
mutex: ?std.Thread.Mutex,
/// === Data Structures ===
/// Compilation phase for a module
pub const ModulePhase = enum {
    /// Module has been created but not yet parsed
    created,
    /// Module has been parsed
    parsed,
    /// Module has been canonicalized
    canonicalized,
    /// Module has been type checked
    type_checked,
};

/// Phase-specific data for a module
pub const PhaseData = union(ModulePhase) {
    created: void,
    parsed: struct {
        ast: *parse.AST,
    },
    canonicalized: struct {
        ast: *parse.AST, // Keep AST for error reporting
        cir: *canonicalize.CIR,
        error_count: u32,
        warning_count: u32,
        was_cached: bool,
        diagnostics: []canonicalize.CIR.Diagnostic,
    },
    type_checked: struct {
        ast: *parse.AST, // Keep AST for error reporting
        cir: *canonicalize.CIR, // Keep CIR for codegen
        canonicalize_error_count: u32,
        canonicalize_warning_count: u32,
        solved_types: struct {
            problems: @import("../check/check_types/problem.zig").Store,
            types: types.Store,
        },
        type_error_count: u32,
    },
};

/// Unified module representation
pub const Module = struct {
    /// The module environment (shared across all phases)
    env: *ModuleEnv,
    /// Current phase and phase-specific data
    phase_data: PhaseData,
    /// Module path (reference to path owned by ModuleEnv)
    module_path: []const u8,
};

/// Unique identifier for a module in the build system
pub const ModuleId = u32;

const TaskQueue = switch (builtin.single_threaded) {
    true => SingleThreadedQueue,
    false => MultiThreadedQueue,
};

const SingleThreadedQueue = struct {
    tasks: std.ArrayList(Task),

    pub fn init(allocator: Allocator) SingleThreadedQueue {
        return .{
            .tasks = std.ArrayList(Task).init(allocator),
        };
    }

    pub fn deinit(self: *SingleThreadedQueue) void {
        self.tasks.deinit();
    }

    pub fn push(self: *SingleThreadedQueue, task: Task) !void {
        try self.tasks.append(task);
    }

    pub fn pop(self: *SingleThreadedQueue) ?Task {
        return self.tasks.pop();
    }

    pub fn isEmpty(self: *SingleThreadedQueue) bool {
        return self.tasks.items.len == 0;
    }
};

const MultiThreadedQueue = struct {
    tasks: std.ArrayList(Task),
    mutex: std.Thread.Mutex,
    condition: std.Thread.Condition,

    pub fn init(allocator: Allocator) MultiThreadedQueue {
        return .{
            .tasks = std.ArrayList(Task).init(allocator),
            .mutex = .{},
            .condition = .{},
        };
    }

    pub fn deinit(self: *MultiThreadedQueue) void {
        self.tasks.deinit();
    }

    pub fn push(self: *MultiThreadedQueue, task: Task) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        try self.tasks.append(task);
        self.condition.signal();
    }

    pub fn pop(self: *MultiThreadedQueue) ?Task {
        self.mutex.lock();
        defer self.mutex.unlock();

        return self.tasks.pop();
    }

    pub fn isEmpty(self: *MultiThreadedQueue) bool {
        self.mutex.lock();
        defer self.mutex.unlock();

        return self.tasks.items.len == 0;
    }

    pub fn wait(self: *MultiThreadedQueue) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        while (self.tasks.items.len == 0) {
            self.condition.wait(&self.mutex);
        }
    }
};

/// Initialize a new builder
pub fn init(config: Config) !Self {
    const cache_manager = CacheManager.init(config.allocator, config.cache_config, config.filesystem);

    const task_queue = try config.allocator.create(TaskQueue);
    task_queue.* = TaskQueue.init(config.allocator);

    const workers = try config.allocator.alloc(Worker, if (config.mode == .single_threaded) 0 else config.thread_count);
    errdefer config.allocator.free(workers);

    for (workers, 0..) |*worker, i| {
        worker.* = Worker.init(config.allocator, i);
    }

    const mutex = switch (config.mode) {
        .single_threaded => null,
        .multi_threaded, .test_mode => std.Thread.Mutex{},
    };

    return Self{
        .config = config,
        .cache_manager = cache_manager,
        .task_queue = task_queue,
        .modules = std.AutoHashMap(ModuleId, Module).init(config.allocator),
        .workers = workers,
        .mutex = mutex,
    };
}

/// Deinitialize the builder
pub fn deinit(self: *Self) void {
    // Clean up workers
    for (self.workers) |*worker| {
        worker.deinit();
    }
    self.config.allocator.free(self.workers);

    // Clean up modules
    var module_iter = self.modules.iterator();
    while (module_iter.next()) |entry| {
        var module = entry.value_ptr;

        // Clean up phase-specific data
        switch (module.phase_data) {
            .created => {},
            .parsed => |data| {
                data.ast.deinit(self.config.allocator);
                self.config.allocator.destroy(data.ast);
            },
            .canonicalized => |data| {
                data.ast.deinit(self.config.allocator);
                self.config.allocator.destroy(data.ast);
                data.cir.deinit();
                self.config.allocator.destroy(data.cir);
                self.config.allocator.free(data.diagnostics);
            },
            .type_checked => |data| {
                data.ast.deinit(self.config.allocator);
                self.config.allocator.destroy(data.ast);
                data.cir.deinit();
                self.config.allocator.destroy(data.cir);
                @constCast(&data.solved_types.problems).deinit(self.config.allocator);
            },
        }

        // Clean up module environment
        module.env.deinit();
        self.config.allocator.destroy(module.env);
    }
    self.modules.deinit();

    // Clean up task queue
    self.task_queue.deinit();
    self.config.allocator.destroy(self.task_queue);

    // Cache manager doesn't need explicit cleanup - it just holds config
}

/// Get a module by ID
pub fn getModule(self: *Self, module_id: ModuleId) ?*Module {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    return self.modules.getPtr(module_id);
}

/// Update a module's phase
fn updateModulePhase(self: *Self, module_id: ModuleId, new_phase_data: PhaseData) !void {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    const module = self.modules.getPtr(module_id) orelse return error.ModuleNotFound;
    module.phase_data = new_phase_data;
}

/// Queue a task
fn queueTask(self: *Self, task_kind: Task.Kind) !void {
    const task = Task{ .kind = task_kind };
    try self.task_queue.push(task);
}

/// Build modules starting from the given source file path
pub fn build(self: *Self, source_path: []const u8) !void {
    // Create initial task to load the root module
    const root_task = Task{
        .kind = .{ .load_file = .{
            .path = try self.config.allocator.dupe(u8, source_path),
            .module_id = self.allocateModuleId(),
        } },
    };

    try self.task_queue.push(root_task);

    // Process tasks based on mode
    switch (self.config.mode) {
        .single_threaded => try self.processSingleThreaded(),
        .multi_threaded => try self.processMultiThreaded(),
        .test_mode => {
            // In test mode, we don't automatically process tasks
            // Tests will manually drive the processing
        },
    }
}

/// Process tasks in single-threaded mode
fn processSingleThreaded(self: *Self) !void {
    while (!self.task_queue.isEmpty()) {
        if (self.getNextTask()) |task| {
            try self.processTask(task);
        }
    }
}

/// Process tasks in multi-threaded mode
fn processMultiThreaded(self: *Self) !void {
    // Start worker threads
    const threads = try self.config.allocator.alloc(std.Thread, self.workers.len);
    defer self.config.allocator.free(threads);

    for (threads, 0..) |*thread, i| {
        thread.* = try std.Thread.spawn(.{}, Worker.run, .{ &self.workers[i], self });
    }

    // Wait for all work to complete
    self.waitForCompletion();

    // Signal workers to stop
    for (self.workers) |*worker| {
        worker.stop();
    }

    // Join all threads
    for (threads) |thread| {
        thread.join();
    }
}

/// Wait for all tasks to complete
fn waitForCompletion(self: *Self) void {
    while (true) {
        self.mutex.?.lock();
        const is_empty = self.task_queue.isEmpty();
        const all_idle = for (self.workers) |*worker| {
            if (!worker.isIdle()) break false;
        } else true;
        self.mutex.?.unlock();

        if (is_empty and all_idle) break;

        std.time.sleep(1_000_000); // 1ms
    }
}

/// Process a single task
pub fn processTask(self: *Self, task: Task) !void {
    switch (task.kind) {
        .load_file => |load| {
            try self.loadFile(load.path, load.module_id);
            self.config.allocator.free(load.path);
        },
        .canonicalize => |canon| {
            try self.canonicalizeModule(canon.module_id);
        },
        .type_check => |tc| {
            try self.typeCheckModule(tc.module_id);
        },
    }
}

/// Load a file from disk
fn loadFile(self: *Self, path: []const u8, module_id: ModuleId) !void {
    // Read the file contents
    const source = try self.config.filesystem.readFile(path, self.config.allocator);
    errdefer self.config.allocator.free(source);

    // Try to load from cache
    const cache_result = try self.cache_manager.lookup(source, path, @import("build_options").compiler_version);

    switch (cache_result) {
        .hit => |cached_data| {
            // Cache hit! The cached_data contains a ProcessResult that was restored from cache

            // Create module with cached data
            const module = Module{
                .env = cached_data.result.cir.env,
                .phase_data = .{
                    .canonicalized = .{
                        .ast = undefined, // AST not available from cache
                        .cir = cached_data.result.cir,
                        .error_count = cached_data.error_count,
                        .warning_count = cached_data.warning_count,
                        .was_cached = true,
                        .diagnostics = &[_]canonicalize.CIR.Diagnostic{}, // No diagnostics from cache
                    },
                },
                .module_path = cached_data.result.cir.env.module_path,
            };

            if (self.mutex) |*mutex| {
                mutex.lock();
                defer mutex.unlock();
            }

            try self.modules.put(module_id, module);

            // Free the reports from cache
            self.config.allocator.free(cached_data.result.reports);
            // Don't free cached_data.result.source - it might be owned by the cached ModuleEnv
            // Free our locally read source since we loaded from cache
            self.config.allocator.free(source);

            // Queue type checking if no errors
            if (cached_data.error_count == 0) {
                try self.queueTask(.{ .type_check = .{ .module_id = module_id } });
            }
            return;
        },
        .miss => {
            // Cache miss - create module and parse
            // Don't free source here - createAndParseModule takes ownership
            try self.createAndParseModule(module_id, source, path);
            return;
        },
        .invalid => {
            // Cache invalid - continue without caching
            // Don't free source here - createAndParseModule takes ownership
            try self.createAndParseModule(module_id, source, path);
            return;
        },
    }
}

/// Create a module and parse it
pub fn createAndParseModule(self: *Self, module_id: ModuleId, source: []const u8, path: []const u8) !void {
    // Create module environment
    // Duplicate path since the caller will free it
    const owned_path = try self.config.allocator.dupe(u8, path);

    var module_env = try self.config.allocator.create(ModuleEnv);
    module_env.* = ModuleEnv.init(self.config.allocator, source, owned_path);
    errdefer {
        module_env.deinit();
        self.config.allocator.destroy(module_env);
    }

    // Create module in created phase
    const module = Module{
        .env = module_env,
        .phase_data = .{ .created = {} },
        .module_path = module_env.module_path,
    };

    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    try self.modules.put(module_id, module);

    // Now parse the module
    try self.parseModule(module_id);
}

/// Parse a module
fn parseModule(self: *Self, module_id: ModuleId) !void {
    // Get the module
    const module = self.getModule(module_id) orelse return error.ModuleNotFound;

    // Ensure module is in created phase
    switch (module.phase_data) {
        .created => {},
        else => return error.InvalidPhase,
    }

    // Calculate line starts
    try module.env.calcLineStarts(module.env.source);

    // Parse the source - AST references ModuleEnv which owns the source
    const parse_result = parse.parse(module.env, module.env.source);

    // Create storage for the AST
    const ast_ptr = try self.config.allocator.create(parse.AST);
    ast_ptr.* = parse_result;

    // Check for parse errors
    const has_parse_errors = ast_ptr.tokenize_diagnostics.items.len > 0 or
        ast_ptr.parse_diagnostics.items.len > 0;

    // Update module to parsed phase
    try self.updateModulePhase(module_id, .{ .parsed = .{
        .ast = ast_ptr,
    } });

    // Only queue canonicalization if there are no parse errors
    if (!has_parse_errors) {
        try self.queueTask(.{ .canonicalize = .{ .module_id = module_id } });
    }
}

/// Canonicalize a module
fn canonicalizeModule(self: *Self, module_id: ModuleId) !void {
    // Get the module
    const module = self.getModule(module_id) orelse return error.ModuleNotFound;

    // Ensure module is in parsed phase
    const parsed_data = switch (module.phase_data) {
        .parsed => |data| data,
        else => return error.InvalidPhase,
    };

    // Create CIR (Canonicalized IR) on the heap so it persists
    const cir = try self.config.allocator.create(canonicalize.CIR);
    errdefer self.config.allocator.destroy(cir);

    // Initialize with the module path as the module name
    cir.* = canonicalize.CIR.init(module.env, module.module_path);

    // Create the canonicalizer
    var canonicalizer = try canonicalize.init(cir, @constCast(parsed_data.ast), null);
    defer canonicalizer.deinit();

    // Canonicalize the file
    try canonicalizer.canonicalizeFile();

    // Get diagnostics (now non-destructive with our CIR fix)
    const diagnostics = cir.getDiagnostics();

    var error_count: u32 = 0;
    var warning_count: u32 = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .shadowing_warning => warning_count += 1,
            else => error_count += 1,
        }
    }

    // Make a copy of the diagnostics since they're owned by the CIR
    const diagnostics_copy = try self.config.allocator.alloc(canonicalize.CIR.Diagnostic, diagnostics.len);
    @memcpy(diagnostics_copy, diagnostics);

    // Update module to canonicalized phase
    try self.updateModulePhase(module_id, .{ .canonicalized = .{
        .ast = parsed_data.ast,
        .cir = cir,
        .error_count = error_count,
        .warning_count = warning_count,
        .was_cached = false,
        .diagnostics = diagnostics_copy,
    } });

    // Store to cache for future use
    const process_result = coordinate_simple.ProcessResult{
        .cir = cir,
        .reports = &[_]reporting.Report{}, // Reports are not cached
        .source = module.env.source,
        .error_count = error_count,
        .warning_count = warning_count,
        .was_cached = false,
    };
    try self.cache_manager.store(module.env.source, @import("build_options").compiler_version, &process_result);

    // Queue type checking task if there are no errors
    if (error_count == 0) {
        try self.queueTask(.{ .type_check = .{ .module_id = module_id } });
    }
}

/// Type check a module
fn typeCheckModule(self: *Self, module_id: ModuleId) !void {
    // Get the module
    const module = self.getModule(module_id) orelse return error.ModuleNotFound;

    // Ensure module is in canonicalized phase
    const canon_data = switch (module.phase_data) {
        .canonicalized => |data| data,
        else => return error.InvalidPhase,
    };

    // For now, we'll create a simple type checking context
    // In a real implementation, we'd need to handle imports and other modules
    const empty_modules: []const *canonicalize.CIR = &.{};

    var type_checker = try check_types.init(
        self.config.allocator,
        &module.env.types,
        @constCast(canon_data.cir),
        empty_modules,
    );

    // Check all definitions
    try type_checker.checkDefs();

    // Count type errors from problems
    // Count type errors - for now assume all problems are errors
    // In a real implementation, we'd need to distinguish between errors and warnings
    const type_error_count: u32 = @intCast(type_checker.problems.problems.items.len);

    // Move ownership of problems from type_checker to avoid double-free
    const problems = type_checker.problems;
    type_checker.problems = problem.Store.initCapacity(self.config.allocator, 64);

    // Update module to type checked phase
    try self.updateModulePhase(module_id, .{ .type_checked = .{
        .ast = canon_data.ast,
        .cir = canon_data.cir,
        .canonicalize_error_count = canon_data.error_count,
        .canonicalize_warning_count = canon_data.warning_count,
        .solved_types = .{
            .problems = problems,
            .types = module.env.types,
        },
        .type_error_count = type_error_count,
    } });

    // Now we can safely deinit the type_checker
    type_checker.deinit();
}

/// Store a module environment
/// Run a single task (for worker threads)
/// Allocate a new module ID
fn allocateModuleId(self: *Self) ModuleId {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    const id = self.next_module_id;
    self.next_module_id += 1;
    return id;
}

/// Get the next task (for workers)
pub fn getNextTask(self: *Self) ?Task {
    return self.task_queue.pop();
}

/// Wait for a task to be available (for workers in multi-threaded mode)
pub fn waitForTask(self: *Self) void {
    if (self.config.mode == .multi_threaded) {
        self.task_queue.wait();
    }
}

/// Store a parse result - takes ownership of the AST
const builtin = @import("builtin");
