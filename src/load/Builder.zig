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
/// Module environments indexed by module ID
module_envs: std.AutoHashMap(ModuleId, *ModuleEnv),
/// Parse results indexed by module ID
parse_results: std.AutoHashMap(ModuleId, ParseResult),
/// Canonicalized results indexed by module ID
canonicalized_results: std.AutoHashMap(ModuleId, CanonicalizedModule),
/// Type checked results indexed by module ID
type_checked_results: std.AutoHashMap(ModuleId, TypeCheckedModule),
/// Workers (empty in single-threaded mode)
workers: []Worker,
/// Next module ID to assign
next_module_id: ModuleId = 0,
/// Mutex for thread-safe operations (null in single-threaded mode)
mutex: ?std.Thread.Mutex,

/// Stores parse result with metadata
pub const ParseResult = struct {
    ast: *parse.AST,
    module_path: []const u8,
};

/// Stores canonicalized module data
pub const CanonicalizedModule = struct {
    /// The canonicalized IR
    cir: *canonicalize.CIR,
    /// Number of errors found during canonicalization
    error_count: u32,
    /// Number of warnings found during canonicalization
    warning_count: u32,
    /// Whether this module was loaded from cache
    was_cached: bool = false,
};

/// Stores type checked module data
pub const TypeCheckedModule = struct {
    /// The type checked results
    solved_types: struct {
        problems: @import("../check/check_types/problem.zig").Store,
        types: types.Store,
    },
    /// Number of type errors
    type_error_count: u32,
};

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
        .module_envs = std.AutoHashMap(ModuleId, *ModuleEnv).init(config.allocator),
        .parse_results = std.AutoHashMap(ModuleId, ParseResult).init(config.allocator),
        .canonicalized_results = std.AutoHashMap(ModuleId, CanonicalizedModule).init(config.allocator),
        .type_checked_results = std.AutoHashMap(ModuleId, TypeCheckedModule).init(config.allocator),
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

    // Clean up type checked results first (no dependency on module environments)
    var type_iter = self.type_checked_results.iterator();
    while (type_iter.next()) |entry| {
        entry.value_ptr.solved_types.problems.deinit(self.config.allocator);
    }
    self.type_checked_results.deinit();

    // Clean up canonicalized results (CIR references module environments)
    var canon_iter = self.canonicalized_results.iterator();
    while (canon_iter.next()) |entry| {
        entry.value_ptr.cir.deinit();
        self.config.allocator.destroy(entry.value_ptr.cir);
    }
    self.canonicalized_results.deinit();

    // Clean up parse results
    var parse_iter = self.parse_results.iterator();
    while (parse_iter.next()) |entry| {

        // The AST owns the source memory, so we need to be careful about cleanup order
        const ast_ptr = entry.value_ptr.ast;
        const module_path = entry.value_ptr.module_path;

        // First deinit the AST (which frees its internal structures)
        ast_ptr.deinit(self.config.allocator);

        // Then free the AST pointer itself
        self.config.allocator.destroy(ast_ptr);

        // Finally free the module path
        self.config.allocator.free(module_path);
    }
    self.parse_results.deinit();

    // Clean up module environments last (after nothing references them)
    var env_iter = self.module_envs.iterator();
    while (env_iter.next()) |entry| {
        entry.value_ptr.*.deinit();
        self.config.allocator.destroy(entry.value_ptr.*);
    }
    self.module_envs.deinit();

    // Clean up task queue
    self.task_queue.deinit();
    self.config.allocator.destroy(self.task_queue);

    // Cache manager doesn't need explicit cleanup - it just holds config
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
        if (self.task_queue.pop()) |task| {
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
        .parse => |parse_task| {
            try self.parseModule(parse_task.module_id, parse_task.source, parse_task.path);
            self.config.allocator.free(parse_task.source);
            self.config.allocator.free(parse_task.path);
        },
        .canonicalize => |canon| {
            try self.canonicalizeModule(canon.module_id);
        },
        .type_check => |type_check_task| {
            try self.typeCheckModule(type_check_task.module_id);
        },
    }
}

/// Load a file from disk
fn loadFile(self: *Self, path: []const u8, module_id: ModuleId) !void {
    // Read the file contents
    const source = try self.config.filesystem.readFile(path, self.config.allocator);
    defer self.config.allocator.free(source);

    // Try to load from cache
    const cache_result = try self.cache_manager.lookup(source, @import("build_options").compiler_version);

    switch (cache_result) {
        .hit => |cached_data| {
            // Cache hit! The cached_data contains a ProcessResult that was restored from cache

            // Store the module environment (already allocated by cache restore)
            try self.storeModuleEnv(module_id, cached_data.result.cir.env);

            // Store the canonicalized result with the CIR pointer
            try self.storeCanonicalizedResult(module_id, cached_data.result.cir, true);

            // Free the unused source and reports from cache
            self.config.allocator.free(cached_data.result.source);
            self.config.allocator.free(cached_data.result.reports);

            // Queue type checking task if there are no errors
            if (cached_data.error_count == 0) {
                const type_check_task = Task{
                    .kind = .{ .type_check = .{
                        .module_id = module_id,
                    } },
                };
                try self.task_queue.push(type_check_task);
            }
            return;
        },
        .miss, .invalid => {
            // Cache miss or invalid - continue with normal parsing
        },
    }

    // Not in cache, need to parse
    const parse_task = Task{
        .kind = .{ .parse = .{
            .module_id = module_id,
            .source = try self.config.allocator.dupe(u8, source),
            .path = try self.config.allocator.dupe(u8, path),
        } },
    };
    try self.task_queue.push(parse_task);
}

/// Parse a module
fn parseModule(self: *Self, module_id: ModuleId, source: []const u8, path: []const u8) !void {
    // Create module environment
    var module_env = try self.config.allocator.create(ModuleEnv);
    module_env.* = ModuleEnv.init(self.config.allocator);
    errdefer {
        module_env.deinit();
        self.config.allocator.destroy(module_env);
    }

    // Calculate line starts
    try module_env.calcLineStarts(source);

    // Parse the source - this returns an owned AST
    const parse_result = parse.parse(module_env, source);

    // Store the module environment
    try self.storeModuleEnv(module_id, module_env);

    // Check for parse errors
    const has_parse_errors = parse_result.tokenize_diagnostics.items.len > 0 or
        parse_result.parse_diagnostics.items.len > 0;

    // Store the parse result - this transfers ownership of the AST
    try self.storeParseResult(module_id, parse_result, path);

    // Only queue canonicalization if there are no parse errors
    if (!has_parse_errors) {
        const canon_task = Task{
            .kind = .{ .canonicalize = .{
                .module_id = module_id,
            } },
        };
        try self.task_queue.push(canon_task);
    }
}

/// Canonicalize a module
fn canonicalizeModule(self: *Self, module_id: ModuleId) !void {
    // Get the module environment and parse result
    const module_env = self.getModuleEnv(module_id) orelse return error.ModuleNotFound;
    const parse_result = self.getParseResult(module_id) orelse return error.ParseResultNotFound;

    // Create CIR (Canonicalized IR) on the heap so it persists
    const cir = try self.config.allocator.create(canonicalize.CIR);
    errdefer self.config.allocator.destroy(cir);

    // Initialize with the module path as the module name
    const module_name = try self.config.allocator.dupe(u8, parse_result.module_path);
    defer self.config.allocator.free(module_name);

    cir.* = canonicalize.CIR.init(module_env, module_name);
    errdefer cir.deinit();

    // Create the canonicalizer
    var canonicalizer = try canonicalize.init(cir, @constCast(parse_result.ast), null);
    defer canonicalizer.deinit();

    // Canonicalize the file
    try canonicalizer.canonicalizeFile();

    // Store the canonicalized result
    try self.storeCanonicalizedResult(module_id, cir, false);

    // Store to cache for future use
    // Count errors and warnings from diagnostics
    const diagnostics = cir.getDiagnostics();
    defer cir.env.gpa.free(diagnostics);

    var error_count: u32 = 0;
    var warning_count: u32 = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .shadowing_warning => warning_count += 1,
            else => error_count += 1,
        }
    }

    const process_result = coordinate_simple.ProcessResult{
        .cir = cir,
        .reports = &[_]reporting.Report{}, // Reports are not cached
        .source = parse_result.ast.source,
        .error_count = error_count,
        .warning_count = warning_count,
        .was_cached = false,
    };
    try self.cache_manager.store(parse_result.ast.source, @import("build_options").compiler_version, &process_result);

    // Queue type checking task if there are no errors
    if (error_count == 0) {
        const type_check_task = Task{
            .kind = .{ .type_check = .{
                .module_id = module_id,
            } },
        };
        try self.task_queue.push(type_check_task);
    }
}

/// Type check a module
fn typeCheckModule(self: *Self, module_id: ModuleId) !void {
    // Get the canonicalized result
    const canon_result = self.getCanonicalizedResult(module_id) orelse return error.CanonicalizedResultNotFound;

    // For now, we'll create a simple type checking context
    // In a real implementation, we'd need to handle imports and other modules
    const empty_modules: []const *canonicalize.CIR = &.{};

    var type_checker = try check_types.init(
        self.config.allocator,
        &canon_result.cir.env.types,
        @constCast(canon_result.cir),
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

    // Store the type checked result
    const type_checked = TypeCheckedModule{
        .solved_types = .{
            .problems = problems,
            .types = canon_result.cir.env.types,
        },
        .type_error_count = @intCast(type_error_count),
    };
    try self.storeTypeCheckedResult(module_id, type_checked);

    // Now we can safely deinit the type_checker
    type_checker.deinit();
}

/// Store a module environment
pub fn storeModuleEnv(self: *Self, module_id: ModuleId, env: *ModuleEnv) !void {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    try self.module_envs.put(module_id, env);
}

/// Get a module environment
pub fn getModuleEnv(self: *Self, module_id: ModuleId) ?*ModuleEnv {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    return self.module_envs.get(module_id);
}

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
fn storeParseResult(self: *Self, module_id: ModuleId, ast: parse.AST, module_path: []const u8) !void {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    // Create storage for the AST
    const ast_ptr = try self.config.allocator.create(parse.AST);

    // Move the AST data - this transfers ownership
    ast_ptr.* = ast;

    const result = ParseResult{
        .ast = ast_ptr,
        .module_path = try self.config.allocator.dupe(u8, module_path),
    };

    try self.parse_results.put(module_id, result);
}

/// Get a parse result
pub fn getParseResult(self: *Self, module_id: ModuleId) ?ParseResult {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    return self.parse_results.get(module_id);
}

/// Store a canonicalized result
fn storeCanonicalizedResult(self: *Self, module_id: ModuleId, cir: *canonicalize.CIR, was_cached: bool) !void {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    // Count diagnostics for error and warning tracking
    const diagnostics = cir.getDiagnostics();
    defer cir.env.gpa.free(diagnostics);

    var error_count: u32 = 0;
    var warning_count: u32 = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .shadowing_warning => warning_count += 1,
            else => error_count += 1,
        }
    }

    const result = CanonicalizedModule{
        .cir = cir,
        .error_count = error_count,
        .warning_count = warning_count,
        .was_cached = was_cached,
    };

    try self.canonicalized_results.put(module_id, result);
}

/// Get a canonicalized result
pub fn getCanonicalizedResult(self: *Self, module_id: ModuleId) ?*const CanonicalizedModule {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    if (self.canonicalized_results.getPtr(module_id)) |ptr| {
        return ptr;
    }
    return null;
}

/// Store a type checked result
fn storeTypeCheckedResult(self: *Self, module_id: ModuleId, result: TypeCheckedModule) !void {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    try self.type_checked_results.put(module_id, result);
}

/// Get a type checked result
pub fn getTypeCheckedResult(self: *Self, module_id: ModuleId) ?*const TypeCheckedModule {
    if (self.mutex) |*mutex| {
        mutex.lock();
        defer mutex.unlock();
    }

    if (self.type_checked_results.getPtr(module_id)) |ptr| {
        return ptr;
    }
    return null;
}

const builtin = @import("builtin");
