//! Command line argument parsing for the CLI
//!
//! Each subcommand is one `Command` declaration: the flags and positional
//! arguments that fill its args struct, and the prose of its help. The parser
//! and the help text are both generated from that declaration.
const std = @import("std");
const base = @import("base");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const mem = std.mem;
const install = @import("install.zig");
const linker = @import("linker.zig");
const RocTarget = @import("target.zig").RocTarget;
const ResolutionConfig = @import("compile").package_resolution.Config;

const bytes_per_mb = 1024 * 1024;

const SpecializationStrategy = base.SpecializationStrategy;

/// Errors that can occur while parsing CLI arguments.
pub const ParseError = Allocator.Error || std.Io.Dir.OpenError || std.Io.Dir.Iterator.Error;

/// The core type representing a parsed command
/// We could use anonymous structs for the argument types instead of defining one for each command to be more concise,
/// but defining a struct per command means that we can easily take that type and pass it into the function that implements each command.
pub const CliArgs = union(enum) {
    run: RunArgs,
    check: CheckArgs,
    build: BuildArgs,
    fmt: FormatArgs,
    test_cmd: TestArgs,
    bundle: BundleArgs,
    unbundle: UnbundleArgs,
    repl: ReplArgs,
    glue: GlueArgs,
    version,
    docs: DocsArgs,
    deps: DepsArgs,
    bump: BumpArgs,
    install: InstallArgs,
    experimental_lsp: ExperimentalLspArgs,
    help: []const u8,
    licenses,
    problem: ArgProblem,

    pub fn deinit(self: CliArgs, alloc: mem.Allocator) void {
        const tag = std.meta.activeTag(self);
        if (tag == .fmt) alloc.free(self.fmt.paths);
        if (tag == .run) alloc.free(self.run.app_args);
        if (tag == .bundle) alloc.free(self.bundle.paths);
        if (tag == .unbundle) alloc.free(self.unbundle.paths);
    }
};

/// Parsed command plus CLI-wide output settings.
pub const ParsedArgs = struct {
    command: CliArgs,
    no_color: bool,
};

/// Errors that can occur due to bad input while parsing the arguments
pub const ArgProblem = union(enum) {
    missing_flag_value: struct {
        flag: []const u8,
    },
    unexpected_argument: struct { cmd: []const u8, arg: []const u8 },
    // Bare `roc <shorthand>` is rejected so the subcommand namespace can never
    // be conflated with the user-chosen shorthand namespace; running an
    // installed shorthand requires the explicit `roc run` subcommand.
    shorthand_requires_run: struct { name: []const u8 },
    invalid_flag_value: struct {
        value: []const u8,
        flag: []const u8,
        valid_options: []const u8,
    },
};

/// The optimization strategy for the compilation of a Roc program
pub const OptLevel = enum {
    dev,
    interpreter,
    speed,
    size,

    /// What choosing the level means, for the help of flags that take one.
    fn description(self: OptLevel) []const u8 {
        return switch (self) {
            .dev => "native dev backend, fast compilation",
            .interpreter => "interpreted, no code generation",
            .speed => "LLVM, optimized for execution speed",
            .size => "LLVM, optimized for binary size",
        };
    }

    /// Convert to the backend evaluation enum used by internal modules
    pub fn toBackend(self: OptLevel) @import("eval").EvalBackend {
        return switch (self) {
            .interpreter => .interpreter,
            .dev => .dev,
            .size, .speed => .llvm,
        };
    }
};

/// Default optimization level for commands that favor fast compilation over
/// fast output—`run`, `test`, `repl`, and `glue` all default here.
pub const default_dev_opt: OptLevel = .dev;

/// Default optimization level for `roc build`, which favors execution speed of
/// the produced binary. Intentionally differs from `default_dev_opt`.
pub const default_build_opt: OptLevel = .speed;

/// Package download size limits for commands that resolve dependencies.
/// Values are in megabytes; 0 means unlimited; null uses the default.
pub const ResolveLimitArgs = struct {
    max_package_mb: ?u32 = null, // per-package decompressed size limit
    max_transitive_mb: ?u32 = null, // overrides both transitive limits (packages and platforms) and the platform bundle cap
    replace_deps: ReplaceDepArgs = .{}, // `--replace-dep OLD NEW` occurrences, in command-line order
};

/// One `--replace-dep OLD NEW` occurrence, exactly as written.
pub const ReplaceDepArg = struct {
    old: []const u8,
    new: []const u8,
};

/// The `--replace-dep` occurrences of one invocation. Held by value so
/// argument structs stay copyable without owning an allocation.
pub const ReplaceDepArgs = struct {
    items: [max]ReplaceDepArg = [_]ReplaceDepArg{.{ .old = "", .new = "" }} ** max,
    len: usize = 0,

    pub const max: usize = 32;

    pub fn slice(self: *const ReplaceDepArgs) []const ReplaceDepArg {
        return self.items[0..self.len];
    }
};

const replace_dep_flag = "--replace-dep";

const ReplaceDepExtraction = struct {
    /// The arguments with every `--replace-dep OLD NEW` triple removed.
    /// Owned by the caller; the strings themselves are borrowed.
    args: []const []const u8,
    replace_deps: ReplaceDepArgs,
    problem: ?ArgProblem,
};

/// Pull every `--replace-dep OLD NEW` out of `args`. The flag takes two
/// separate arguments so URLs and paths need no escaping rules. Arguments
/// after `--` belong to the app being run and are left alone.
fn extractReplaceDeps(alloc: mem.Allocator, args: []const []const u8) mem.Allocator.Error!ReplaceDepExtraction {
    var rest = try std.array_list.Managed([]const u8).initCapacity(alloc, args.len);
    errdefer rest.deinit();
    var replace_deps: ReplaceDepArgs = .{};
    var problem: ?ArgProblem = null;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (mem.eql(u8, arg, "--")) {
            rest.appendSliceAssumeCapacity(args[i..]);
            break;
        }
        if (!mem.eql(u8, arg, replace_dep_flag)) {
            if (mem.startsWith(u8, arg, replace_dep_flag ++ "=") and problem == null) {
                problem = .{ .invalid_flag_value = .{
                    .flag = replace_dep_flag,
                    .value = arg[replace_dep_flag.len + 1 ..],
                    .valid_options = "two separate arguments: --replace-dep OLD NEW",
                } };
                continue;
            }
            rest.appendAssumeCapacity(arg);
            continue;
        }
        if (i + 2 >= args.len or mem.eql(u8, args[i + 1], "--") or mem.eql(u8, args[i + 2], "--")) {
            if (problem == null) problem = .{ .missing_flag_value = .{ .flag = replace_dep_flag ++ " OLD NEW" } };
            break;
        }
        if (replace_deps.len == ReplaceDepArgs.max) {
            if (problem == null) problem = .{ .invalid_flag_value = .{
                .flag = replace_dep_flag,
                .value = args[i + 1],
                .valid_options = "at most 32 replacements per invocation",
            } };
        } else {
            replace_deps.items[replace_deps.len] = .{ .old = args[i + 1], .new = args[i + 2] };
            replace_deps.len += 1;
        }
        i += 2;
    }

    return .{ .args = try rest.toOwnedSlice(), .replace_deps = replace_deps, .problem = problem };
}

/// The file a command reads when no path is given.
const default_roc_file = "main.roc";

/// Arguments for the default `roc` command
pub const RunArgs = struct {
    path: []const u8 = default_roc_file, // the path of the roc file to be executed
    opt: OptLevel = default_dev_opt, // the optimization level (dev, interpreter, size, speed)
    specialization_strategy: ?SpecializationStrategy = null, // explicit --specialize override, if provided
    target: ?[]const u8 = null, // the target to compile for (e.g., x64musl, x64glibc)
    app_args: []const []const u8 = &[_][]const u8{}, // any arguments to be passed to roc application being run
    no_cache: bool = false, // bypass the executable cache
    watch: bool = false, // hot reload when source inputs change; implied for dev runs
    explicit_watch: bool = false, // --watch was passed (as opposed to implied by a dev run)
    explicit_opt: bool = false, // --opt was passed (as opposed to defaulted)
    timings: bool = false, // always show the per-phase timing breakdown
    max_threads: ?usize = null, // max worker threads (null = auto, 1 = single-threaded)
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
    via_run_subcommand: bool = false, // parsed from explicit `roc run` (permits installed shorthands)
    root_source_url: ?[]const u8 = null, // internal: bundle URL provenance when the source was a URL or installed shorthand
};

/// Arguments for `roc install`
pub const InstallArgs = struct {
    shorthand: []const u8, // the name to install the bundle under (REQUIRED)
    url: []const u8, // the bundle URL to install (REQUIRED)
    max_threads: ?usize = null, // max worker threads for the install-time build
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
};

/// Arguments for `roc check`
pub const CheckArgs = struct {
    path: []const u8 = default_roc_file, // the path of the roc file to be checked
    main: ?[]const u8 = null, // the path to a roc file with an app header to be used to resolved dependencies
    time: bool = false, // whether to print timing information
    timings: bool = false, // always show the per-phase timing breakdown
    no_cache: bool = false, // disable cache
    verbose: bool = false, // enable verbose output
    watch: bool = false, // rerun check when source inputs change
    watch_inputs_file: ?[]const u8 = null, // internal: write watch input paths and byte states here
    max_threads: ?usize = null, // max worker threads (null = auto, 1 = single-threaded)
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
    root_source_url: ?[]const u8 = null, // internal: bundle URL provenance when the source was a URL or installed shorthand
    main_source_url: ?[]const u8 = null, // internal: bundle URL provenance when --main was a URL or installed shorthand
};

/// Arguments for `roc build`
pub const BuildArgs = struct {
    path: []const u8 = default_roc_file, // the path to the roc file to be built
    opt: OptLevel = default_build_opt, // the optimization level (dev, interpreter, size, speed)
    specialization_strategy: ?SpecializationStrategy = null, // explicit --specialize override, if provided
    target: ?[]const u8 = null, // the target to compile for (e.g., x64musl, x64glibc)
    output: ?[]const u8 = null, // the path where the output binary should be created
    debug: bool = false, // include debug information in the output binary
    fuzz: bool = false, // add libFuzzer no-link coverage instrumentation to LLVM output
    keep_temp: bool = false, // do not delete temporary directories created during build
    verbose: bool = false, // enable verbose output including cache statistics
    timings: bool = false, // always show the per-phase timing breakdown
    no_cache: bool = false, // disable compilation caching
    watch: bool = false, // rebuild when source inputs change
    watch_inputs_file: ?[]const u8 = null, // internal: write watch input paths and byte states here
    max_threads: ?usize = null, // max worker threads (null = auto, 1 = single-threaded)
    wasm_memory: ?usize = null, // initial memory size for WASM targets (default: sized from data segments plus the stack)
    wasm_stack_size: ?usize = null, // stack size for WASM targets (default: 8MB)
    require_executable_output: bool = false, // reject static/shared library targets
    require_host_runnable_output: bool = false, // internal: reject targets that cannot run on this host
    suppress_build_status: bool = false, // suppress "Built..." output (used by `roc` execution)
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
    synthetic_output_basename: ?[]const u8 = null, // internal: default output name when the source was a shorthand/URL, not a path
    root_source_url: ?[]const u8 = null, // internal: bundle URL provenance when the source was a URL or installed shorthand
    synthetic_default_platform: bool = false, // internal: build rewrote a headerless app to the default platform
    source_dir_override: ?[]const u8 = null, // internal: resolve root sibling imports from this directory
    synthetic_root_original_path: ?[]const u8 = null, // internal: original path for a synthetic default-app root
    synthetic_root_original_source: ?[]const u8 = null, // internal: normalized original source for synthetic-root diagnostics
    synthetic_root_header_len: usize = 0, // internal: byte length of the header prepended to synthetic_root_original_source
    synthetic_root_header_lines: u32 = 0, // internal: newline count of that header, for diagnostic line remapping
};

/// Arguments for `roc test`
pub const TestArgs = struct {
    path: []const u8 = default_roc_file, // the path to the file to be tested
    opt: OptLevel = default_dev_opt, // the optimization level (dev, interpreter, size, speed)
    specialization_strategy: ?SpecializationStrategy = null, // explicit --specialize override, if provided
    main: ?[]const u8 = null, // the path to a roc file with an app header to be used to resolve dependencies
    verbose: bool = false, // enable verbose output showing individual test results
    timings: bool = false, // always show the per-phase timing breakdown
    no_cache: bool = false, // disable compilation caching, force re-run all tests
    watch: bool = false, // rerun tests when source inputs change
    watch_inputs_file: ?[]const u8 = null, // internal: write watch input paths and byte states here
    max_threads: ?usize = null, // max worker threads (null = auto, 1 = single-threaded)
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
    root_source_url: ?[]const u8 = null, // internal: bundle URL provenance when the source was a URL or installed shorthand
    main_source_url: ?[]const u8 = null, // internal: bundle URL provenance when --main was a URL or installed shorthand
};

/// Arguments for `roc fmt`
pub const FormatArgs = struct {
    paths: []const []const u8, // the paths of files to be formatted
    stdin: bool = false, // if the input should be read in from stdin and output to stdout
    check: bool = false, // if the command should only check formatting rather than applying it
};

/// Arguments for `roc bundle`
pub const BundleArgs = struct {
    paths: []const []const u8, // the paths of roc files to bundle
    output_dir: ?[]const u8 = null, // the directory to output the bundle to
    compression_level: i32 = 3, // zstd compression level
};

/// Arguments for `roc unbundle`
pub const UnbundleArgs = struct {
    paths: []const []const u8, // the paths of .tar.zst files to unbundle
};

/// Arguments for `roc docs`
pub const DocsArgs = struct {
    path: []const u8 = default_roc_file, // the path of the roc file to generate docs for
    main: ?[]const u8 = null, // the path to a roc file with an app header to be used to resolved dependencies
    output: []const u8 = "generated-docs", // the output directory for generated documentation
    time: bool = false, // whether to print timing information
    no_cache: bool = false, // disable cache
    verbose: bool = false, // enable verbose output
    serve: bool = false, // start an HTTP server after generating docs
    with_lang_ref: bool = false, // include the language reference articles from docs/langref
    builtins: bool = false, // document the builtin module embedded in this compiler instead of a file
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
    root_source_url: ?[]const u8 = null, // internal: bundle URL provenance when the source was a URL or installed shorthand
    main_source_url: ?[]const u8 = null, // internal: bundle URL provenance when --main was a URL or installed shorthand
};

/// Arguments for `roc deps`
pub const DepsArgs = struct {
    path: []const u8 = default_roc_file, // the root .roc file whose dependency graph to print
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits and dependency replacements
};

/// Arguments for `roc bump`
pub const BumpArgs = struct {
    path: []const u8 = default_roc_file, // the new package's main .roc file
    old: []const u8, // the old package: URL, .tar.zst bundle, directory, or .roc file (REQUIRED)
    old_version: ?[]const u8 = null, // the old version (required unless `old` is a versioned URL)
    expect: ?[]const u8 = null, // fail unless this version is a sufficient bump
    no_cache: bool = false, // disable cache
    verbose: bool = false, // enable verbose output
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
    root_source_url: ?[]const u8 = null, // internal: bundle URL provenance when the source was a URL or installed shorthand
};

/// Arguments for `roc experimental-lsp`
pub const ExperimentalLspArgs = struct {
    debug_io: bool = false, // log the LSP messages to a temporary log file
    debug_build: bool = false,
    debug_syntax: bool = false,
    debug_server: bool = false,
};

/// Arguments for `roc repl`
pub const ReplArgs = struct {
    opt: OptLevel = default_dev_opt,
    specialization_strategy: ?SpecializationStrategy = null,
};

/// Arguments for `roc glue`
pub const GlueArgs = struct {
    glue_spec: []const u8, // path to the glue spec .roc file (REQUIRED)
    output_dir: []const u8, // path to the output directory for generated glue files (REQUIRED)
    platform_path: []const u8 = default_roc_file, // path to the platform .roc file
    opt: OptLevel = default_dev_opt,
    specialization_strategy: ?SpecializationStrategy = null,
    no_cache: bool = false, // disable compilation caching
};

/// How a flag is written on the command line, which is also the comparison
/// that recognizes it.
const FlagForm = enum {
    /// `--name`, matched exactly. Sets a `bool` field; a flag with no field is
    /// accepted and changes nothing.
    toggle,
    /// `--name=<value>`, matched by prefix.
    attached,
    /// `--name <value>`, matched exactly, taking the next argument as its value.
    separate,
    /// `--`: every later argument is positional.
    terminator,
    /// Matched exactly; the command answers with the compiler's version.
    version,
    /// Matched by prefix and refused as an unexpected argument.
    rejected,
    /// Listed in help only; another part of argument parsing consumes it.
    documented,
};

/// One flag of a command: how it is written, which field of the command's args
/// struct it sets, and its line in the command's help.
const Flag = struct {
    name: []const u8,
    /// Single-dash spelling. An `attached` flag's value follows it directly.
    short: ?[]const u8 = null,
    form: FlagForm = .toggle,
    /// The args-struct field the flag sets, as a dotted path. The field's type
    /// decides how the value is parsed, and a field with no default makes the
    /// flag required.
    field: ?[]const u8 = null,
    /// A `bool` field set whenever the flag is given.
    marks: ?[]const u8 = null,
    /// How help spells the flag's value.
    value_name: []const u8 = "",
    /// The flag's description in help. Null keeps the flag out of help. Help
    /// appends what the declaration already states: the accepted `levels`,
    /// the `range`, and the field's default.
    help: ?[]const u8 = null,
    /// What an integer value must be, for the problem reported when it is not.
    valid: []const u8 = "",
    /// Inclusive bounds of an integer value.
    range: ?[2]i32 = null,
    /// The optimization levels an `OptLevel` field accepts.
    levels: []const OptLevel = std.enums.values(OptLevel),

    /// The flag as the left column of its help line.
    fn spelling(comptime flag: Flag) []const u8 {
        const value = switch (flag.form) {
            .attached => "=" ++ flag.value_name,
            .separate => " " ++ flag.value_name,
            .toggle, .terminator, .version, .rejected, .documented => "",
        };
        return (if (flag.short) |short| "  " ++ short ++ ", " else "      ") ++ flag.name ++ value;
    }

    /// The flag's description for the help of a command whose args struct is `Parsed`.
    fn description(comptime flag: Flag, comptime Parsed: type) []const u8 {
        var text: []const u8 = flag.help.?;
        const path = flag.field orelse return text;
        const field = fieldInfoAt(Parsed, path);
        if (field.type == OptLevel) text = text ++ " " ++ optLevelList(flag.levels, .described);
        if (flag.range) |range| text = text ++ std.fmt.comptimePrint(" ({d}-{d})", .{ range[0], range[1] });
        const default = field.defaultValue() orelse return text;
        if (field.type == OptLevel) return text ++ " [default: " ++ @tagName(default) ++ "]";
        if (field.type == []const u8) return text ++ " [default: " ++ default ++ "]";
        if (@typeInfo(field.type) == .int) return text ++ std.fmt.comptimePrint(" [default: {d}]", .{default});
        return text;
    }
};

/// `levels` by name: comma-separated for a problem's valid options, or each
/// with its description for help.
fn optLevelList(comptime levels: []const OptLevel, comptime style: enum { names, described }) []const u8 {
    var text: []const u8 = "";
    for (levels, 0..) |level, index| {
        text = text ++ switch (style) {
            .names => (if (index == 0) "" else ",") ++ @tagName(level),
            .described => (if (index == 0) "" else if (index + 1 == levels.len) ", or " else ", ") ++
                @tagName(level) ++ " (" ++ level.description() ++ ")",
        };
    }
    return text;
}

/// The declaration of the struct field at a dotted `path` under `Parsed`.
fn fieldInfoAt(comptime Parsed: type, comptime path: []const u8) std.builtin.Type.StructField {
    if (mem.findScalar(u8, path, '.')) |dot| {
        return fieldInfoAt(@FieldType(Parsed, path[0..dot]), path[dot + 1 ..]);
    }
    for (@typeInfo(Parsed).@"struct".fields) |field| {
        if (mem.eql(u8, field.name, path)) return field;
    }
    @compileError(@typeName(Parsed) ++ " has no field `" ++ path ++ "` for a flag or argument to set");
}

/// The struct field at a dotted `path` under `parsed`.
fn fieldAt(parsed: anytype, comptime path: []const u8) *fieldInfoAt(@TypeOf(parsed.*), path).type {
    if (comptime mem.findScalar(u8, path, '.')) |dot| {
        return fieldAt(&@field(parsed, path[0..dot]), path[dot + 1 ..]);
    }
    return &@field(parsed, path);
}

/// A struct field filled by a positional argument.
const Positional = struct {
    field: []const u8,
    /// Set for an argument the command cannot run without: leaving it out
    /// prints this text followed by the command's help.
    missing: ?[]const u8 = null,
};

/// One subcommand: the `CliArgs` variant it parses into, the flags and
/// positional arguments that fill that variant's struct, and the prose of its
/// help. `parseArgsFor` and `helpText` are both generated from it, so the
/// accepted arguments and the documented ones cannot differ.
const Command = struct {
    /// The word after `roc`; empty for the command that runs with no word.
    name: []const u8,
    tag: std.meta.Tag(CliArgs),
    /// The command in one line, as `roc help` lists it.
    summary: []const u8,
    /// The opening of the command's own help, when that is not the summary.
    about: ?[]const u8 = null,
    /// The usage line after `roc <name>`.
    usage: []const u8 = "",
    /// A paragraph of help after the usage line.
    details: ?[]const u8 = null,
    /// The lines of help describing the positional arguments.
    arguments: ?[]const u8 = null,
    /// A paragraph of help after the options.
    footer: ?[]const u8 = null,
    flags: []const Flag = &.{},
    /// Whether the command shares the package size limit flags of dependency
    /// resolution, and whether it also accepts `--replace-dep`.
    resolve: enum { none, limits, limits_and_replace_deps } = .none,
    /// The leftmost column the option descriptions may start at.
    min_column: usize = 0,
    /// The fields positional arguments fill, in order.
    positionals: []const Positional = &.{},
    /// The slice field collecting every argument after `positionals` are filled.
    rest: ?[]const u8 = null,
    /// The one element `rest` holds when no argument reached it.
    rest_default: ?[]const u8 = null,
    /// Which unrecognized arguments are problems rather than positional arguments.
    refuses: enum { nothing, dashed, double_dashed } = .nothing,
    /// The fields no argument sets: the command's implementation fills them.
    derived: []const []const u8 = &.{},

    /// The payload of the `CliArgs` variant the command parses into.
    fn Args(comptime cmd: Command) type {
        return @FieldType(CliArgs, @tagName(cmd.tag));
    }

    /// The command's own flags followed by the shared ones it accepts.
    fn allFlags(comptime cmd: Command) []const Flag {
        const shared: []const Flag = switch (cmd.resolve) {
            .none => &.{},
            .limits => &limit_flags,
            .limits_and_replace_deps => &replace_dep_and_limit_flags,
        };
        return cmd.flags ++ shared;
    }

    /// Whether leaving `flag` out is a problem: it sets a field that has no default.
    fn requires(comptime cmd: Command, comptime flag: Flag) bool {
        const path = flag.field orelse return false;
        if (fieldInfoAt(cmd.Args(), path).defaultValue() != null) return false;
        if (flag.form != .attached and flag.form != .separate) {
            @compileError(flag.name ++ " cannot be required: only a flag with a value can set a field with no default");
        }
        return true;
    }

    /// Compile error unless the command and its args struct account for each
    /// other: every struct field is set by exactly one of a flag, a positional
    /// argument, `rest`, or the implementation (`derived`); a field with no
    /// default is always set; and no prefix-matched flag shadows another.
    fn check(comptime cmd: Command) void {
        @setEvalBranchQuota(100_000);
        const flags = cmd.allFlags();
        for (flags, 0..) |flag, index| {
            if (flag.form != .attached and flag.form != .rejected) continue;
            for (flags, 0..) |other, other_index| {
                if (index != other_index and other.form != .documented and mem.startsWith(u8, other.name, flag.name)) {
                    @compileError(flag.name ++ " is matched by prefix, so it would also match " ++ other.name);
                }
            }
        }

        const Parsed = cmd.Args();
        if (Parsed == void) return;
        for (cmd.derived) |name| {
            if (fieldInfoAt(Parsed, name).defaultValue() == null) {
                @compileError(@typeName(Parsed) ++ "." ++ name ++ " is derived, so it needs a default");
            }
        }
        for (@typeInfo(Parsed).@"struct".fields) |field| {
            var setters: usize = 0;
            for (cmd.derived) |name| setters += @intFromBool(mem.eql(u8, name, field.name));
            if (cmd.rest) |name| setters += @intFromBool(mem.eql(u8, name, field.name));
            for (cmd.positionals) |positional| {
                if (!mem.eql(u8, positional.field, field.name)) continue;
                setters += 1;
                if ((positional.missing == null) != (field.defaultValue() != null)) {
                    @compileError(@typeName(Parsed) ++ "." ++ field.name ++ " must have a default exactly when the argument may be left out");
                }
            }
            var flagged = false;
            for (flags) |flag| {
                if (flag.marks) |name| flagged = flagged or mem.eql(u8, name, field.name);
                if (flag.field) |path| flagged = flagged or mem.eql(u8, mem.sliceTo(path, '.'), field.name);
            }
            if (setters + @intFromBool(flagged) != 1) {
                @compileError(@typeName(Parsed) ++ "." ++ field.name ++
                    " must be set by exactly one of: a flag, a positional argument, `rest`, or `derived`");
            }
        }
    }

    /// The text `roc <name> --help` prints.
    fn helpText(comptime cmd: Command) []const u8 {
        @setEvalBranchQuota(100_000);
        const Parsed = cmd.Args();
        const help_flag = [_]Flag{.{ .name = "--help", .short = "-h", .help = "Print help" }};
        const column = optionColumn(cmd.flags ++ help_flag, cmd.min_column);

        var text: []const u8 = (cmd.about orelse cmd.summary) ++ "\n\nUsage: roc" ++
            (if (cmd.name.len > 0) " " ++ cmd.name else "") ++ cmd.usage ++ "\n";
        if (cmd.details) |details| text = text ++ "\n" ++ details ++ "\n";
        if (cmd.arguments) |arguments| text = text ++ "\nArguments:\n" ++ arguments ++ "\n";
        text = text ++ "\nOptions:\n" ++ optionLines(Parsed, cmd.flags, column);
        const shared = cmd.allFlags()[cmd.flags.len..];
        text = text ++ optionLines(Parsed, shared, optionColumn(shared, 0)) ++ optionLines(Parsed, &help_flag, column);
        if (cmd.footer) |footer| text = text ++ "\n" ++ footer ++ "\n";
        return text;
    }
};

/// The column the descriptions of `flags` start at in help: two spaces past
/// the longest listed flag, and no further left than `min_column`.
fn optionColumn(comptime flags: []const Flag, comptime min_column: usize) usize {
    var column = min_column;
    for (flags) |flag| {
        if (flag.help != null) column = @max(column, flag.spelling().len + 2);
    }
    return column;
}

/// The help lines of the listed `flags`, with descriptions starting at `column`.
fn optionLines(comptime Parsed: type, comptime flags: []const Flag, comptime column: usize) []const u8 {
    var text: []const u8 = "";
    for (flags) |flag| {
        if (flag.help == null) continue;
        var lines = mem.splitScalar(u8, flag.description(Parsed), '\n');
        text = text ++ flag.spelling() ++ " " ** (column - flag.spelling().len) ++ lines.first() ++ "\n";
        while (lines.next()) |line| text = text ++ " " ** column ++ line ++ "\n";
    }
    return text;
}

const limit_flags = [_]Flag{
    .{
        .name = "--max-package-mb",
        .form = .attached,
        .field = "resolve_limits.max_package_mb",
        .value_name = "<N>",
        .valid = "size in MB (0 for unlimited)",
        .help = std.fmt.comptimePrint("Per-package decompressed size limit in MB (default: {d}, 0 for unlimited)", .{
            ResolutionConfig.default_max_package_expanded_bytes / bytes_per_mb,
        }),
    },
    .{
        .name = "--max-transitive-mb",
        .form = .attached,
        .field = "resolve_limits.max_transitive_mb",
        .value_name = "<N>",
        .valid = "size in MB (0 for unlimited)",
        .help = std.fmt.comptimePrint(
            \\Combined size limit in MB for each direct dependency's transitive packages
            \\(defaults: packages {d}, platforms {d}; 0 for unlimited)
            \\Also caps each platform bundle during extraction
        , .{
            ResolutionConfig.default_max_transitive_expanded_bytes / bytes_per_mb,
            ResolutionConfig.default_max_platform_transitive_expanded_bytes / bytes_per_mb,
        }),
    },
};

/// `extractReplaceDeps` consumes `--replace-dep` before any command parses, so
/// the commands that accept it only document it.
const replace_dep_and_limit_flags = [_]Flag{.{
    .name = replace_dep_flag ++ " OLD NEW",
    .form = .documented,
    .help =
    \\Load NEW wherever a dependency declares exactly OLD, for this invocation only.
    \\Each is a complete package URL or a path to a root .roc file; repeatable.
    \\Run `roc deps` to see the declared sources
    ,
}} ++ limit_flags;

const specialize_flag: Flag = .{
    .name = "--specialize",
    .form = .attached,
    .field = "specialization_strategy",
    .value_name = "<yes|no>",
    .help = "Use lambda-set specialization (yes, default) or experimental boxy lowering (no)",
};

const target_flag: Flag = .{
    .name = "--target",
    .form = .attached,
    .field = "target",
    .value_name = "<target>",
    .help = "Target to compile for. A v1 in the name (" ++ @tagName(RocTarget.x64v1musl) ++
        ") targets the oldest CPUs of that architecture. Defaults to native target with musl for static linking. One of:" ++
        target_roster: {
            var lines: []const u8 = "";
            for (RocTarget.roster) |line| lines = lines ++ "\n" ++ line;
            break :target_roster lines;
        },
};

/// Internal: names the file a watched command writes its input paths and byte states to.
const watch_inputs_file_flag: Flag = .{ .name = "--watch-inputs-file", .form = .attached, .field = "watch_inputs_file" };

fn jobsFlag(comptime help: []const u8) Flag {
    return .{ .name = "--jobs", .short = "-j", .form = .attached, .field = "max_threads", .value_name = "<N>", .valid = "positive integer", .help = help };
}

const jobs_flag = jobsFlag("Max worker threads for parallel compilation (default: auto-detect CPU count)");

const run_flags = [_]Flag{
    .{ .name = "--", .form = .terminator },
    .{ .name = "--version", .short = "-v", .form = .version },
    .{ .name = "--opt", .form = .attached, .field = "opt", .marks = "explicit_opt", .value_name = "<opt>", .help = "Execution mode:" },
    specialize_flag,
    target_flag,
    .{ .name = "--no-cache", .field = "no_cache", .help = "Disable compilation and executable caches (useful for compiler and platform developers)" },
    .{ .name = "--no-color", .form = .documented, .help = "Do not use ANSI escape codes in CLI output" },
    .{ .name = "--watch", .field = "explicit_watch" },
    .{ .name = "--timings", .field = "timings" },
    jobs_flag,
};

const run_derived = [_][]const u8{ "watch", "via_run_subcommand", "root_source_url" };

/// Every subcommand, in the order `roc help` lists them.
const commands = [_]Command{
    .{
        .name = "run",
        .tag = .run,
        .summary = "Run a .roc file, a bundle URL, or an installed shorthand",
        .about = "Run a Roc application",
        .usage = " [OPTIONS] [SOURCE] [-- [ARGS_FOR_APP]...]",
        .details =
        \\SOURCE may be:
        \\  a .roc file path        roc run main.roc
        \\  a bundle URL            roc run https://example.com/tool/1.2.3/<hash>.tar.zst
        \\  an installed shorthand  roc run tokei      (see `roc install`)
        \\
        \\Running an installed shorthand executes the optimized binary that was
        \\built at install time; no compilation or network access is needed.
        \\File and URL sources accept the same options as the default `roc` command.
        ,
        .flags = &run_flags,
        .resolve = .limits_and_replace_deps,
        .min_column = 33,
        .positionals = &.{.{ .field = "path" }},
        .rest = "app_args",
        .derived = &run_derived,
    },
    .{
        .name = "install",
        .tag = .install,
        .summary = "Install a Roc app or glue spec from a bundle URL under a shorthand name",
        .usage = " [OPTIONS] <SHORTHAND> <URL>",
        .details =
        \\Downloads the bundle, verifies its content hash, and builds it with
        \\--opt=speed. An app becomes an optimized binary that `roc run
        \\<SHORTHAND>` executes with no compile step; a glue spec becomes an
        \\optimized plugin dylib that `roc glue <SHORTHAND> ...` loads directly.
        \\Installations persist outside the cache and are scoped to the compiler
        \\version that installed them.
        ,
        .arguments =
        \\  <SHORTHAND>  A name of your choice: a lowercase letter followed by
        \\               lowercase letters, digits, or underscores
        \\  <URL>        A .tar.zst bundle URL ending in a base58-encoded BLAKE3 hash
        ,
        .flags = &.{jobsFlag("Max worker threads for the install-time build")},
        .resolve = .limits,
        .min_column = 33,
        .positionals = &.{ .{ .field = "shorthand", .missing = "" }, .{ .field = "url", .missing = "" } },
    },
    .{
        .name = "build",
        .tag = .build,
        .summary = "Build a binary from the given .roc file, but don't run it",
        .usage = " [OPTIONS] [ROC_FILE]",
        .arguments = "  [ROC_FILE] The .roc file to build [default: " ++ default_roc_file ++ "]",
        .flags = &.{
            .{ .name = "--output", .form = .attached, .field = "output", .value_name = "<output>", .help = "The full path to the output binary, including filename. To specify directory only, specify a path that ends in a directory separator (e.g. a slash)" },
            .{ .name = "--opt", .form = .attached, .field = "opt", .value_name = "<opt>", .help = "Build mode:" },
            specialize_flag,
            target_flag,
            .{ .name = "--debug", .field = "debug", .help = "Include debug information in the output binary" },
            .{ .name = "--fuzz", .field = "fuzz", .help = "Add libFuzzer no-link coverage instrumentation; final linkage must provide the runtime" },
            .{ .name = "--keep-temp", .field = "keep_temp", .help = "Keep all temporary directories created during build" },
            .{ .name = "--verbose", .field = "verbose", .help = "Enable verbose output including cache statistics" },
            .{ .name = "--timings", .field = "timings", .help = "Show how long each compilation phase took (shown automatically when a build is slow)" },
            .{ .name = "--no-cache", .field = "no_cache", .help = "Disable compilation caching" },
            .{ .name = "--watch", .field = "watch", .help = "Rebuild when source inputs change" },
            watch_inputs_file_flag,
            jobs_flag,
            .{ .name = "--wasm-memory", .form = .attached, .field = "wasm_memory", .value_name = "<bytes>", .valid = "positive integer (bytes)", .help = "Initial memory size for WASM targets in bytes (default: sized from data segments plus the stack)" },
            .{ .name = "--wasm-stack-size", .form = .attached, .field = "wasm_stack_size", .value_name = "<bytes>", .valid = "positive integer (bytes)", .help = std.fmt.comptimePrint("Stack size for WASM targets in bytes (default: {d} = {d}MB)", .{ linker.DEFAULT_WASM_STACK_SIZE, linker.DEFAULT_WASM_STACK_SIZE / bytes_per_mb }) },
        },
        .resolve = .limits_and_replace_deps,
        .min_column = 37,
        .positionals = &.{.{ .field = "path" }},
        .derived = &.{
            "require_executable_output",
            "require_host_runnable_output",
            "suppress_build_status",
            "synthetic_output_basename",
            "root_source_url",
            "synthetic_default_platform",
            "source_dir_override",
            "synthetic_root_original_path",
            "synthetic_root_original_source",
            "synthetic_root_header_len",
            "synthetic_root_header_lines",
        },
    },
    .{
        .name = "bundle",
        .tag = .bundle,
        .summary = "Bundle .roc files into a compressed archive",
        .usage = " [OPTIONS] [ROC_FILES]...",
        .arguments = "  [ROC_FILES]...  The .roc files to bundle [default: " ++ default_roc_file ++ "]",
        .flags = &.{
            .{ .name = "--output-dir", .form = .separate, .field = "output_dir", .value_name = "<PATH>", .help = "Directory to output the bundle to [default: current directory]" },
            .{ .name = "--compression", .form = .separate, .field = "compression_level", .value_name = "<N>", .range = .{ 1, 22 }, .help = "Compression level" },
        },
        .rest = "paths",
        .rest_default = default_roc_file,
        .refuses = .double_dashed,
    },
    .{
        .name = "unbundle",
        .tag = .unbundle,
        .summary = "Extract files from compressed .tar.zst archives",
        .usage = " [OPTIONS] [ARCHIVE_FILES]...",
        .arguments =
        \\  [ARCHIVE_FILES]...  The .tar.zst files to unbundle
        \\                      [default: all .tar.zst files in current directory]
        ,
        .rest = "paths",
        .refuses = .dashed,
    },
    .{
        .name = "test",
        .tag = .test_cmd,
        .summary = "Run all top-level `expect`s in a module, and in the modules and path dependencies it imports",
        .about =
        \\Run all top-level `expect`s in a main module and any modules it imports
        \\
        \\Dependencies reached through a filesystem path are tested too, because
        \\they are yours to edit. Dependencies downloaded from a URL are not: their
        \\`expect`s belong to whoever published them.
        ,
        .usage = " [OPTIONS] [ROC_FILE]",
        .arguments = "  [ROC_FILE] The .roc file to test [default: " ++ default_roc_file ++ "]",
        .flags = &.{
            .{ .name = "--opt", .form = .attached, .field = "opt", .value_name = "<opt>", .help = "Execution mode:" },
            specialize_flag,
            .{ .name = "--main", .form = .attached, .field = "main", .value_name = "<main>", .help = "The .roc file of the main app/package module to resolve dependencies from" },
            .{ .name = "--verbose", .field = "verbose", .help = "Enable verbose output showing individual test results" },
            .{ .name = "--timings", .field = "timings", .help = "Show how long each compilation and test phase took" },
            .{ .name = "--no-cache", .field = "no_cache", .help = "Disable compilation caching, force re-run all tests" },
            .{ .name = "--watch", .field = "watch", .help = "Re-run when source inputs change" },
            watch_inputs_file_flag,
            jobs_flag,
        },
        .resolve = .limits_and_replace_deps,
        .min_column = 38,
        .positionals = &.{.{ .field = "path" }},
        .refuses = .dashed,
        .derived = &.{ "root_source_url", "main_source_url" },
    },
    .{
        .name = "repl",
        .tag = .repl,
        .summary = "Launch the interactive Read Eval Print Loop (REPL)",
        .usage = " [OPTIONS]",
        .flags = &.{
            .{ .name = "--opt", .form = .attached, .field = "opt", .value_name = "<opt>", .help = "Execution mode:" },
            specialize_flag,
        },
    },
    .{
        .name = "fmt",
        .tag = .fmt,
        .summary = "Format a .roc file or the .roc files contained in a directory using standard Roc formatting",
        .usage = " [OPTIONS] [DIRECTORY_OR_FILES]",
        .arguments = "  [DIRECTORY_OR_FILES]",
        .footer = "If DIRECTORY_OR_FILES is omitted, the .roc files in the current working directory are formatted.",
        .flags = &.{
            .{
                .name = "--check",
                .field = "check",
                .help =
                \\Checks that specified files are formatted
                \\(If formatting is needed, return a non-zero exit code.)
                ,
            },
            .{ .name = "--stdin", .field = "stdin", .help = "Format code from stdin; output to stdout" },
            .{ .name = "--", .form = .terminator, .help = "Treat all remaining arguments as paths" },
        },
        .rest = "paths",
        .rest_default = default_roc_file,
        .refuses = .dashed,
    },
    .{
        .name = "glue",
        .tag = .glue,
        .summary = "Generate native glue code from a Roc platform using a language-specific glue spec",
        .about = "Generate glue code from a platform using a glue spec",
        .usage = " [OPTIONS] <GLUE_SPEC> <GLUE_DIR> [ROC_FILE]",
        .arguments = "  <GLUE_SPEC>  The glue spec .roc file that defines how to generate glue code\n" ++
            "  <GLUE_DIR>   The output directory for generated glue files\n" ++
            "  [ROC_FILE]   The platform .roc file to analyze [default: " ++ default_roc_file ++ "]",
        .flags = &.{
            .{ .name = "--opt", .form = .attached, .field = "opt", .value_name = "<opt>", .levels = &.{ .dev, .size, .speed }, .help = "Compile and run the glue spec with" },
            specialize_flag,
            .{ .name = "--no-cache", .field = "no_cache", .help = "Disable compilation caching" },
        },
        .positionals = &.{
            .{ .field = "glue_spec", .missing = "Error: Missing required argument <GLUE_SPEC>\n\n" },
            .{ .field = "output_dir", .missing = "Error: Missing required argument <GLUE_DIR>\n\n" },
            .{ .field = "platform_path" },
        },
    },
    .{
        .name = "version",
        .tag = .version,
        .summary = "Print the Roc compiler's version",
        .about = "Print the Roc compiler’s version",
    },
    .{
        .name = "check",
        .tag = .check,
        .summary = "Check the code for problems, but don't build or run it",
        .usage = " [OPTIONS] [ROC_FILE]",
        .arguments = "  [ROC_FILE]  The .roc file to check [default: " ++ default_roc_file ++ "]",
        .flags = &.{
            .{ .name = "--specialize", .form = .rejected },
            .{ .name = "--main", .form = .attached, .field = "main", .value_name = "<main>", .help = "The .roc file of the main app/package module to resolve dependencies from" },
            .{ .name = "--time", .field = "time", .help = "Print timing information for each compilation phase. Will not print anything if everything is cached." },
            .{ .name = "--timings", .field = "timings", .help = "Show how long each compilation phase took (shown automatically when checking is slow)" },
            .{ .name = "--no-cache", .field = "no_cache", .help = "Disable caching" },
            .{ .name = "--verbose", .field = "verbose", .help = "Enable verbose output including cache statistics" },
            .{ .name = "--watch", .field = "watch", .help = "Re-run when source inputs change" },
            watch_inputs_file_flag,
            jobs_flag,
        },
        .resolve = .limits_and_replace_deps,
        .positionals = &.{.{ .field = "path" }},
        .derived = &.{ "root_source_url", "main_source_url" },
    },
    .{
        .name = "docs",
        .tag = .docs,
        .summary = "Generate documentation for a Roc package or platform",
        .about = "Generate documentation for a Roc package",
        .usage = " [OPTIONS] [ROC_FILE]",
        .arguments = "  [ROC_FILE]  The .roc file to generate docs for [default: " ++ default_roc_file ++ "]",
        .flags = &.{
            .{ .name = "--main", .form = .attached, .field = "main", .value_name = "<main>", .help = "The .roc file of the main app/package module to resolve dependencies from" },
            .{ .name = "--output", .form = .attached, .field = "output", .value_name = "<dir>", .help = "Output directory for generated documentation" },
            .{ .name = "--serve", .field = "serve", .help = "Start an HTTP server to view the documentation" },
            .{ .name = "--with-lang-ref", .field = "with_lang_ref", .help = "Include the language reference articles from docs/langref" },
            .{ .name = "--builtins", .field = "builtins", .help = "Document the builtins of this roc compiler instead of a .roc file" },
            .{ .name = "--time", .field = "time", .help = "Print timing information for each compilation phase. Will not print anything if everything is cached." },
            .{ .name = "--no-cache", .field = "no_cache", .help = "Disable caching" },
            .{ .name = "--verbose", .field = "verbose", .help = "Enable verbose output including cache statistics" },
        },
        .resolve = .limits_and_replace_deps,
        .positionals = &.{.{ .field = "path" }},
        .derived = &.{ "root_source_url", "main_source_url" },
    },
    .{
        .name = "deps",
        .tag = .deps,
        .summary = "Print the dependency tree of a Roc app, package, or platform",
        .about =
        \\Print the dependency tree of a Roc app, package, or platform
        \\
        \\Resolves the dependency graph and prints every declared source in full,
        \\ready to copy into --replace-dep. Nothing is compiled or run. Packages
        \\that are not cached yet are downloaded so their headers can be read.
        ,
        .usage = " [OPTIONS] [ROC_FILE]",
        .arguments = "  [ROC_FILE]  The root .roc file of the app, package, or platform [default: " ++ default_roc_file ++ "]",
        .resolve = .limits_and_replace_deps,
        .min_column = 31,
        .positionals = &.{.{ .field = "path" }},
        .refuses = .dashed,
    },
    .{
        .name = "bump",
        .tag = .bump,
        .summary = "Compare a package's public API against a previous version and report the required semver bump",
        .about =
        \\Compare a package's public API against a previous version and report the
        \\required semver bump (patch, minor, or major) plus the next version.
        ,
        .usage = " --old <OLD> [OPTIONS] [ROC_FILE]",
        .arguments = "  [ROC_FILE]  The new package's main .roc file [default: " ++ default_roc_file ++ "]",
        .footer =
        \\Both the old and new package must compile with this compiler. Only the
        \\modules exposed by the package header are compared; platform
        \\provides/requires are not yet part of the comparison.
        \\
        \\If this is the package's first release, there is nothing to compare—
        \\publish it as 1.0.0.
        ,
        .flags = &.{
            .{
                .name = "--old",
                .form = .separate,
                .field = "old",
                .value_name = "<OLD>",
                .help =
                \\The previous package version: a package URL, a
                \\.tar.zst bundle, a directory, or a main .roc file
                ,
            },
            .{
                .name = "--old-version",
                .form = .separate,
                .field = "old_version",
                .value_name = "<X.Y.Z>",
                .help =
                \\The previous version number (required unless
                \\--old is a URL with a version path segment)
                ,
            },
            .{
                .name = "--expect",
                .form = .separate,
                .field = "expect",
                .value_name = "<X.Y.Z>",
                .help =
                \\Fail unless this version bumps at least as far
                \\as the API diff requires (for release CI)
                ,
            },
            .{ .name = "--no-cache", .field = "no_cache", .help = "Disable caching" },
            .{ .name = "--verbose", .field = "verbose", .help = "Enable verbose output" },
        },
        .resolve = .limits,
        .positionals = &.{.{ .field = "path" }},
        .refuses = .double_dashed,
        .derived = &.{"root_source_url"},
    },
    .{
        .name = "experimental-lsp",
        .tag = .experimental_lsp,
        .summary = "Start the experimental language server (LSP) implementation",
        .about = "Start the experimental Roc language server (LSP)",
        .usage = " [OPTIONS]",
        .flags = &.{
            .{
                // LSP clients (e.g. vscode-languageclient) append a transport
                // flag to the server command line.
                .name = "--stdio",
                .help =
                \\Communicate over stdio (the default and only
                \\transport; accepted for compatibility with LSP
                \\clients that pass it explicitly)
                ,
            },
            .{ .name = "--debug-transport", .field = "debug_io", .help = "Mirror all JSON-RPC traffic to a temp log file" },
            .{ .name = "--debug-build", .field = "debug_build", .help = "Log build environment actions to the debug log" },
            .{ .name = "--debug-syntax", .field = "debug_syntax", .help = "Log syntax/type checking steps to the debug log" },
            .{ .name = "--debug-server", .field = "debug_server", .help = "Log server lifecycle details to the debug log" },
        },
    },
    .{
        .name = "help",
        .tag = .help,
        .summary = "Print this message",
    },
    .{
        .name = "licenses",
        .tag = .licenses,
        .summary = "Prints license info for Roc as well as attributions to other projects used by Roc",
    },
};

/// The command `roc` runs when its first argument names no subcommand. Its
/// help is the top-level help.
const default_command: Command = .{
    .name = "",
    .tag = .run,
    .summary = "Run the given .roc file\nYou can use one of the COMMANDS below to do something else!",
    .usage = " [OPTIONS] [ROC_FILE] [ARGS_FOR_APP]...\n       roc <COMMAND>",
    .details = "Commands:" ++ command_roster: {
        var width: usize = 0;
        for (commands) |cmd| width = @max(width, cmd.name.len + 1);
        var lines: []const u8 = "";
        for (commands) |cmd| lines = lines ++ "\n  " ++ cmd.name ++ " " ** (width - cmd.name.len) ++ cmd.summary;
        break :command_roster lines;
    },
    .arguments = "  [ROC_FILE]         The .roc file of an app to run [default: " ++ default_roc_file ++ "]\n" ++
        "  [ARGS_FOR_APP]...  Arguments to pass into the app being run\n" ++
        "                     e.g. `roc app.roc -- arg1 arg2`",
    .flags = &run_flags,
    .resolve = .limits_and_replace_deps,
    .min_column = 37,
    .positionals = &.{.{ .field = "path" }},
    .rest = "app_args",
    .derived = &run_derived,
};

/// Parse a list of arguments.
pub fn parse(alloc: mem.Allocator, std_io: std.Io, args: []const []const u8) ParseError!CliArgs {
    return (try parseWithGlobalOptions(alloc, std_io, args)).command;
}

/// Parse CLI-wide options before dispatching to a command parser.
///
/// `--no-color` is consumed before command parsing so every command shares one
/// explicit output setting. Arguments after `--` belong to the executed Roc
/// application and remain untouched.
pub fn parseWithGlobalOptions(alloc: mem.Allocator, std_io: std.Io, args: []const []const u8) ParseError!ParsedArgs {
    var command_args = try alloc.alloc([]const u8, args.len);
    defer alloc.free(command_args);

    var no_color = false;
    var after_separator = false;
    var command_arg_count: usize = 0;
    for (args) |arg| {
        if (mem.eql(u8, arg, "--")) after_separator = true;
        if (!after_separator and mem.eql(u8, arg, "--no-color")) {
            no_color = true;
            continue;
        }
        command_args[command_arg_count] = arg;
        command_arg_count += 1;
    }

    return .{
        .command = try parseCommand(alloc, std_io, command_args[0..command_arg_count]),
        .no_color = no_color,
    };
}

fn parseCommand(alloc: mem.Allocator, std_io: std.Io, all_args: []const []const u8) ParseError!CliArgs {
    const extraction = try extractReplaceDeps(alloc, all_args);
    defer alloc.free(extraction.args);
    if (extraction.problem) |problem| return CliArgs{ .problem = problem };
    const args = extraction.args;
    const replace_deps = extraction.replace_deps;

    if (args.len > 0) {
        inline for (commands) |cmd| {
            if (mem.eql(u8, args[0], cmd.name)) {
                // Only commands that load a dependency graph acquire replacement
                // behavior. The others reject the flag by command name before
                // command-specific parsing, so a `--help` or a missing-argument
                // problem in that command cannot hide it.
                if (cmd.resolve != .limits_and_replace_deps and replace_deps.len > 0) {
                    return unexpectedArgument(cmd.name, replace_dep_flag);
                }
                const parsed = if (cmd.tag == .help)
                    CliArgs{ .help = main_help }
                else if (cmd.tag == .run)
                    // `roc run` accepts everything the default run accepts, plus
                    // installed shorthands (which the default run rejects to keep
                    // the subcommand namespace separate from the shorthand namespace).
                    try parseRun(cmd, alloc, args[1..])
                else if (cmd.tag == .unbundle)
                    try parseUnbundle(cmd, alloc, std_io, args[1..])
                else if (cmd.tag == .docs)
                    try parseDocs(cmd, alloc, args[1..])
                else
                    try parseArgsFor(cmd, alloc, args[1..]);
                return withReplaceDeps(cmd, alloc, parsed, replace_deps, all_args[0]);
            }
        }
    }
    const parsed = try parseRun(default_command, alloc, args);
    return withReplaceDeps(default_command, alloc, parsed, replace_deps, if (all_args.len > 0) all_args[0] else "");
}

/// Hands a parsed command the invocation's `--replace-dep` occurrences. Help
/// and problems pass through; any other outcome that is not the command's own
/// variant has nowhere to put them, so the flag is unexpected there.
fn withReplaceDeps(comptime cmd: Command, alloc: mem.Allocator, parsed: CliArgs, replace_deps: ReplaceDepArgs, first_arg: []const u8) CliArgs {
    if (replace_deps.len == 0 or parsed == .help or parsed == .problem) return parsed;
    if (cmd.resolve == .limits_and_replace_deps and parsed == cmd.tag) {
        var result = parsed;
        @field(result, @tagName(cmd.tag)).resolve_limits.replace_deps = replace_deps;
        return result;
    }
    parsed.deinit(alloc);
    return unexpectedArgument(first_arg, replace_dep_flag);
}

const main_help = default_command.helpText();

/// `roc docs`. `--builtins` documents the compiler's embedded builtin module,
/// so there is no file to document and no main file to resolve packages from.
fn parseDocs(comptime cmd: Command, alloc: mem.Allocator, args: []const []const u8) mem.Allocator.Error!CliArgs {
    const parsed = try parseArgsFor(cmd, alloc, args);
    if (parsed != .docs or !parsed.docs.builtins) return parsed;
    for (args) |arg| {
        if (arg.len == 0 or arg[0] != '-') {
            parsed.deinit(alloc);
            return unexpectedArgument(cmd.name, arg);
        }
    }
    if (parsed.docs.main != null) {
        parsed.deinit(alloc);
        return unexpectedArgument(cmd.name, "--main");
    }
    return parsed;
}

fn unexpectedArgument(cmd: []const u8, arg: []const u8) CliArgs {
    return .{ .problem = .{ .unexpected_argument = .{ .cmd = cmd, .arg = arg } } };
}

/// Parses the arguments after a command's name from its `Command` declaration.
/// Each flag is recognized by the comparison its form names; an argument no
/// flag recognizes is positional unless the command refuses it.
fn parseArgsFor(comptime cmd: Command, alloc: mem.Allocator, args: []const []const u8) mem.Allocator.Error!CliArgs {
    comptime cmd.check();
    const Parsed = cmd.Args();
    const flags = comptime cmd.allFlags();
    const help = comptime cmd.helpText();

    var parsed: Parsed = undefined;
    if (Parsed != void) {
        inline for (@typeInfo(Parsed).@"struct".fields) |field| {
            if (comptime field.defaultValue()) |default| @field(parsed, field.name) = default;
        }
    }
    var flags_given = [_]bool{false} ** flags.len;
    var positional_count: usize = 0;
    var rest = std.array_list.Managed([]const u8).init(alloc);
    defer rest.deinit();
    var flags_ended = false;

    var i: usize = 0;
    next_arg: while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (!flags_ended) {
            if (isHelpFlag(arg)) return .{ .help = help };

            inline for (flags, 0..) |flag, flag_index| {
                switch (flag.form) {
                    .toggle => if (mem.eql(u8, arg, flag.name)) {
                        if (flag.field) |path| fieldAt(&parsed, path).* = true;
                        continue :next_arg;
                    },
                    .attached => {
                        var written: ?[]const u8 = null;
                        var maybe_value: ?[]const u8 = null;
                        if (mem.startsWith(u8, arg, flag.name)) {
                            written = flag.name;
                            maybe_value = getFlagValue(arg);
                        } else if (flag.short) |short| {
                            if (mem.startsWith(u8, arg, short)) {
                                written = short;
                                if (arg.len > short.len) maybe_value = arg[short.len..];
                            }
                        }
                        if (written) |spelling| {
                            const value = maybe_value orelse return .{ .problem = .{ .missing_flag_value = .{ .flag = spelling } } };
                            if (setFlagValue(flag, fieldAt(&parsed, flag.field.?), spelling, value)) |problem| return .{ .problem = problem };
                            if (flag.marks) |name| @field(parsed, name) = true;
                            flags_given[flag_index] = true;
                            continue :next_arg;
                        }
                    },
                    .separate => if (mem.eql(u8, arg, flag.name)) {
                        if (i + 1 >= args.len) return .{ .problem = .{ .missing_flag_value = .{ .flag = flag.name } } };
                        i += 1;
                        if (setFlagValue(flag, fieldAt(&parsed, flag.field.?), flag.name, args[i])) |problem| return .{ .problem = problem };
                        flags_given[flag_index] = true;
                        continue :next_arg;
                    },
                    .terminator => if (mem.eql(u8, arg, flag.name)) {
                        flags_ended = true;
                        continue :next_arg;
                    },
                    .version => if (mem.eql(u8, arg, flag.name) or mem.eql(u8, arg, flag.short.?)) return .version,
                    .rejected => if (mem.startsWith(u8, arg, flag.name)) return unexpectedArgument(cmd.name, arg),
                    .documented => {},
                }
            }

            const refused = switch (cmd.refuses) {
                .nothing => false,
                .dashed => mem.startsWith(u8, arg, "-"),
                .double_dashed => mem.startsWith(u8, arg, "--"),
            };
            if (refused) return unexpectedArgument(cmd.name, arg);

            inline for (cmd.positionals, 0..) |positional, index| {
                if (positional_count == index) {
                    @field(parsed, positional.field) = arg;
                    positional_count += 1;
                    continue :next_arg;
                }
            }
        }
        if (cmd.rest == null) return unexpectedArgument(cmd.name, arg);
        try rest.append(arg);
    }

    inline for (cmd.positionals, 0..) |positional, index| {
        if (positional.missing) |complaint| {
            if (positional_count <= index) return .{ .help = complaint ++ help };
        }
    }
    inline for (flags, 0..) |flag, flag_index| {
        if (comptime cmd.requires(flag)) {
            if (!flags_given[flag_index]) return .{ .problem = .{ .missing_flag_value = .{ .flag = flag.name } } };
        }
    }
    if (cmd.rest) |rest_field| {
        if (rest.items.len == 0) {
            if (cmd.rest_default) |default| try rest.append(default);
        }
        @field(parsed, rest_field) = try rest.toOwnedSlice();
    }
    return @unionInit(CliArgs, @tagName(cmd.tag), parsed);
}

/// Stores the value written for `flag` in the field the flag sets, parsed as
/// the field's type requires. Returns the problem when the value is not one
/// the field accepts; `written` is the spelling of the flag to report.
fn setFlagValue(comptime flag: Flag, field: anytype, written: []const u8, value: []const u8) ?ArgProblem {
    const Field = @TypeOf(field.*);
    const valid_options: []const u8 = comptime if (Field == OptLevel)
        optLevelList(flag.levels, .names)
    else if (Field == ?SpecializationStrategy)
        SpecializationStrategy.cliOptions()
    else if (flag.range) |range|
        std.fmt.comptimePrint("integer between {d} and {d}", .{ range[0], range[1] })
    else
        flag.valid;
    const invalid: ArgProblem = .{ .invalid_flag_value = .{ .flag = written, .value = value, .valid_options = valid_options } };

    if (Field == []const u8 or Field == ?[]const u8) {
        field.* = value;
    } else if (Field == OptLevel) {
        inline for (flag.levels) |level| {
            if (mem.eql(u8, value, @tagName(level))) {
                field.* = level;
                return null;
            }
        }
        return invalid;
    } else if (Field == ?SpecializationStrategy) {
        field.* = SpecializationStrategy.fromCliValue(value) orelse return invalid;
    } else {
        const Int = if (@typeInfo(Field) == .optional) @typeInfo(Field).optional.child else Field;
        const number = std.fmt.parseInt(Int, value, 10) catch return invalid;
        if (flag.range) |range| {
            if (number < range[0] or number > range[1]) return invalid;
        }
        field.* = number;
    }
    return null;
}

fn parseRun(comptime cmd: Command, alloc: mem.Allocator, args: []const []const u8) mem.Allocator.Error!CliArgs {
    var parsed = try parseArgsFor(cmd, alloc, args);
    if (parsed != .run) return parsed;

    const run = &parsed.run;
    run.watch = run.explicit_watch or run.opt == .dev;
    run.via_run_subcommand = cmd.name.len > 0;
    if (!run.via_run_subcommand and install.classifySourceRef(run.path) == .shorthand) {
        const name = run.path;
        parsed.deinit(alloc);
        return CliArgs{ .problem = ArgProblem{ .shorthand_requires_run = .{ .name = name } } };
    }
    return parsed;
}

fn parseUnbundle(comptime cmd: Command, alloc: mem.Allocator, std_io: std.Io, args: []const []const u8) ParseError!CliArgs {
    const parsed = try parseArgsFor(cmd, alloc, args);
    if (parsed != .unbundle or parsed.unbundle.paths.len > 0) return parsed;

    // If no paths specified, default to all .tar.zst files in current directory
    var paths = std.array_list.Managed([]const u8).init(alloc);
    errdefer paths.deinit();
    var cwd = try std.Io.Dir.cwd().openDir(std_io, ".", .{ .iterate = true });
    defer cwd.close(std_io);
    var iter = cwd.iterate();
    while (try iter.next(std_io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".tar.zst")) {
            try paths.append(try alloc.dupe(u8, entry.name));
        }
    }

    // If still no files found, show help
    if (paths.items.len == 0) {
        return CliArgs{ .help = comptime cmd.helpText() ++ "\nError: No .tar.zst files found in current directory\n" };
    }
    return CliArgs{ .unbundle = UnbundleArgs{ .paths = try paths.toOwnedSlice() } };
}

fn isHelpFlag(arg: []const u8) bool {
    return mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help");
}

fn getFlagValue(arg: []const u8) ?[]const u8 {
    var iter = mem.splitScalar(u8, arg, '=');
    // ignore the flag key
    _ = iter.next();
    return iter.next();
}

test "specialization strategy parsing" {
    const gpa = testing.allocator;

    {
        const result = try parse(gpa, testing.io, &[_][]const u8{});
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?SpecializationStrategy, null), result.run.specialization_strategy);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "--specialize=yes", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqual(SpecializationStrategy.lss, result.run.specialization_strategy.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--specialize=no" });
        defer result.deinit(gpa);
        try testing.expectEqual(SpecializationStrategy.boxy, result.run.specialization_strategy.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--", "--specialize=no" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?SpecializationStrategy, null), result.run.specialization_strategy);
        try testing.expectEqualStrings("--specialize=no", result.run.app_args[0]);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--specialize"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--specialize", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--specialize=maybe"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--specialize", result.problem.invalid_flag_value.flag);
        try testing.expectEqualStrings("maybe", result.problem.invalid_flag_value.value);
        try testing.expectEqualStrings("yes,no", result.problem.invalid_flag_value.valid_options);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--specialize=no" });
        defer result.deinit(gpa);
        try testing.expectEqual(SpecializationStrategy.boxy, result.build.specialization_strategy.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--specialize=yes" });
        defer result.deinit(gpa);
        try testing.expectEqual(SpecializationStrategy.lss, result.test_cmd.specialization_strategy.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "repl", "--specialize=no" });
        defer result.deinit(gpa);
        try testing.expectEqual(SpecializationStrategy.boxy, result.repl.specialization_strategy.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "--specialize=yes", "Glue.roc", "glue-out" });
        defer result.deinit(gpa);
        try testing.expectEqual(SpecializationStrategy.lss, result.glue.specialization_strategy.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--specialize=no" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("check", result.problem.unexpected_argument.cmd);
        try testing.expectEqualStrings("--specialize=no", result.problem.unexpected_argument.arg);
    }
}

test "default roc command" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.run.path);
        try testing.expectEqual(.dev, result.run.opt);
        try testing.expect(result.run.watch);
        try testing.expectEqualSlices([]const u8, &[_][]const u8{}, result.run.app_args);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "apparg1", "apparg2" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqualStrings("apparg1", result.run.app_args[0]);
        try testing.expectEqualStrings("apparg2", result.run.app_args[1]);
        try testing.expectEqual(false, result.run.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "--timings", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(true, result.run.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "--watch", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expect(result.run.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"foo.roc"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expect(result.run.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "--opt=speed", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expect(!result.run.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"-v"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--version"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "ignored.roc", "--version" });
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"-h"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--help"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "ignored.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--opt=speed" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(.speed, result.run.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--opt"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--opt=notreal"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    // Test -- separator: args after -- should go to app_args
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--", "arg1", "arg2" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(@as(usize, 2), result.run.app_args.len);
        try testing.expectEqualStrings("arg1", result.run.app_args[0]);
        try testing.expectEqualStrings("arg2", result.run.app_args[1]);
    }
    // Test -- separator is not included in app_args
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--", "onlyarg" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(usize, 1), result.run.app_args.len);
        try testing.expectEqualStrings("onlyarg", result.run.app_args[0]);
    }
    // Test flags after -- are treated as app args, not roc flags
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--", "--help", "-v", "--version" });
        defer result.deinit(gpa);
        try testing.expectEqual(.run, std.meta.activeTag(result));
        try testing.expectEqual(@as(usize, 3), result.run.app_args.len);
        try testing.expectEqualStrings("--help", result.run.app_args[0]);
        try testing.expectEqualStrings("-v", result.run.app_args[1]);
        try testing.expectEqualStrings("--version", result.run.app_args[2]);
    }
    // Test -- with flags before it still parses roc flags
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "--opt=speed", "foo.roc", "--", "arg1" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(.speed, result.run.opt);
        try testing.expectEqual(@as(usize, 1), result.run.app_args.len);
        try testing.expectEqualStrings("arg1", result.run.app_args[0]);
    }
    // Test -- without any args after it
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(@as(usize, 0), result.run.app_args.len);
    }
}

test "roc build" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"build"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(.speed, result.build.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
        try testing.expectEqual(.speed, result.build.opt);
        try testing.expectEqual(false, result.build.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--timings", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
        try testing.expectEqual(true, result.build.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"build"});
        defer result.deinit(gpa);
        try testing.expect(!result.build.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--watch", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
        try testing.expect(result.build.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--watch-inputs-file=/tmp/roc-watch-inputs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("/tmp/roc-watch-inputs", result.build.watch_inputs_file.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=size" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.size, result.build.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=dev" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.dev, result.build.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=interpreter" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.interpreter, result.build.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=notreal" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=speed", "foo/bar.roc", "--output=mypath" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo/bar.roc", result.build.path);
        try testing.expectEqual(OptLevel.speed, result.build.opt);
        try testing.expectEqualStrings("mypath", result.build.output.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=invalid" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.invalid_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        // Test --debug flag
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--debug", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
        try testing.expect(result.build.debug);
    }
    {
        // Test that debug defaults to false
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expect(!result.build.debug);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--fuzz", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
        try testing.expect(result.build.fuzz);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expect(!result.build.fuzz);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "foo.roc", "--opt=size", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--thisisactuallyafile" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--thisisactuallyafile", result.build.path);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--keep-temp" });
        defer result.deinit(gpa);
        try testing.expect(result.build.keep_temp);
    }
}

test "roc fmt" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"fmt"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.fmt.paths[0]);
        try testing.expect(!result.fmt.stdin);
        try testing.expect(!result.fmt.check);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--check" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.fmt.paths[0]);
        try testing.expect(!result.fmt.stdin);
        try testing.expect(result.fmt.check);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--stdin" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.fmt.paths[0]);
        try testing.expect(result.fmt.stdin);
        try testing.expect(!result.fmt.check);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--stdin", "--check", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.fmt.paths[0]);
        try testing.expect(result.fmt.stdin);
        try testing.expect(result.fmt.check);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.fmt.paths[0]);
        try testing.expectEqualStrings("bar.roc", result.fmt.paths[1]);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--", "--thisisactuallyafile" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--thisisactuallyafile", result.fmt.paths[0]);
    }
}

test "roc test" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"test"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.test_cmd.path);
        try testing.expectEqual(null, result.test_cmd.main);
        try testing.expectEqual(.dev, result.test_cmd.opt);
        try testing.expect(!result.test_cmd.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "foo.roc", "--opt=speed" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
        try testing.expectEqual(.speed, result.test_cmd.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--timings", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expect(result.test_cmd.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--watch", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
        try testing.expect(result.test_cmd.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--watch-inputs-file=/tmp/roc-watch-inputs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("/tmp/roc-watch-inputs", result.test_cmd.watch_inputs_file.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--opt" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--opt=notreal" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--target=wasm32", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--target=wasm32", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "foo.roc", "--target=wasm32" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--target=wasm32", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc check" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"check"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.check.path);
        try testing.expectEqual(null, result.check.main);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqual(false, result.check.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--timings", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqual(true, result.check.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--watch", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expect(result.check.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--watch-inputs-file=/tmp/roc-watch-inputs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("/tmp/roc-watch-inputs", result.check.watch_inputs_file.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--main=mymain.roc", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqualStrings("mymain.roc", result.check.main.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--time" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.check.path);
        try testing.expectEqual(true, result.check.time);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "foo.roc", "--time", "--main=bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqualStrings("bar.roc", result.check.main.?);
        try testing.expectEqual(true, result.check.time);
    }
    // --jobs flag tests
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-j1" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, 1), result.check.max_threads);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-j4" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, 4), result.check.max_threads);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--jobs=2" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, 2), result.check.max_threads);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--jobs=8" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, 8), result.check.max_threads);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--jobs=abc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--jobs", result.problem.invalid_flag_value.flag);
        try testing.expectEqualStrings("abc", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-jabc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("-j", result.problem.invalid_flag_value.flag);
        try testing.expectEqualStrings("abc", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--jobs" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--jobs", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-j" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("-j", result.problem.missing_flag_value.flag);
    }
    {
        // default is null (auto-detect)
        const result = try parse(gpa, testing.io, &[_][]const u8{"check"});
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, null), result.check.max_threads);
    }
}

test "roc repl" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"repl"});
        defer result.deinit(gpa);
        try testing.expectEqual(.repl, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "repl", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "repl", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "repl", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const parsed = try parseWithGlobalOptions(gpa, testing.io, &[_][]const u8{ "repl", "--no-color" });
        defer parsed.command.deinit(gpa);
        try testing.expectEqual(.repl, std.meta.activeTag(parsed.command));
        try testing.expect(parsed.no_color);
    }
}

test "global no-color is not forwarded to the app" {
    const gpa = testing.allocator;

    const parsed = try parseWithGlobalOptions(gpa, testing.io, &[_][]const u8{ "--no-color", "app.roc", "--", "--no-color" });
    defer parsed.command.deinit(gpa);

    try testing.expect(parsed.no_color);
    try testing.expectEqualSlices([]const u8, &.{"--no-color"}, parsed.command.run.app_args);
}

test "roc glue" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "Glue.roc", "glue-out" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("Glue.roc", result.glue.glue_spec);
        try testing.expectEqualStrings("glue-out", result.glue.output_dir);
        try testing.expectEqualStrings("main.roc", result.glue.platform_path);
        try testing.expectEqual(OptLevel.dev, result.glue.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "Glue.roc", "glue-out", "platform/main.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("platform/main.roc", result.glue.platform_path);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "--opt=size", "Glue.roc", "glue-out" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("Glue.roc", result.glue.glue_spec);
        try testing.expectEqualStrings("glue-out", result.glue.output_dir);
        try testing.expectEqual(OptLevel.size, result.glue.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "--opt=interpreter", "Glue.roc", "glue-out" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.invalid_flag_value.flag);
        try testing.expectEqualStrings("interpreter", result.problem.invalid_flag_value.value);
        try testing.expectEqualStrings("dev,size,speed", result.problem.invalid_flag_value.valid_options);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc experimental-lsp" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"experimental-lsp"});
        defer result.deinit(gpa);
        try testing.expectEqual(.experimental_lsp, std.meta.activeTag(result));
        try testing.expect(!result.experimental_lsp.debug_io);
    }
    {
        // LSP clients (e.g. vscode-languageclient) append `--stdio`; it must be
        // accepted as a no-op since stdio is the only transport we support.
        const result = try parse(gpa, testing.io, &[_][]const u8{ "experimental-lsp", "--stdio" });
        defer result.deinit(gpa);
        try testing.expectEqual(.experimental_lsp, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "experimental-lsp", "--debug-transport", "--stdio" });
        defer result.deinit(gpa);
        try testing.expectEqual(.experimental_lsp, std.meta.activeTag(result));
        try testing.expect(result.experimental_lsp.debug_io);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "experimental-lsp", "--bogus" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("experimental-lsp", result.problem.unexpected_argument.cmd);
        try testing.expectEqualStrings("--bogus", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "experimental-lsp", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc version" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"version"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "version", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "version", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "version", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc docs" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"docs"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.docs.path);
        try testing.expectEqual(null, result.docs.main);
        try testing.expectEqualStrings("generated-docs", result.docs.output);
        try testing.expectEqual(false, result.docs.time);
        try testing.expectEqual(false, result.docs.no_cache);
        try testing.expectEqual(false, result.docs.verbose);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("generated-docs", result.docs.output);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--main=mymain.roc", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("mymain.roc", result.docs.main.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--output=my-docs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("my-docs", result.docs.output);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--time" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.docs.path);
        try testing.expectEqual(true, result.docs.time);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc", "--time", "--main=bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("bar.roc", result.docs.main.?);
        try testing.expectEqual(true, result.docs.time);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--no-cache" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.docs.no_cache);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--verbose" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.docs.verbose);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--with-lang-ref" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.docs.with_lang_ref);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqual(false, result.docs.with_lang_ref);
        try testing.expectEqual(false, result.docs.builtins);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--builtins", "--with-lang-ref", "--output=site" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.docs.builtins);
        try testing.expectEqual(true, result.docs.with_lang_ref);
        try testing.expectEqualStrings("site", result.docs.output);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--builtins", "Builtin.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("Builtin.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--main=main.roc", "--builtins" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--main", result.problem.unexpected_argument.arg);
    }
}

test "roc bump" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "bump", "--old", "https://example.com/pkg/1.2.3/hash.tar.zst" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.bump.path);
        try testing.expectEqualStrings("https://example.com/pkg/1.2.3/hash.tar.zst", result.bump.old);
        try testing.expectEqual(null, result.bump.old_version);
        try testing.expectEqual(false, result.bump.no_cache);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "bump", "--old", "old_pkg", "--old-version", "1.2.3", "new_pkg/main.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("new_pkg/main.roc", result.bump.path);
        try testing.expectEqualStrings("old_pkg", result.bump.old);
        try testing.expectEqualStrings("1.2.3", result.bump.old_version.?);
        try testing.expectEqual(null, result.bump.expect);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "bump", "--old", "old_pkg", "--old-version", "1.2.3", "--expect", "2.0.0" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("2.0.0", result.bump.expect.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "bump", "--old", "old_pkg", "--expect" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--expect", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"bump"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--old", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "bump", "--old" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--old", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "bump", "--old", "a", "b.roc", "c.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("c.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "bump", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "bump", "--old", "a", "--no-cache", "--verbose" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.bump.no_cache);
        try testing.expectEqual(true, result.bump.verbose);
    }
}

test "roc help" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"help"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "help", "extrastuff" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "dependency-resolving command help lists size limit flags" {
    const gpa = testing.allocator;
    const cases: []const []const []const u8 = &.{
        &.{"--help"},
        &.{ "run", "--help" },
        &.{ "install", "--help" },
        &.{ "check", "--help" },
        &.{ "build", "--help" },
        &.{ "test", "--help" },
        &.{ "docs", "--help" },
        &.{ "bump", "--help" },
    };

    for (cases) |args| {
        const result = try parse(gpa, testing.io, args);
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
        try testing.expect(std.mem.find(u8, result.help, "--max-package-mb") != null);
        try testing.expect(std.mem.find(u8, result.help, "--max-transitive-mb") != null);
        try testing.expect(std.mem.find(u8, result.help, "packages 100, platforms 512") != null);
    }
}

test "roc licenses" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"licenses"});
        defer result.deinit(gpa);
        try testing.expectEqual(.licenses, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "licenses", "extrastuff" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("extrastuff", result.problem.unexpected_argument.arg);
    }
}

test "roc install" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "install", "tokei", "https://example.com/tokei/1.2.3/abc.tar.zst" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("tokei", result.install.shorthand);
        try testing.expectEqualStrings("https://example.com/tokei/1.2.3/abc.tar.zst", result.install.url);
        try testing.expectEqual(@as(?usize, null), result.install.max_threads);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "install", "--jobs=4", "--max-package-mb=20", "tokei", "https://example.com/tokei/1.2.3/abc.tar.zst" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, 4), result.install.max_threads);
        try testing.expectEqual(@as(?u32, 20), result.install.resolve_limits.max_package_mb);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "install", "tokei" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"install"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "install", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "install", "a", "https://example.com/x/1.0.0/abc.tar.zst", "extra" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("install", result.problem.unexpected_argument.cmd);
        try testing.expectEqualStrings("extra", result.problem.unexpected_argument.arg);
    }
}

test "roc run subcommand" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "run", "tokei", "--", "arg1", "arg2" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("tokei", result.run.path);
        try testing.expect(result.run.via_run_subcommand);
        try testing.expectEqual(@as(usize, 2), result.run.app_args.len);
        try testing.expectEqualStrings("arg1", result.run.app_args[0]);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "run", "app.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("app.roc", result.run.path);
        try testing.expect(result.run.via_run_subcommand);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "run", "https://example.com/x/1.0.0/abc.tar.zst" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("https://example.com/x/1.0.0/abc.tar.zst", result.run.path);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"run"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.run.path);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "run", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "run", "tokei", "--watch" });
        defer result.deinit(gpa);
        try testing.expect(result.run.explicit_watch);
        try testing.expect(!result.run.explicit_opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "run", "tokei", "--opt=dev" });
        defer result.deinit(gpa);
        try testing.expect(result.run.explicit_opt);
    }
}

test "bare shorthand requires the run subcommand" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"tokei"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("tokei", result.problem.shorthand_requires_run.name);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"./tokei"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("./tokei", result.run.path);
        try testing.expect(!result.run.via_run_subcommand);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"https://example.com/x/1.0.0/abc.tar.zst"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("https://example.com/x/1.0.0/abc.tar.zst", result.run.path);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"app.roc"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("app.roc", result.run.path);
    }
}

test "roc deps" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"deps"});
        try testing.expectEqualStrings("main.roc", result.deps.path);
        try testing.expectEqual(@as(usize, 0), result.deps.resolve_limits.replace_deps.len);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "deps", "pkg/main.roc", "--max-package-mb=5" });
        try testing.expectEqualStrings("pkg/main.roc", result.deps.path);
        try testing.expectEqual(@as(?u32, 5), result.deps.resolve_limits.max_package_mb);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "deps", "a.roc", "b.roc" });
        try testing.expectEqualStrings("b.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "deps", "--help" });
        try testing.expect(std.mem.find(u8, result.help, "--replace-dep OLD NEW") != null);
    }
}

test "--replace-dep takes two arguments, repeats, and is position-independent" {
    const gpa = testing.allocator;
    const url = "https://example.com/pkg/1.2.3/abc.tar.zst";
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--replace-dep", url, "../pkg/main.roc", "app.roc" });
        const deps = result.check.resolve_limits.replace_deps.slice();
        try testing.expectEqualStrings("app.roc", result.check.path);
        try testing.expectEqual(@as(usize, 1), deps.len);
        try testing.expectEqualStrings(url, deps[0].old);
        try testing.expectEqualStrings("../pkg/main.roc", deps[0].new);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "app.roc", "--replace-dep", url, "../a/main.roc", "--replace-dep", "./b/main.roc", url });
        const deps = result.test_cmd.resolve_limits.replace_deps.slice();
        try testing.expectEqual(@as(usize, 2), deps.len);
        try testing.expectEqualStrings("./b/main.roc", deps[1].old);
        try testing.expectEqualStrings(url, deps[1].new);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--replace-dep", url, "../pkg/main.roc", "app.roc" });
        try testing.expectEqual(@as(usize, 1), result.build.resolve_limits.replace_deps.len);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--replace-dep", url, "../pkg/main.roc", "main.roc" });
        try testing.expectEqual(@as(usize, 1), result.docs.resolve_limits.replace_deps.len);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "deps", "--replace-dep", url, "../pkg/main.roc" });
        try testing.expectEqual(@as(usize, 1), result.deps.resolve_limits.replace_deps.len);
        try testing.expectEqualStrings("main.roc", result.deps.path);
    }
    // Implicit and explicit run both accept it; app args after `--` are untouched.
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "app.roc", "--replace-dep", url, "../pkg/main.roc", "--", "--replace-dep", "x", "y" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(usize, 1), result.run.resolve_limits.replace_deps.len);
        try testing.expectEqual(@as(usize, 3), result.run.app_args.len);
        try testing.expectEqualStrings("--replace-dep", result.run.app_args[0]);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "run", "--replace-dep", url, "../pkg/main.roc", "app.roc" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(usize, 1), result.run.resolve_limits.replace_deps.len);
        try testing.expectEqualStrings("app.roc", result.run.path);
    }
}

test "--replace-dep rejects missing values, the = form, and non-resolving commands" {
    const gpa = testing.allocator;
    const url = "https://example.com/pkg/1.2.3/abc.tar.zst";
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "app.roc", "--replace-dep", url });
        try testing.expectEqualStrings("--replace-dep OLD NEW", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--replace-dep", url, "--", "x" });
        try testing.expectEqualStrings("--replace-dep OLD NEW", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--replace-dep=a=b", "app.roc" });
        try testing.expectEqualStrings("--replace-dep", result.problem.invalid_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--replace-dep", url, "../pkg/main.roc", "app.roc" });
        try testing.expectEqualStrings("fmt", result.problem.unexpected_argument.cmd);
        try testing.expectEqualStrings("--replace-dep", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "bundle", "--replace-dep", url, "../pkg/main.roc", "main.roc" });
        try testing.expectEqualStrings("bundle", result.problem.unexpected_argument.cmd);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "install", "--replace-dep", url, "../pkg/main.roc", "tool", url });
        try testing.expectEqualStrings("install", result.problem.unexpected_argument.cmd);
    }
    // Neither `--help` nor a missing-argument problem in the command hides the rejection.
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--replace-dep", url, "../pkg/main.roc", "--help" });
        try testing.expectEqualStrings("fmt", result.problem.unexpected_argument.cmd);
        try testing.expectEqualStrings("--replace-dep", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "--replace-dep", url, "../pkg/main.roc" });
        try testing.expectEqualStrings("glue", result.problem.unexpected_argument.cmd);
        try testing.expectEqualStrings("--replace-dep", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "help", "--replace-dep", url, "../pkg/main.roc" });
        try testing.expectEqualStrings("help", result.problem.unexpected_argument.cmd);
    }
    // Resolving commands still get their own help and problems.
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--replace-dep", url, "../pkg/main.roc", "--help" });
        try testing.expect(std.mem.find(u8, result.help, "--replace-dep OLD NEW") != null);
    }
}

test "issue 5181: misspelled formatter flags identify the offending argument" {
    const gpa = testing.allocator;
    // spellchecker:ignore-next-line
    for ([_][]const u8{ "--chek", "-x", "--stdiin" }) |flag| {
        const result = try parse(gpa, testing.io, &.{ "fmt", flag, "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqual(.problem, std.meta.activeTag(result));
        try testing.expectEqualStrings("fmt", result.problem.unexpected_argument.cmd);
        try testing.expectEqualStrings(flag, result.problem.unexpected_argument.arg);
    }
}

test "formatter accepts flag-like filenames after the option terminator" {
    const gpa = testing.allocator;
    const result = try parse(gpa, testing.io, &.{ "fmt", "--", "-x.roc" });
    defer result.deinit(gpa);
    try testing.expectEqualStrings("-x.roc", result.fmt.paths[0]);
}

test "every flag sets the field its table entry names" {
    const gpa = testing.allocator;
    inline for (commands ++ [_]Command{default_command}) |cmd| {
        if (cmd.tag != .help) {
            // What the command cannot parse without, so each flag is tried on
            // an otherwise valid command line.
            const required: []const []const u8 = comptime required: {
                @setEvalBranchQuota(100_000);
                var list: []const []const u8 = &.{};
                for (cmd.positionals) |positional| {
                    if (positional.missing != null) list = list ++ .{"sample.roc"};
                }
                for (cmd.allFlags()) |flag| {
                    if (cmd.requires(flag)) list = list ++ flagSample(cmd, flag, flag.name);
                }
                break :required list;
            };

            inline for (comptime cmd.allFlags()) |flag| {
                if (flag.field) |path| {
                    const spellings: []const []const u8 = comptime if (flag.short) |short| &.{ flag.name, short } else &.{flag.name};
                    inline for (spellings) |written| {
                        const result = try parseArgsFor(cmd, gpa, required ++ comptime flagSample(cmd, flag, written));
                        defer result.deinit(gpa);
                        try testing.expectEqual(cmd.tag, std.meta.activeTag(result));

                        var args = @field(result, @tagName(cmd.tag));
                        const value = fieldAt(&args, path).*;
                        const Field = @TypeOf(value);
                        if (Field == bool) {
                            try testing.expect(value);
                        } else if (Field == OptLevel) {
                            try testing.expectEqual(flag.levels[flag.levels.len - 1], value);
                        } else if (Field == ?SpecializationStrategy) {
                            try testing.expectEqual(SpecializationStrategy.boxy, value.?);
                        } else if (Field == []const u8) {
                            try testing.expectEqualStrings("sample", value);
                        } else if (Field == ?[]const u8) {
                            try testing.expectEqualStrings("sample", value.?);
                        } else {
                            try testing.expectEqual(@as(Field, 7), value);
                        }
                        if (flag.marks) |name| try testing.expect(@field(args, name));
                    }
                }
            }
        }
    }
}

/// A command line that gives `flag` a value its field accepts, spelled `written`.
fn flagSample(comptime cmd: Command, comptime flag: Flag, comptime written: []const u8) []const []const u8 {
    @setEvalBranchQuota(100_000);
    const Field = fieldInfoAt(cmd.Args(), flag.field.?).type;
    const sample = if (Field == OptLevel)
        @tagName(flag.levels[flag.levels.len - 1])
    else if (Field == ?SpecializationStrategy)
        "no"
    else if (Field == []const u8 or Field == ?[]const u8)
        "sample"
    else
        "7";
    return switch (flag.form) {
        .toggle => &.{written},
        .attached => &.{written ++ (if (mem.eql(u8, written, flag.name)) "=" else "") ++ sample},
        .separate => &.{ written, sample },
        .terminator, .version, .rejected, .documented => unreachable,
    };
}

test "every listed flag is in its command's help, and no unlisted one is" {
    inline for (commands ++ [_]Command{default_command}) |cmd| {
        if (cmd.tag != .help) {
            const help = comptime cmd.helpText();
            inline for (comptime cmd.allFlags()) |flag| {
                const listed = std.mem.find(u8, help, comptime flag.spelling() ++ " ") != null or
                    std.mem.find(u8, help, comptime flag.spelling() ++ "\n") != null;
                try testing.expectEqual(flag.help != null, listed);
            }
        }
    }
}

test "a missing required argument prints the command's help" {
    const gpa = testing.allocator;
    const help = try parse(gpa, testing.io, &.{ "glue", "--help" });
    {
        const result = try parse(gpa, testing.io, &.{"glue"});
        try testing.expectEqualStrings("Error: Missing required argument <GLUE_SPEC>\n\n", result.help[0 .. result.help.len - help.help.len]);
        try testing.expectEqualStrings(help.help, result.help[result.help.len - help.help.len ..]);
    }
    {
        const result = try parse(gpa, testing.io, &.{ "glue", "Glue.roc" });
        try testing.expectEqualStrings("Error: Missing required argument <GLUE_DIR>\n\n", result.help[0 .. result.help.len - help.help.len]);
        try testing.expectEqualStrings(help.help, result.help[result.help.len - help.help.len ..]);
    }
    {
        const install_help = try parse(gpa, testing.io, &.{ "install", "--help" });
        const result = try parse(gpa, testing.io, &.{ "install", "tokei" });
        try testing.expectEqualStrings(install_help.help, result.help);
    }
}

test "roc test takes --main with an attached value, as its help says" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &.{ "test", "--main=app.roc", "foo.roc" });
        try testing.expectEqualStrings("app.roc", result.test_cmd.main.?);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
    }
    {
        const result = try parse(gpa, testing.io, &.{ "test", "--main", "app.roc" });
        try testing.expectEqualStrings("--main", result.problem.missing_flag_value.flag);
    }
}

test "the valid options of --opt are the levels the command accepts" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &.{ "build", "--opt=fast" });
        try testing.expectEqualStrings("dev,interpreter,speed,size", result.problem.invalid_flag_value.valid_options);
    }
    {
        const result = try parse(gpa, testing.io, &.{ "glue", "--opt=fast", "Glue.roc", "out" });
        try testing.expectEqualStrings("dev,size,speed", result.problem.invalid_flag_value.valid_options);
    }
}

test "numeric flag values report what they must be" {
    const gpa = testing.allocator;
    const cases = [_]struct { args: []const []const u8, flag: []const u8, valid_options: []const u8 }{
        .{ .args = &.{ "bundle", "--compression", "23" }, .flag = "--compression", .valid_options = "integer between 1 and 22" },
        .{ .args = &.{ "bundle", "--compression", "0" }, .flag = "--compression", .valid_options = "integer between 1 and 22" },
        .{ .args = &.{ "bundle", "--compression", "x" }, .flag = "--compression", .valid_options = "integer between 1 and 22" },
        .{ .args = &.{ "build", "--wasm-memory=x" }, .flag = "--wasm-memory", .valid_options = "positive integer (bytes)" },
        .{ .args = &.{ "build", "--wasm-stack-size=-1" }, .flag = "--wasm-stack-size", .valid_options = "positive integer (bytes)" },
        .{ .args = &.{ "check", "--max-package-mb=x" }, .flag = "--max-package-mb", .valid_options = "size in MB (0 for unlimited)" },
        .{ .args = &.{ "check", "--jobs=x" }, .flag = "--jobs", .valid_options = "positive integer" },
    };
    for (cases) |case| {
        const result = try parse(gpa, testing.io, case.args);
        defer result.deinit(gpa);
        try testing.expectEqualStrings(case.flag, result.problem.invalid_flag_value.flag);
        try testing.expectEqualStrings(case.valid_options, result.problem.invalid_flag_value.valid_options);
    }
    {
        const result = try parse(gpa, testing.io, &.{ "bundle", "--compression", "22", "--output-dir", "out", "a.roc" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(i32, 22), result.bundle.compression_level);
        try testing.expectEqualStrings("out", result.bundle.output_dir.?);
    }
}

fn expectHelp(args: []const []const u8, expected: []const u8) (ParseError || error{TestExpectedEqual})!void {
    const result = try parse(testing.allocator, testing.io, args);
    defer result.deinit(testing.allocator);
    try testing.expectEqualStrings(expected, result.help);
}

test "golden help: roc --help" {
    try expectHelp(&.{"--help"},
        \\Run the given .roc file
        \\You can use one of the COMMANDS below to do something else!
        \\
        \\Usage: roc [OPTIONS] [ROC_FILE] [ARGS_FOR_APP]...
        \\       roc <COMMAND>
        \\
        \\Commands:
        \\  run              Run a .roc file, a bundle URL, or an installed shorthand
        \\  install          Install a Roc app or glue spec from a bundle URL under a shorthand name
        \\  build            Build a binary from the given .roc file, but don't run it
        \\  bundle           Bundle .roc files into a compressed archive
        \\  unbundle         Extract files from compressed .tar.zst archives
        \\  test             Run all top-level `expect`s in a module, and in the modules and path dependencies it imports
        \\  repl             Launch the interactive Read Eval Print Loop (REPL)
        \\  fmt              Format a .roc file or the .roc files contained in a directory using standard Roc formatting
        \\  glue             Generate native glue code from a Roc platform using a language-specific glue spec
        \\  version          Print the Roc compiler's version
        \\  check            Check the code for problems, but don't build or run it
        \\  docs             Generate documentation for a Roc package or platform
        \\  deps             Print the dependency tree of a Roc app, package, or platform
        \\  bump             Compare a package's public API against a previous version and report the required semver bump
        \\  experimental-lsp Start the experimental language server (LSP) implementation
        \\  help             Print this message
        \\  licenses         Prints license info for Roc as well as attributions to other projects used by Roc
        \\
        \\Arguments:
        \\  [ROC_FILE]         The .roc file of an app to run [default: main.roc]
        \\  [ARGS_FOR_APP]...  Arguments to pass into the app being run
        \\                     e.g. `roc app.roc -- arg1 arg2`
        \\
        \\Options:
        \\      --opt=<opt>                    Execution mode: dev (native dev backend, fast compilation), interpreter (interpreted, no code generation), speed (LLVM, optimized for execution speed), or size (LLVM, optimized for binary size) [default: dev]
        \\      --specialize=<yes|no>          Use lambda-set specialization (yes, default) or experimental boxy lowering (no)
        \\      --target=<target>              Target to compile for. A v1 in the name (x64v1musl) targets the oldest CPUs of that architecture. Defaults to native target with musl for static linking. One of:
        \\                                       x64musl, arm64musl, arm32musl                           - Linux (static, portable)
        \\                                       x64glibc, x64linux, arm64linux, arm64glibc, arm32linux  - Linux (dynamic, faster)
        \\                                       x64mac, arm64mac                                        - macOS
        \\                                       x64win, arm64win                                        - Windows (MSVC)
        \\                                       x64mingw, arm64mingw                                    - Windows (MinGW)
        \\                                       x64freebsd, x64openbsd, x64netbsd                       - BSD
        \\                                       x64elf                                                  - Freestanding ELF
        \\                                       wasm32                                                  - WebAssembly
        \\      --no-cache                     Disable compilation and executable caches (useful for compiler and platform developers)
        \\      --no-color                     Do not use ANSI escape codes in CLI output
        \\  -j, --jobs=<N>                     Max worker threads for parallel compilation (default: auto-detect CPU count)
        \\      --replace-dep OLD NEW    Load NEW wherever a dependency declares exactly OLD, for this invocation only.
        \\                               Each is a complete package URL or a path to a root .roc file; repeatable.
        \\                               Run `roc deps` to see the declared sources
        \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
        \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages
        \\                               (defaults: packages 100, platforms 512; 0 for unlimited)
        \\  -h, --help                         Print help
        \\
    );
}

test "golden help: roc run --help" {
    try expectHelp(&.{ "run", "--help" },
        \\Run a Roc application
        \\
        \\Usage: roc run [OPTIONS] [SOURCE] [-- [ARGS_FOR_APP]...]
        \\
        \\SOURCE may be:
        \\  a .roc file path        roc run main.roc
        \\  a bundle URL            roc run https://example.com/tool/1.2.3/<hash>.tar.zst
        \\  an installed shorthand  roc run tokei      (see `roc install`)
        \\
        \\Running an installed shorthand executes the optimized binary that was
        \\built at install time; no compilation or network access is needed.
        \\File and URL sources accept the same options as the default `roc` command.
        \\
        \\Options:
        \\      --opt=<opt>                Execution mode: dev (native dev backend, fast compilation), interpreter (interpreted, no code generation), speed (LLVM, optimized for execution speed), or size (LLVM, optimized for binary size) [default: dev]
        \\      --specialize=<yes|no>      Use lambda-set specialization (yes, default) or experimental boxy lowering (no)
        \\      --target=<target>          Target to compile for. A v1 in the name (x64v1musl) targets the oldest CPUs of that architecture. Defaults to native target with musl for static linking. One of:
        \\                                   x64musl, arm64musl, arm32musl                           - Linux (static, portable)
        \\                                   x64glibc, x64linux, arm64linux, arm64glibc, arm32linux  - Linux (dynamic, faster)
        \\                                   x64mac, arm64mac                                        - macOS
        \\                                   x64win, arm64win                                        - Windows (MSVC)
        \\                                   x64mingw, arm64mingw                                    - Windows (MinGW)
        \\                                   x64freebsd, x64openbsd, x64netbsd                       - BSD
        \\                                   x64elf                                                  - Freestanding ELF
        \\                                   wasm32                                                  - WebAssembly
        \\      --no-cache                 Disable compilation and executable caches (useful for compiler and platform developers)
        \\      --no-color                 Do not use ANSI escape codes in CLI output
        \\  -j, --jobs=<N>                 Max worker threads for parallel compilation (default: auto-detect CPU count)
        \\      --replace-dep OLD NEW    Load NEW wherever a dependency declares exactly OLD, for this invocation only.
        \\                               Each is a complete package URL or a path to a root .roc file; repeatable.
        \\                               Run `roc deps` to see the declared sources
        \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
        \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages
        \\                               (defaults: packages 100, platforms 512; 0 for unlimited)
        \\  -h, --help                     Print help
        \\
    );
}

test "golden help: roc install --help" {
    try expectHelp(&.{ "install", "--help" },
        \\Install a Roc app or glue spec from a bundle URL under a shorthand name
        \\
        \\Usage: roc install [OPTIONS] <SHORTHAND> <URL>
        \\
        \\Downloads the bundle, verifies its content hash, and builds it with
        \\--opt=speed. An app becomes an optimized binary that `roc run
        \\<SHORTHAND>` executes with no compile step; a glue spec becomes an
        \\optimized plugin dylib that `roc glue <SHORTHAND> ...` loads directly.
        \\Installations persist outside the cache and are scoped to the compiler
        \\version that installed them.
        \\
        \\Arguments:
        \\  <SHORTHAND>  A name of your choice: a lowercase letter followed by
        \\               lowercase letters, digits, or underscores
        \\  <URL>        A .tar.zst bundle URL ending in a base58-encoded BLAKE3 hash
        \\
        \\Options:
        \\  -j, --jobs=<N>                 Max worker threads for the install-time build
        \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
        \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages
        \\                               (defaults: packages 100, platforms 512; 0 for unlimited)
        \\  -h, --help                     Print help
        \\
    );
}

test "golden help: roc build --help" {
    try expectHelp(&.{ "build", "--help" },
        \\Build a binary from the given .roc file, but don't run it
        \\
        \\Usage: roc build [OPTIONS] [ROC_FILE]
        \\
        \\Arguments:
        \\  [ROC_FILE] The .roc file to build [default: main.roc]
        \\
        \\Options:
        \\      --output=<output>              The full path to the output binary, including filename. To specify directory only, specify a path that ends in a directory separator (e.g. a slash)
        \\      --opt=<opt>                    Build mode: dev (native dev backend, fast compilation), interpreter (interpreted, no code generation), speed (LLVM, optimized for execution speed), or size (LLVM, optimized for binary size) [default: speed]
        \\      --specialize=<yes|no>          Use lambda-set specialization (yes, default) or experimental boxy lowering (no)
        \\      --target=<target>              Target to compile for. A v1 in the name (x64v1musl) targets the oldest CPUs of that architecture. Defaults to native target with musl for static linking. One of:
        \\                                       x64musl, arm64musl, arm32musl                           - Linux (static, portable)
        \\                                       x64glibc, x64linux, arm64linux, arm64glibc, arm32linux  - Linux (dynamic, faster)
        \\                                       x64mac, arm64mac                                        - macOS
        \\                                       x64win, arm64win                                        - Windows (MSVC)
        \\                                       x64mingw, arm64mingw                                    - Windows (MinGW)
        \\                                       x64freebsd, x64openbsd, x64netbsd                       - BSD
        \\                                       x64elf                                                  - Freestanding ELF
        \\                                       wasm32                                                  - WebAssembly
        \\      --debug                        Include debug information in the output binary
        \\      --fuzz                         Add libFuzzer no-link coverage instrumentation; final linkage must provide the runtime
        \\      --keep-temp                    Keep all temporary directories created during build
        \\      --verbose                      Enable verbose output including cache statistics
        \\      --timings                      Show how long each compilation phase took (shown automatically when a build is slow)
        \\      --no-cache                     Disable compilation caching
        \\      --watch                        Rebuild when source inputs change
        \\  -j, --jobs=<N>                     Max worker threads for parallel compilation (default: auto-detect CPU count)
        \\      --wasm-memory=<bytes>          Initial memory size for WASM targets in bytes (default: sized from data segments plus the stack)
        \\      --wasm-stack-size=<bytes>      Stack size for WASM targets in bytes (default: 8388608 = 8MB)
        \\      --replace-dep OLD NEW    Load NEW wherever a dependency declares exactly OLD, for this invocation only.
        \\                               Each is a complete package URL or a path to a root .roc file; repeatable.
        \\                               Run `roc deps` to see the declared sources
        \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
        \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages
        \\                               (defaults: packages 100, platforms 512; 0 for unlimited)
        \\  -h, --help                         Print help
        \\
    );
}

test "golden help: roc bundle --help" {
    try expectHelp(&.{ "bundle", "--help" },
        \\Bundle .roc files into a compressed archive
        \\
        \\Usage: roc bundle [OPTIONS] [ROC_FILES]...
        \\
        \\Arguments:
        \\  [ROC_FILES]...  The .roc files to bundle [default: main.roc]
        \\
        \\Options:
        \\      --output-dir <PATH>  Directory to output the bundle to [default: current directory]
        \\      --compression <N>    Compression level (1-22) [default: 3]
        \\  -h, --help               Print help
        \\
    );
}

test "golden help: roc unbundle --help" {
    try expectHelp(&.{ "unbundle", "--help" },
        \\Extract files from compressed .tar.zst archives
        \\
        \\Usage: roc unbundle [OPTIONS] [ARCHIVE_FILES]...
        \\
        \\Arguments:
        \\  [ARCHIVE_FILES]...  The .tar.zst files to unbundle
        \\                      [default: all .tar.zst files in current directory]
        \\
        \\Options:
        \\  -h, --help  Print help
        \\
    );
}

test "golden help: roc test --help" {
    try expectHelp(&.{ "test", "--help" },
        \\Run all top-level `expect`s in a main module and any modules it imports
        \\
        \\Dependencies reached through a filesystem path are tested too, because
        \\they are yours to edit. Dependencies downloaded from a URL are not: their
        \\`expect`s belong to whoever published them.
        \\
        \\Usage: roc test [OPTIONS] [ROC_FILE]
        \\
        \\Arguments:
        \\  [ROC_FILE] The .roc file to test [default: main.roc]
        \\
        \\Options:
        \\      --opt=<opt>                     Execution mode: dev (native dev backend, fast compilation), interpreter (interpreted, no code generation), speed (LLVM, optimized for execution speed), or size (LLVM, optimized for binary size) [default: dev]
        \\      --specialize=<yes|no>           Use lambda-set specialization (yes, default) or experimental boxy lowering (no)
        \\      --main=<main>                   The .roc file of the main app/package module to resolve dependencies from
        \\      --verbose                       Enable verbose output showing individual test results
        \\      --timings                       Show how long each compilation and test phase took
        \\      --no-cache                      Disable compilation caching, force re-run all tests
        \\      --watch                         Re-run when source inputs change
        \\  -j, --jobs=<N>                      Max worker threads for parallel compilation (default: auto-detect CPU count)
        \\      --replace-dep OLD NEW    Load NEW wherever a dependency declares exactly OLD, for this invocation only.
        \\                               Each is a complete package URL or a path to a root .roc file; repeatable.
        \\                               Run `roc deps` to see the declared sources
        \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
        \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages
        \\                               (defaults: packages 100, platforms 512; 0 for unlimited)
        \\  -h, --help                          Print help
        \\
    );
}

test "golden help: roc repl --help" {
    try expectHelp(&.{ "repl", "--help" },
        \\Launch the interactive Read Eval Print Loop (REPL)
        \\
        \\Usage: roc repl [OPTIONS]
        \\
        \\Options:
        \\      --opt=<opt>            Execution mode: dev (native dev backend, fast compilation), interpreter (interpreted, no code generation), speed (LLVM, optimized for execution speed), or size (LLVM, optimized for binary size) [default: dev]
        \\      --specialize=<yes|no>  Use lambda-set specialization (yes, default) or experimental boxy lowering (no)
        \\  -h, --help                 Print help
        \\
    );
}

test "golden help: roc fmt --help" {
    try expectHelp(&.{ "fmt", "--help" },
        \\Format a .roc file or the .roc files contained in a directory using standard Roc formatting
        \\
        \\Usage: roc fmt [OPTIONS] [DIRECTORY_OR_FILES]
        \\
        \\Arguments:
        \\  [DIRECTORY_OR_FILES]
        \\
        \\Options:
        \\      --check  Checks that specified files are formatted
        \\               (If formatting is needed, return a non-zero exit code.)
        \\      --stdin  Format code from stdin; output to stdout
        \\      --       Treat all remaining arguments as paths
        \\  -h, --help   Print help
        \\
        \\If DIRECTORY_OR_FILES is omitted, the .roc files in the current working directory are formatted.
        \\
    );
}

test "golden help: roc glue --help" {
    try expectHelp(&.{ "glue", "--help" },
        \\Generate glue code from a platform using a glue spec
        \\
        \\Usage: roc glue [OPTIONS] <GLUE_SPEC> <GLUE_DIR> [ROC_FILE]
        \\
        \\Arguments:
        \\  <GLUE_SPEC>  The glue spec .roc file that defines how to generate glue code
        \\  <GLUE_DIR>   The output directory for generated glue files
        \\  [ROC_FILE]   The platform .roc file to analyze [default: main.roc]
        \\
        \\Options:
        \\      --opt=<opt>            Compile and run the glue spec with dev (native dev backend, fast compilation), size (LLVM, optimized for binary size), or speed (LLVM, optimized for execution speed) [default: dev]
        \\      --specialize=<yes|no>  Use lambda-set specialization (yes, default) or experimental boxy lowering (no)
        \\      --no-cache             Disable compilation caching
        \\  -h, --help                 Print help
        \\
    );
}

test "golden help: roc version --help" {
    try expectHelp(&.{ "version", "--help" },
        \\Print the Roc compiler’s version
        \\
        \\Usage: roc version
        \\
        \\Options:
        \\  -h, --help  Print help
        \\
    );
}

test "golden help: roc check --help" {
    try expectHelp(&.{ "check", "--help" },
        \\Check the code for problems, but don't build or run it
        \\
        \\Usage: roc check [OPTIONS] [ROC_FILE]
        \\
        \\Arguments:
        \\  [ROC_FILE]  The .roc file to check [default: main.roc]
        \\
        \\Options:
        \\      --main=<main>  The .roc file of the main app/package module to resolve dependencies from
        \\      --time         Print timing information for each compilation phase. Will not print anything if everything is cached.
        \\      --timings      Show how long each compilation phase took (shown automatically when checking is slow)
        \\      --no-cache     Disable caching
        \\      --verbose      Enable verbose output including cache statistics
        \\      --watch        Re-run when source inputs change
        \\  -j, --jobs=<N>     Max worker threads for parallel compilation (default: auto-detect CPU count)
        \\      --replace-dep OLD NEW    Load NEW wherever a dependency declares exactly OLD, for this invocation only.
        \\                               Each is a complete package URL or a path to a root .roc file; repeatable.
        \\                               Run `roc deps` to see the declared sources
        \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
        \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages
        \\                               (defaults: packages 100, platforms 512; 0 for unlimited)
        \\  -h, --help         Print help
        \\
    );
}

test "golden help: roc docs --help" {
    try expectHelp(&.{ "docs", "--help" },
        \\Generate documentation for a Roc package
        \\
        \\Usage: roc docs [OPTIONS] [ROC_FILE]
        \\
        \\Arguments:
        \\  [ROC_FILE]  The .roc file to generate docs for [default: main.roc]
        \\
        \\Options:
        \\      --main=<main>    The .roc file of the main app/package module to resolve dependencies from
        \\      --output=<dir>   Output directory for generated documentation [default: generated-docs]
        \\      --serve          Start an HTTP server to view the documentation
        \\      --with-lang-ref  Include the language reference articles from docs/langref
        \\      --time           Print timing information for each compilation phase. Will not print anything if everything is cached.
        \\      --no-cache       Disable caching
        \\      --verbose        Enable verbose output including cache statistics
        \\      --replace-dep OLD NEW    Load NEW wherever a dependency declares exactly OLD, for this invocation only.
        \\                               Each is a complete package URL or a path to a root .roc file; repeatable.
        \\                               Run `roc deps` to see the declared sources
        \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
        \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages
        \\                               (defaults: packages 100, platforms 512; 0 for unlimited)
        \\  -h, --help           Print help
        \\
    );
}

test "golden help: roc deps --help" {
    try expectHelp(&.{ "deps", "--help" },
        \\Print the dependency tree of a Roc app, package, or platform
        \\
        \\Resolves the dependency graph and prints every declared source in full,
        \\ready to copy into --replace-dep. Nothing is compiled or run. Packages
        \\that are not cached yet are downloaded so their headers can be read.
        \\
        \\Usage: roc deps [OPTIONS] [ROC_FILE]
        \\
        \\Arguments:
        \\  [ROC_FILE]  The root .roc file of the app, package, or platform [default: main.roc]
        \\
        \\Options:
        \\      --replace-dep OLD NEW    Load NEW wherever a dependency declares exactly OLD, for this invocation only.
        \\                               Each is a complete package URL or a path to a root .roc file; repeatable.
        \\                               Run `roc deps` to see the declared sources
        \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
        \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages
        \\                               (defaults: packages 100, platforms 512; 0 for unlimited)
        \\  -h, --help                   Print help
        \\
    );
}

test "golden help: roc bump --help" {
    try expectHelp(&.{ "bump", "--help" },
        \\Compare a package's public API against a previous version and report the
        \\required semver bump (patch, minor, or major) plus the next version.
        \\
        \\Usage: roc bump --old <OLD> [OPTIONS] [ROC_FILE]
        \\
        \\Arguments:
        \\  [ROC_FILE]  The new package's main .roc file [default: main.roc]
        \\
        \\Options:
        \\      --old <OLD>            The previous package version: a package URL, a
        \\                             .tar.zst bundle, a directory, or a main .roc file
        \\      --old-version <X.Y.Z>  The previous version number (required unless
        \\                             --old is a URL with a version path segment)
        \\      --expect <X.Y.Z>       Fail unless this version bumps at least as far
        \\                             as the API diff requires (for release CI)
        \\      --no-cache             Disable caching
        \\      --verbose              Enable verbose output
        \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
        \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages
        \\                               (defaults: packages 100, platforms 512; 0 for unlimited)
        \\  -h, --help                 Print help
        \\
        \\Both the old and new package must compile with this compiler. Only the
        \\modules exposed by the package header are compared; platform
        \\provides/requires are not yet part of the comparison.
        \\
        \\If this is the package's first release, there is nothing to compare—
        \\publish it as 1.0.0.
        \\
    );
}

test "golden help: roc experimental-lsp --help" {
    try expectHelp(&.{ "experimental-lsp", "--help" },
        \\Start the experimental Roc language server (LSP)
        \\
        \\Usage: roc experimental-lsp [OPTIONS]
        \\
        \\Options:
        \\      --stdio            Communicate over stdio (the default and only
        \\                         transport; accepted for compatibility with LSP
        \\                         clients that pass it explicitly)
        \\      --debug-transport  Mirror all JSON-RPC traffic to a temp log file
        \\      --debug-build      Log build environment actions to the debug log
        \\      --debug-syntax     Log syntax/type checking steps to the debug log
        \\      --debug-server     Log server lifecycle details to the debug log
        \\  -h, --help             Print help
        \\
    );
}

test "golden help: roc licenses --help" {
    try expectHelp(&.{ "licenses", "--help" },
        \\Prints license info for Roc as well as attributions to other projects used by Roc
        \\
        \\Usage: roc licenses
        \\
        \\Options:
        \\  -h, --help  Print help
        \\
    );
}
