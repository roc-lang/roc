//! LLVM C API bindings for compiling bitcode to object code.
//!
//! This module provides Zig bindings to LLVM's C API for:
//! 1. Parsing LLVM bitcode into a module
//! 2. Creating a target machine for code generation
//! 3. Emitting object files from LLVM modules
//!
//! Some functions use custom C++ wrappers (prefixed with ZigLLVM) because
//! the standard LLVM C API doesn't expose all necessary functionality.
//! These wrappers are implemented in src/build/zig_llvm.cpp.
//!
//! Adapted from the Zig compiler at https://codeberg.org/ziglang/zig and licensed under the MIT license. Thanks, Zig team!

/// LLVM boolean type compatible with C int. Do not compare directly to .True, use toBool() instead.
pub const Bool = enum(c_int) {
    False,
    True,
    _,

    /// Converts an LLVM Bool to a Zig bool.
    pub fn toBool(self: Bool) bool {
        return self != .False;
    }
};

/// Opaque handle to an LLVM memory buffer for holding bitcode or other data.
pub const MemoryBuffer = opaque {
    pub const createMemoryBufferWithMemoryRange = LLVMCreateMemoryBufferWithMemoryRange;
    pub const dispose = LLVMDisposeMemoryBuffer;

    extern fn LLVMCreateMemoryBufferWithMemoryRange(
        InputData: [*]const u8,
        InputDataLength: usize,
        BufferName: ?[*:0]const u8,
        RequiresNullTerminator: Bool,
    ) *MemoryBuffer;
    extern fn LLVMDisposeMemoryBuffer(MemBuf: *MemoryBuffer) void;
};

/// LLVM Context - owns and manages core LLVM data structures.
/// Make sure to use the *InContext functions instead of the global ones.
pub const Context = opaque {
    pub const create = LLVMContextCreate;
    extern fn LLVMContextCreate() *Context;

    pub const dispose = LLVMContextDispose;
    extern fn LLVMContextDispose(C: *Context) void;

    pub const parseBitcodeInContext2 = LLVMParseBitcodeInContext2;
    extern fn LLVMParseBitcodeInContext2(C: *Context, MemBuf: *MemoryBuffer, OutModule: **Module) Bool;
};

/// Opaque handle to an LLVM module containing functions, globals, and metadata.
pub const Module = opaque {
    pub const dispose = LLVMDisposeModule;
    extern fn LLVMDisposeModule(*Module) void;

    pub const setTargetTriple = LLVMSetTarget;
    extern fn LLVMSetTarget(M: *Module, Triple: [*:0]const u8) void;

    pub const getTargetTriple = LLVMGetTarget;
    extern fn LLVMGetTarget(M: *const Module) [*:0]const u8;

    pub const setDataLayout = LLVMSetDataLayout;
    extern fn LLVMSetDataLayout(M: *Module, DataLayoutStr: [*:0]const u8) void;

    pub const getDataLayout = LLVMGetDataLayoutStr;
    extern fn LLVMGetDataLayoutStr(M: *const Module) [*:0]const u8;

    /// Links the source module into this module.
    /// The source module is destroyed after a successful link.
    /// Returns true on error, false on success.
    pub const link = LLVMLinkModules2;
    extern fn LLVMLinkModules2(Dest: *Module, Src: *Module) Bool;

    pub const verify = LLVMVerifyModule;
    extern fn LLVMVerifyModule(M: *Module, Action: VerifierFailureAction, OutMessage: *[*:0]const u8) Bool;

    pub const getFirstFunction = LLVMGetFirstFunction;
    extern fn LLVMGetFirstFunction(M: *Module) ?*Value;

    pub const getFirstGlobal = LLVMGetFirstGlobal;
    extern fn LLVMGetFirstGlobal(M: *Module) ?*Value;

    pub const getFirstGlobalAlias = LLVMGetFirstGlobalAlias;
    extern fn LLVMGetFirstGlobalAlias(M: *Module) ?*Value;

    pub const getNamedGlobal = LLVMGetNamedGlobal;
    extern fn LLVMGetNamedGlobal(M: *Module, Name: [*:0]const u8) ?*Value;
};

/// Opaque handle to an LLVM value.
pub const Value = opaque {
    pub const getNextFunction = LLVMGetNextFunction;
    extern fn LLVMGetNextFunction(FnVal: *Value) ?*Value;

    pub const getNextGlobal = LLVMGetNextGlobal;
    extern fn LLVMGetNextGlobal(Global: *Value) ?*Value;

    pub const getNextGlobalAlias = LLVMGetNextGlobalAlias;
    extern fn LLVMGetNextGlobalAlias(Alias: *Value) ?*Value;

    pub const getName = LLVMGetValueName2;
    extern fn LLVMGetValueName2(Val: *Value, Len: *usize) [*]const u8;

    pub const isDeclaration = LLVMIsDeclaration;
    extern fn LLVMIsDeclaration(Global: *Value) Bool;

    pub const setLinkage = LLVMSetLinkage;
    extern fn LLVMSetLinkage(Global: *Value, Linkage: c_int) void;

    pub const getLinkage = LLVMGetLinkage;
    extern fn LLVMGetLinkage(Global: *Value) c_int;

    pub const setVisibility = LLVMSetVisibility;
    extern fn LLVMSetVisibility(Global: *Value, Viz: c_int) void;

    pub const aliasGetAliasee = LLVMAliasGetAliasee;
    extern fn LLVMAliasGetAliasee(Alias: *Value) ?*Value;

    pub const replaceAllUsesWith = LLVMReplaceAllUsesWith;
    extern fn LLVMReplaceAllUsesWith(OldVal: *Value, NewVal: *Value) void;

    pub const removeStringAttributeAtIndex = LLVMRemoveStringAttributeAtIndex;
    extern fn LLVMRemoveStringAttributeAtIndex(FnVal: *Value, Idx: c_uint, Name: [*]const u8, Len: c_uint) void;

    pub const deleteGlobal = LLVMDeleteGlobal;
    extern fn LLVMDeleteGlobal(GlobalVar: *Value) void;
};

/// Runs LLVM global dead-code elimination on a module whose unused definitions
/// have already been made internal.
pub const runGlobalDCE = ZigLLVMRunGlobalDCE;
extern fn ZigLLVMRunGlobalDCE(module: *Module) void;

/// LLVM-C linkage value for `internal`: a local definition, never an exported
/// symbol, and discarded by global DCE when unused.
pub const internal_linkage: c_int = 8;

/// LLVM-C linkage value for `private`: a local definition whose name is not
/// visible to the linker.
pub const private_linkage: c_int = 9;

/// LLVM-C attribute index for function-level attributes (`~0U`).
pub const attribute_function_index: c_uint = 0xFFFFFFFF;

/// Controls how LLVM reports module verifier failures.
pub const VerifierFailureAction = enum(c_int) {
    AbortProcess,
    PrintMessage,
    ReturnStatus,
};

/// Frees a message string allocated by LLVM.
pub const disposeMessage = LLVMDisposeMessage;
extern fn LLVMDisposeMessage(Message: [*:0]const u8) void;

/// Opaque handle to an LLVM target machine for code generation to a specific architecture.
pub const TargetMachine = opaque {
    pub const FloatABI = enum(c_int) {
        /// Target-specific (either soft or hard depending on triple, etc).
        Default,
        /// Soft float.
        Soft,
        // Hard float.
        Hard,
    };

    pub const create = ZigLLVMCreateTargetMachine;
    extern fn ZigLLVMCreateTargetMachine(
        T: *Target,
        Triple: [*:0]const u8,
        CPU: ?[*:0]const u8,
        Features: ?[*:0]const u8,
        Level: CodeGenOptLevel,
        Reloc: RelocMode,
        CodeModel: CodeModel,
        function_sections: bool,
        data_sections: bool,
        float_abi: FloatABI,
        abi_name: ?[*:0]const u8,
        emulated_tls: bool,
    ) *TargetMachine;

    pub const dispose = LLVMDisposeTargetMachine;
    extern fn LLVMDisposeTargetMachine(T: *TargetMachine) void;

    pub const EmitOptions = extern struct {
        is_debug: bool,
        ir_opt_level: IrOptimizationLevel,
        time_report_out: ?*[*:0]u8,
        tsan: bool,
        sancov: bool,
        lto: LtoPhase,
        allow_fast_isel: bool,
        allow_machine_outliner: bool,
        asm_filename: ?[*:0]const u8,
        bin_filename: ?[*:0]const u8,
        llvm_ir_filename: ?[*:0]const u8,
        bitcode_filename: ?[*:0]const u8,
        coverage: Coverage,
        no_target_libcalls: bool,
        lower_memory_intrinsics_to_loops: bool,

        pub const LtoPhase = enum(c_int) {
            None,
            ThinPreLink,
            ThinPostLink,
            FullPreLink,
            FullPostLink,
        };

        pub const Coverage = extern struct {
            CoverageType: Coverage.Type,
            IndirectCalls: bool,
            TraceBB: bool,
            TraceCmp: bool,
            TraceDiv: bool,
            TraceGep: bool,
            Use8bitCounters: bool,
            TracePC: bool,
            TracePCGuard: bool,
            Inline8bitCounters: bool,
            InlineBoolFlag: bool,
            PCTable: bool,
            NoPrune: bool,
            StackDepth: bool,
            TraceLoads: bool,
            TraceStores: bool,
            CollectControlFlow: bool,

            pub const Type = enum(c_int) {
                None = 0,
                Function,
                BB,
                Edge,
            };
        };
    };

    pub const emitToFile = ZigLLVMTargetMachineEmitToFile;
    extern fn ZigLLVMTargetMachineEmitToFile(
        T: *TargetMachine,
        M: *Module,
        ErrorMessage: *[*:0]const u8,
        options: *const EmitOptions,
    ) bool;

    /// Like `emitToFile`, but writes the object into a buffer
    /// (`bin_filename` is ignored). On success the caller owns
    /// `out_bytes[0..out_len]` and releases it with `freeEmittedObject`.
    pub const emitObjectToMemory = ZigLLVMTargetMachineEmitObjectToMemory;
    extern fn ZigLLVMTargetMachineEmitObjectToMemory(
        T: *TargetMachine,
        M: *Module,
        ErrorMessage: *[*:0]const u8,
        options: *const EmitOptions,
        out_bytes: *[*]u8,
        out_len: *usize,
    ) bool;

    pub const freeEmittedObject = ZigLLVMFreeEmittedObject;
    extern fn ZigLLVMFreeEmittedObject(bytes: [*]u8) void;
};

/// LLVM code model controlling address space assumptions for code generation.
pub const CodeModel = enum(c_int) {
    Default,
    JITDefault,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
};

/// LLVM optimization level for code generation passes.
pub const CodeGenOptLevel = enum(c_int) {
    None,
    Less,
    Default,
    Aggressive,
};

/// IR optimization level for LLVM passes before code generation.
pub const IrOptimizationLevel = enum(c_int) {
    Oz = 0,
    O3 = 1,
    O0 = 2,

    pub fn toCodeGenOptLevel(self: IrOptimizationLevel) CodeGenOptLevel {
        return switch (self) {
            .Oz => .Default,
            .O3 => .Aggressive,
            .O0 => .None,
        };
    }
};

/// LLVM relocation model for position-independent code generation.
pub const RelocMode = enum(c_int) {
    Default,
    Static,
    PIC,
    DynamicNoPIC,
    ROPI,
    RWPI,
    ROPI_RWPI,
};

/// Opaque handle to an LLVM target representing a specific architecture.
pub const Target = opaque {
    pub const getFromTriple = LLVMGetTargetFromTriple;
    extern fn LLVMGetTargetFromTriple(Triple: [*:0]const u8, T: **Target, ErrorMessage: *[*:0]const u8) Bool;
};

/// Initializes all LLVM targets (architectures) for code generation.
pub const initializeAllTargets = ZigLLVMInitializeAllTargets;
extern fn ZigLLVMInitializeAllTargets() void;

// Individual target initialization functions (standard LLVM C API)
// These register target info, codegen, machine code, asm printer, and asm parser
// components for each supported architecture.

/// Returns the default target triple for the host (e.g., "x86_64-unknown-linux-gnu").
/// The returned string must be freed with disposeMessage.
pub const GetDefaultTargetTriple = LLVMGetDefaultTargetTriple;
extern fn LLVMGetDefaultTargetTriple() [*:0]u8;

/// High-level function to compile bitcode to an object file.
/// Returns an error message on failure, null on success.
pub fn compileBitcodeToObject(
    bitcode: []const u8,
    target_triple: [*:0]const u8,
    cpu: ?[*:0]const u8,
    features: ?[*:0]const u8,
    output_path: [*:0]const u8,
    ir_opt_level: IrOptimizationLevel,
    is_debug: bool,
) ?[*:0]const u8 {
    // Initialize all targets
    initializeAllTargets();

    // Create memory buffer from bitcode
    const mem_buf = MemoryBuffer.createMemoryBufferWithMemoryRange(
        bitcode.ptr,
        bitcode.len,
        "roc_bitcode",
        Bool.False,
    );
    defer mem_buf.dispose();

    // Create LLVM context
    const context = Context.create();
    defer context.dispose();

    // Parse bitcode into module
    var module: *Module = undefined;
    if (context.parseBitcodeInContext2(mem_buf, &module).toBool()) {
        return "Failed to parse bitcode";
    }
    defer module.dispose();

    // Get target from triple
    var target: *Target = undefined;
    var error_message: [*:0]const u8 = undefined;
    if (Target.getFromTriple(target_triple, &target, &error_message).toBool()) {
        return error_message;
    }

    // Create target machine
    const target_machine = TargetMachine.create(
        target,
        target_triple,
        cpu,
        features,
        ir_opt_level.toCodeGenOptLevel(),
        .Default,
        .Default,
        true, // function_sections
        true, // data_sections
        .Default, // float_abi
        null, // abi_name
        false, // emulated_tls
    );
    defer target_machine.dispose();

    // Set up emit options with default coverage
    const default_coverage = TargetMachine.EmitOptions.Coverage{
        .CoverageType = .None,
        .IndirectCalls = false,
        .TraceBB = false,
        .TraceCmp = false,
        .TraceDiv = false,
        .TraceGep = false,
        .Use8bitCounters = false,
        .TracePC = false,
        .TracePCGuard = false,
        .Inline8bitCounters = false,
        .InlineBoolFlag = false,
        .PCTable = false,
        .NoPrune = false,
        .StackDepth = false,
        .TraceLoads = false,
        .TraceStores = false,
        .CollectControlFlow = false,
    };

    const emit_options = TargetMachine.EmitOptions{
        .is_debug = is_debug,
        .ir_opt_level = ir_opt_level,
        .time_report_out = null,
        .tsan = false,
        .sancov = false,
        .lto = .None,
        .allow_fast_isel = false,
        .allow_machine_outliner = true,
        .asm_filename = null,
        .bin_filename = output_path,
        .llvm_ir_filename = null,
        .bitcode_filename = null,
        .coverage = default_coverage,
        .no_target_libcalls = false,
        .lower_memory_intrinsics_to_loops = false,
    };

    // Emit object file
    var emit_error: [*:0]const u8 = undefined;
    if (target_machine.emitToFile(module, &emit_error, &emit_options)) {
        return emit_error;
    }

    return null;
}
