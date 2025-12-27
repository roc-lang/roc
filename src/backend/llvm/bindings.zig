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
//! Adapted from the Zig compiler's LLVM bindings.

const std = @import("std");

/// Do not compare directly to .True, use toBool() instead.
pub const Bool = enum(c_int) {
    False,
    True,
    _,

    pub fn fromBool(b: bool) Bool {
        return @enumFromInt(@intFromBool(b));
    }

    pub fn toBool(self: Bool) bool {
        return self != .False;
    }
};

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

    pub const setOptBisectLimit = ZigLLVMSetOptBisectLimit;
    extern fn ZigLLVMSetOptBisectLimit(C: *Context, limit: c_int) void;

    pub const enableBrokenDebugInfoCheck = ZigLLVMEnableBrokenDebugInfoCheck;
    extern fn ZigLLVMEnableBrokenDebugInfoCheck(C: *Context) void;

    pub const getBrokenDebugInfo = ZigLLVMGetBrokenDebugInfo;
    extern fn ZigLLVMGetBrokenDebugInfo(C: *Context) bool;

    pub const intType = LLVMIntTypeInContext;
    extern fn LLVMIntTypeInContext(C: *Context, NumBits: c_uint) *Type;
};

pub const Module = opaque {
    pub const dispose = LLVMDisposeModule;
    extern fn LLVMDisposeModule(*Module) void;

    pub const setTargetTriple = LLVMSetTarget;
    extern fn LLVMSetTarget(M: *Module, Triple: [*:0]const u8) void;

    pub const setDataLayout = LLVMSetDataLayout;
    extern fn LLVMSetDataLayout(M: *Module, DataLayoutStr: [*:0]const u8) void;

    pub const getDataLayout = LLVMGetDataLayoutStr;
    extern fn LLVMGetDataLayoutStr(M: *const Module) [*:0]const u8;
};

pub const disposeMessage = LLVMDisposeMessage;
extern fn LLVMDisposeMessage(Message: [*:0]const u8) void;

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
        is_small: bool,
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

    pub const createTargetDataLayout = LLVMCreateTargetDataLayout;
    extern fn LLVMCreateTargetDataLayout(*TargetMachine) *TargetData;
};

pub const TargetData = opaque {
    pub const dispose = LLVMDisposeTargetData;
    extern fn LLVMDisposeTargetData(*TargetData) void;

    pub const abiAlignmentOfType = LLVMABIAlignmentOfType;
    extern fn LLVMABIAlignmentOfType(TD: *TargetData, Ty: *Type) c_uint;

    pub const copyStringRepOfTargetData = LLVMCopyStringRepOfTargetData;
    extern fn LLVMCopyStringRepOfTargetData(*TargetData) [*:0]const u8;
};

pub const Type = opaque {};

pub const CodeModel = enum(c_int) {
    Default,
    JITDefault,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
};

pub const CodeGenOptLevel = enum(c_int) {
    None,
    Less,
    Default,
    Aggressive,
};

pub const RelocMode = enum(c_int) {
    Default,
    Static,
    PIC,
    DynamicNoPIC,
    ROPI,
    RWPI,
    ROPI_RWPI,
};

pub const Target = opaque {
    pub const getFromTriple = LLVMGetTargetFromTriple;
    extern fn LLVMGetTargetFromTriple(Triple: [*:0]const u8, T: **Target, ErrorMessage: *[*:0]const u8) Bool;
};

// Zig's custom target initialization function that initializes all targets
pub const initializeAllTargets = ZigLLVMInitializeAllTargets;
extern fn ZigLLVMInitializeAllTargets() void;

// Individual target initialization functions (standard LLVM C API)
pub extern fn LLVMInitializeAArch64TargetInfo() void;
pub extern fn LLVMInitializeAMDGPUTargetInfo() void;
pub extern fn LLVMInitializeARMTargetInfo() void;
pub extern fn LLVMInitializeAVRTargetInfo() void;
pub extern fn LLVMInitializeBPFTargetInfo() void;
pub extern fn LLVMInitializeHexagonTargetInfo() void;
pub extern fn LLVMInitializeLanaiTargetInfo() void;
pub extern fn LLVMInitializeMipsTargetInfo() void;
pub extern fn LLVMInitializeMSP430TargetInfo() void;
pub extern fn LLVMInitializeNVPTXTargetInfo() void;
pub extern fn LLVMInitializePowerPCTargetInfo() void;
pub extern fn LLVMInitializeRISCVTargetInfo() void;
pub extern fn LLVMInitializeSparcTargetInfo() void;
pub extern fn LLVMInitializeSystemZTargetInfo() void;
pub extern fn LLVMInitializeWebAssemblyTargetInfo() void;
pub extern fn LLVMInitializeX86TargetInfo() void;
pub extern fn LLVMInitializeXCoreTargetInfo() void;
pub extern fn LLVMInitializeLoongArchTargetInfo() void;

pub extern fn LLVMInitializeAArch64Target() void;
pub extern fn LLVMInitializeAMDGPUTarget() void;
pub extern fn LLVMInitializeARMTarget() void;
pub extern fn LLVMInitializeAVRTarget() void;
pub extern fn LLVMInitializeBPFTarget() void;
pub extern fn LLVMInitializeHexagonTarget() void;
pub extern fn LLVMInitializeLanaiTarget() void;
pub extern fn LLVMInitializeMipsTarget() void;
pub extern fn LLVMInitializeMSP430Target() void;
pub extern fn LLVMInitializeNVPTXTarget() void;
pub extern fn LLVMInitializePowerPCTarget() void;
pub extern fn LLVMInitializeRISCVTarget() void;
pub extern fn LLVMInitializeSparcTarget() void;
pub extern fn LLVMInitializeSystemZTarget() void;
pub extern fn LLVMInitializeWebAssemblyTarget() void;
pub extern fn LLVMInitializeX86Target() void;
pub extern fn LLVMInitializeXCoreTarget() void;
pub extern fn LLVMInitializeLoongArchTarget() void;

pub extern fn LLVMInitializeAArch64TargetMC() void;
pub extern fn LLVMInitializeAMDGPUTargetMC() void;
pub extern fn LLVMInitializeARMTargetMC() void;
pub extern fn LLVMInitializeAVRTargetMC() void;
pub extern fn LLVMInitializeBPFTargetMC() void;
pub extern fn LLVMInitializeHexagonTargetMC() void;
pub extern fn LLVMInitializeLanaiTargetMC() void;
pub extern fn LLVMInitializeMipsTargetMC() void;
pub extern fn LLVMInitializeMSP430TargetMC() void;
pub extern fn LLVMInitializeNVPTXTargetMC() void;
pub extern fn LLVMInitializePowerPCTargetMC() void;
pub extern fn LLVMInitializeRISCVTargetMC() void;
pub extern fn LLVMInitializeSparcTargetMC() void;
pub extern fn LLVMInitializeSystemZTargetMC() void;
pub extern fn LLVMInitializeWebAssemblyTargetMC() void;
pub extern fn LLVMInitializeX86TargetMC() void;
pub extern fn LLVMInitializeXCoreTargetMC() void;
pub extern fn LLVMInitializeLoongArchTargetMC() void;

pub extern fn LLVMInitializeAArch64AsmPrinter() void;
pub extern fn LLVMInitializeAMDGPUAsmPrinter() void;
pub extern fn LLVMInitializeARMAsmPrinter() void;
pub extern fn LLVMInitializeAVRAsmPrinter() void;
pub extern fn LLVMInitializeBPFAsmPrinter() void;
pub extern fn LLVMInitializeHexagonAsmPrinter() void;
pub extern fn LLVMInitializeLanaiAsmPrinter() void;
pub extern fn LLVMInitializeMipsAsmPrinter() void;
pub extern fn LLVMInitializeMSP430AsmPrinter() void;
pub extern fn LLVMInitializeNVPTXAsmPrinter() void;
pub extern fn LLVMInitializePowerPCAsmPrinter() void;
pub extern fn LLVMInitializeRISCVAsmPrinter() void;
pub extern fn LLVMInitializeSparcAsmPrinter() void;
pub extern fn LLVMInitializeSystemZAsmPrinter() void;
pub extern fn LLVMInitializeWebAssemblyAsmPrinter() void;
pub extern fn LLVMInitializeX86AsmPrinter() void;
pub extern fn LLVMInitializeXCoreAsmPrinter() void;
pub extern fn LLVMInitializeLoongArchAsmPrinter() void;

pub extern fn LLVMInitializeAArch64AsmParser() void;
pub extern fn LLVMInitializeAMDGPUAsmParser() void;
pub extern fn LLVMInitializeARMAsmParser() void;
pub extern fn LLVMInitializeAVRAsmParser() void;
pub extern fn LLVMInitializeBPFAsmParser() void;
pub extern fn LLVMInitializeHexagonAsmParser() void;
pub extern fn LLVMInitializeLanaiAsmParser() void;
pub extern fn LLVMInitializeMipsAsmParser() void;
pub extern fn LLVMInitializeMSP430AsmParser() void;
pub extern fn LLVMInitializePowerPCAsmParser() void;
pub extern fn LLVMInitializeRISCVAsmParser() void;
pub extern fn LLVMInitializeSparcAsmParser() void;
pub extern fn LLVMInitializeSystemZAsmParser() void;
pub extern fn LLVMInitializeWebAssemblyAsmParser() void;
pub extern fn LLVMInitializeX86AsmParser() void;
pub extern fn LLVMInitializeLoongArchAsmParser() void;

// LLD linker functions
extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkELF(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkMachO(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkWasm(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool, disable_output: bool) bool;

pub const LinkCOFF = ZigLLDLinkCOFF;
pub const LinkELF = ZigLLDLinkELF;
pub const LinkMachO = ZigLLDLinkMachO;
pub const LinkWasm = ZigLLDLinkWasm;

// Archive functions
pub const ArchiveKind = enum(c_int) {
    GNU,
    GNU64,
    BSD,
    DARWIN,
    DARWIN64,
    COFF,
    AIXBIG,
};

pub const WriteArchive = ZigLLVMWriteArchive;
extern fn ZigLLVMWriteArchive(
    archive_name: [*:0]const u8,
    file_names_ptr: [*]const [*:0]const u8,
    file_names_len: usize,
    archive_kind: ArchiveKind,
) bool;

pub const ParseCommandLineOptions = ZigLLVMParseCommandLineOptions;
extern fn ZigLLVMParseCommandLineOptions(argc: usize, argv: [*]const [*:0]const u8) void;

pub const GetHostCPUName = LLVMGetHostCPUName;
extern fn LLVMGetHostCPUName() ?[*:0]u8;

pub const GetHostCPUFeatures = LLVMGetHostCPUFeatures;
extern fn LLVMGetHostCPUFeatures() ?[*:0]u8;

/// High-level function to compile bitcode to an object file.
/// Returns an error message on failure, null on success.
pub fn compileBitcodeToObject(
    bitcode: []const u8,
    target_triple: [*:0]const u8,
    cpu: ?[*:0]const u8,
    features: ?[*:0]const u8,
    output_path: [*:0]const u8,
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
        if (is_debug) .None else .Default,
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
        .is_small = false,
        .time_report_out = null,
        .tsan = false,
        .sancov = false,
        .lto = .None,
        .allow_fast_isel = is_debug,
        .allow_machine_outliner = !is_debug,
        .asm_filename = null,
        .bin_filename = output_path,
        .llvm_ir_filename = null,
        .bitcode_filename = null,
        .coverage = default_coverage,
    };

    // Emit object file
    var emit_error: [*:0]const u8 = undefined;
    if (target_machine.emitToFile(module, &emit_error, &emit_options)) {
        return emit_error;
    }

    return null;
}
