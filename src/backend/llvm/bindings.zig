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

/// LLVM memory buffer for reading bitcode and other data from memory.
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

/// LLVM Module - contains functions, global variables, and symbol table.
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

/// Frees an error message string returned by LLVM.
pub const disposeMessage = LLVMDisposeMessage;
extern fn LLVMDisposeMessage(Message: [*:0]const u8) void;

/// LLVM target machine for code generation with specific CPU features and options.
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

/// LLVM target data layout describing sizes, alignments, and address spaces.
pub const TargetData = opaque {
    pub const dispose = LLVMDisposeTargetData;
    extern fn LLVMDisposeTargetData(*TargetData) void;

    pub const abiAlignmentOfType = LLVMABIAlignmentOfType;
    extern fn LLVMABIAlignmentOfType(TD: *TargetData, Ty: *Type) c_uint;

    pub const copyStringRepOfTargetData = LLVMCopyStringRepOfTargetData;
    extern fn LLVMCopyStringRepOfTargetData(*TargetData) [*:0]const u8;
};

/// Opaque LLVM type reference used by the C API.
pub const Type = opaque {};

/// Code model for code generation affecting addressing modes and code placement.
pub const CodeModel = enum(c_int) {
    Default,
    JITDefault,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
};

/// Optimization level for code generation.
pub const CodeGenOptLevel = enum(c_int) {
    None,
    Less,
    Default,
    Aggressive,
};

/// Relocation model controlling how symbols are addressed.
pub const RelocMode = enum(c_int) {
    Default,
    Static,
    PIC,
    DynamicNoPIC,
    ROPI,
    RWPI,
    ROPI_RWPI,
};

/// LLVM target representing a specific architecture and OS combination.
pub const Target = opaque {
    pub const getFromTriple = LLVMGetTargetFromTriple;
    extern fn LLVMGetTargetFromTriple(Triple: [*:0]const u8, T: **Target, ErrorMessage: *[*:0]const u8) Bool;
};

/// Zig's custom target initialization function that initializes all targets.
pub const initializeAllTargets = ZigLLVMInitializeAllTargets;
extern fn ZigLLVMInitializeAllTargets() void;

/// Initialize target info for AArch64 (ARM 64-bit) architecture.
pub extern fn LLVMInitializeAArch64TargetInfo() void;
/// Initialize target info for AMD GPU architecture.
pub extern fn LLVMInitializeAMDGPUTargetInfo() void;
/// Initialize target info for ARM (32-bit) architecture.
pub extern fn LLVMInitializeARMTargetInfo() void;
/// Initialize target info for AVR microcontroller architecture.
pub extern fn LLVMInitializeAVRTargetInfo() void;
/// Initialize target info for BPF (Berkeley Packet Filter) architecture.
pub extern fn LLVMInitializeBPFTargetInfo() void;
/// Initialize target info for Hexagon DSP architecture.
pub extern fn LLVMInitializeHexagonTargetInfo() void;
/// Initialize target info for Lanai architecture.
pub extern fn LLVMInitializeLanaiTargetInfo() void;
/// Initialize target info for MIPS architecture.
pub extern fn LLVMInitializeMipsTargetInfo() void;
/// Initialize target info for MSP430 microcontroller architecture.
pub extern fn LLVMInitializeMSP430TargetInfo() void;
/// Initialize target info for NVIDIA PTX architecture.
pub extern fn LLVMInitializeNVPTXTargetInfo() void;
/// Initialize target info for PowerPC architecture.
pub extern fn LLVMInitializePowerPCTargetInfo() void;
/// Initialize target info for RISC-V architecture.
pub extern fn LLVMInitializeRISCVTargetInfo() void;
/// Initialize target info for SPARC architecture.
pub extern fn LLVMInitializeSparcTargetInfo() void;
/// Initialize target info for SystemZ (IBM Z) architecture.
pub extern fn LLVMInitializeSystemZTargetInfo() void;
/// Initialize target info for WebAssembly architecture.
pub extern fn LLVMInitializeWebAssemblyTargetInfo() void;
/// Initialize target info for x86 architecture.
pub extern fn LLVMInitializeX86TargetInfo() void;
/// Initialize target info for XCore architecture.
pub extern fn LLVMInitializeXCoreTargetInfo() void;
/// Initialize target info for LoongArch architecture.
pub extern fn LLVMInitializeLoongArchTargetInfo() void;

/// Initialize code generation for AArch64 architecture.
pub extern fn LLVMInitializeAArch64Target() void;
/// Initialize code generation for AMD GPU architecture.
pub extern fn LLVMInitializeAMDGPUTarget() void;
/// Initialize code generation for ARM architecture.
pub extern fn LLVMInitializeARMTarget() void;
/// Initialize code generation for AVR architecture.
pub extern fn LLVMInitializeAVRTarget() void;
/// Initialize code generation for BPF architecture.
pub extern fn LLVMInitializeBPFTarget() void;
/// Initialize code generation for Hexagon architecture.
pub extern fn LLVMInitializeHexagonTarget() void;
/// Initialize code generation for Lanai architecture.
pub extern fn LLVMInitializeLanaiTarget() void;
/// Initialize code generation for MIPS architecture.
pub extern fn LLVMInitializeMipsTarget() void;
/// Initialize code generation for MSP430 architecture.
pub extern fn LLVMInitializeMSP430Target() void;
/// Initialize code generation for NVIDIA PTX architecture.
pub extern fn LLVMInitializeNVPTXTarget() void;
/// Initialize code generation for PowerPC architecture.
pub extern fn LLVMInitializePowerPCTarget() void;
/// Initialize code generation for RISC-V architecture.
pub extern fn LLVMInitializeRISCVTarget() void;
/// Initialize code generation for SPARC architecture.
pub extern fn LLVMInitializeSparcTarget() void;
/// Initialize code generation for SystemZ architecture.
pub extern fn LLVMInitializeSystemZTarget() void;
/// Initialize code generation for WebAssembly architecture.
pub extern fn LLVMInitializeWebAssemblyTarget() void;
/// Initialize code generation for x86 architecture.
pub extern fn LLVMInitializeX86Target() void;
/// Initialize code generation for XCore architecture.
pub extern fn LLVMInitializeXCoreTarget() void;
/// Initialize code generation for LoongArch architecture.
pub extern fn LLVMInitializeLoongArchTarget() void;

/// Initialize machine code components for AArch64 architecture.
pub extern fn LLVMInitializeAArch64TargetMC() void;
/// Initialize machine code components for AMD GPU architecture.
pub extern fn LLVMInitializeAMDGPUTargetMC() void;
/// Initialize machine code components for ARM architecture.
pub extern fn LLVMInitializeARMTargetMC() void;
/// Initialize machine code components for AVR architecture.
pub extern fn LLVMInitializeAVRTargetMC() void;
/// Initialize machine code components for BPF architecture.
pub extern fn LLVMInitializeBPFTargetMC() void;
/// Initialize machine code components for Hexagon architecture.
pub extern fn LLVMInitializeHexagonTargetMC() void;
/// Initialize machine code components for Lanai architecture.
pub extern fn LLVMInitializeLanaiTargetMC() void;
/// Initialize machine code components for MIPS architecture.
pub extern fn LLVMInitializeMipsTargetMC() void;
/// Initialize machine code components for MSP430 architecture.
pub extern fn LLVMInitializeMSP430TargetMC() void;
/// Initialize machine code components for NVIDIA PTX architecture.
pub extern fn LLVMInitializeNVPTXTargetMC() void;
/// Initialize machine code components for PowerPC architecture.
pub extern fn LLVMInitializePowerPCTargetMC() void;
/// Initialize machine code components for RISC-V architecture.
pub extern fn LLVMInitializeRISCVTargetMC() void;
/// Initialize machine code components for SPARC architecture.
pub extern fn LLVMInitializeSparcTargetMC() void;
/// Initialize machine code components for SystemZ architecture.
pub extern fn LLVMInitializeSystemZTargetMC() void;
/// Initialize machine code components for WebAssembly architecture.
pub extern fn LLVMInitializeWebAssemblyTargetMC() void;
/// Initialize machine code components for x86 architecture.
pub extern fn LLVMInitializeX86TargetMC() void;
/// Initialize machine code components for XCore architecture.
pub extern fn LLVMInitializeXCoreTargetMC() void;
/// Initialize machine code components for LoongArch architecture.
pub extern fn LLVMInitializeLoongArchTargetMC() void;

/// Initialize assembly printer for AArch64 architecture.
pub extern fn LLVMInitializeAArch64AsmPrinter() void;
/// Initialize assembly printer for AMD GPU architecture.
pub extern fn LLVMInitializeAMDGPUAsmPrinter() void;
/// Initialize assembly printer for ARM architecture.
pub extern fn LLVMInitializeARMAsmPrinter() void;
/// Initialize assembly printer for AVR architecture.
pub extern fn LLVMInitializeAVRAsmPrinter() void;
/// Initialize assembly printer for BPF architecture.
pub extern fn LLVMInitializeBPFAsmPrinter() void;
/// Initialize assembly printer for Hexagon architecture.
pub extern fn LLVMInitializeHexagonAsmPrinter() void;
/// Initialize assembly printer for Lanai architecture.
pub extern fn LLVMInitializeLanaiAsmPrinter() void;
/// Initialize assembly printer for MIPS architecture.
pub extern fn LLVMInitializeMipsAsmPrinter() void;
/// Initialize assembly printer for MSP430 architecture.
pub extern fn LLVMInitializeMSP430AsmPrinter() void;
/// Initialize assembly printer for NVIDIA PTX architecture.
pub extern fn LLVMInitializeNVPTXAsmPrinter() void;
/// Initialize assembly printer for PowerPC architecture.
pub extern fn LLVMInitializePowerPCAsmPrinter() void;
/// Initialize assembly printer for RISC-V architecture.
pub extern fn LLVMInitializeRISCVAsmPrinter() void;
/// Initialize assembly printer for SPARC architecture.
pub extern fn LLVMInitializeSparcAsmPrinter() void;
/// Initialize assembly printer for SystemZ architecture.
pub extern fn LLVMInitializeSystemZAsmPrinter() void;
/// Initialize assembly printer for WebAssembly architecture.
pub extern fn LLVMInitializeWebAssemblyAsmPrinter() void;
/// Initialize assembly printer for x86 architecture.
pub extern fn LLVMInitializeX86AsmPrinter() void;
/// Initialize assembly printer for XCore architecture.
pub extern fn LLVMInitializeXCoreAsmPrinter() void;
/// Initialize assembly printer for LoongArch architecture.
pub extern fn LLVMInitializeLoongArchAsmPrinter() void;

/// Initialize assembly parser for AArch64 architecture.
pub extern fn LLVMInitializeAArch64AsmParser() void;
/// Initialize assembly parser for AMD GPU architecture.
pub extern fn LLVMInitializeAMDGPUAsmParser() void;
/// Initialize assembly parser for ARM architecture.
pub extern fn LLVMInitializeARMAsmParser() void;
/// Initialize assembly parser for AVR architecture.
pub extern fn LLVMInitializeAVRAsmParser() void;
/// Initialize assembly parser for BPF architecture.
pub extern fn LLVMInitializeBPFAsmParser() void;
/// Initialize assembly parser for Hexagon architecture.
pub extern fn LLVMInitializeHexagonAsmParser() void;
/// Initialize assembly parser for Lanai architecture.
pub extern fn LLVMInitializeLanaiAsmParser() void;
/// Initialize assembly parser for MIPS architecture.
pub extern fn LLVMInitializeMipsAsmParser() void;
/// Initialize assembly parser for MSP430 architecture.
pub extern fn LLVMInitializeMSP430AsmParser() void;
/// Initialize assembly parser for PowerPC architecture.
pub extern fn LLVMInitializePowerPCAsmParser() void;
/// Initialize assembly parser for RISC-V architecture.
pub extern fn LLVMInitializeRISCVAsmParser() void;
/// Initialize assembly parser for SPARC architecture.
pub extern fn LLVMInitializeSparcAsmParser() void;
/// Initialize assembly parser for SystemZ architecture.
pub extern fn LLVMInitializeSystemZAsmParser() void;
/// Initialize assembly parser for WebAssembly architecture.
pub extern fn LLVMInitializeWebAssemblyAsmParser() void;
/// Initialize assembly parser for x86 architecture.
pub extern fn LLVMInitializeX86AsmParser() void;
/// Initialize assembly parser for LoongArch architecture.
pub extern fn LLVMInitializeLoongArchAsmParser() void;

// LLD linker functions
extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkELF(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkMachO(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkWasm(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool, disable_output: bool) bool;

/// Invoke the LLD linker for Windows COFF object files.
pub const LinkCOFF = ZigLLDLinkCOFF;
/// Invoke the LLD linker for Unix ELF object files.
pub const LinkELF = ZigLLDLinkELF;
/// Invoke the LLD linker for macOS Mach-O object files.
pub const LinkMachO = ZigLLDLinkMachO;
/// Invoke the LLD linker for WebAssembly modules.
pub const LinkWasm = ZigLLDLinkWasm;

/// Archive file format variants for creating static libraries.
pub const ArchiveKind = enum(c_int) {
    GNU,
    GNU64,
    BSD,
    DARWIN,
    DARWIN64,
    COFF,
    AIXBIG,
};

/// Create a static library archive from the given object files.
pub const WriteArchive = ZigLLVMWriteArchive;
extern fn ZigLLVMWriteArchive(
    archive_name: [*:0]const u8,
    file_names_ptr: [*]const [*:0]const u8,
    file_names_len: usize,
    archive_kind: ArchiveKind,
) bool;

/// Parse LLVM command line options for configuring optimization and codegen behavior.
pub const ParseCommandLineOptions = ZigLLVMParseCommandLineOptions;
extern fn ZigLLVMParseCommandLineOptions(argc: usize, argv: [*]const [*:0]const u8) void;

/// Get the name of the host CPU (e.g., "skylake", "apple-m1").
pub const GetHostCPUName = LLVMGetHostCPUName;
extern fn LLVMGetHostCPUName() ?[*:0]u8;

/// Get the feature string for the host CPU (e.g., "+avx2,+sse4.2").
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
