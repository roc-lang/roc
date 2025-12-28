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

/// LLVM boolean type compatible with C int. Do not compare directly to .True, use toBool() instead.
pub const Bool = enum(c_int) {
    False,
    True,
    _,

    /// Converts a Zig bool to LLVM Bool.
    pub fn fromBool(b: bool) Bool {
        return @enumFromInt(@intFromBool(b));
    }

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

    pub const setOptBisectLimit = ZigLLVMSetOptBisectLimit;
    extern fn ZigLLVMSetOptBisectLimit(C: *Context, limit: c_int) void;

    pub const enableBrokenDebugInfoCheck = ZigLLVMEnableBrokenDebugInfoCheck;
    extern fn ZigLLVMEnableBrokenDebugInfoCheck(C: *Context) void;

    pub const getBrokenDebugInfo = ZigLLVMGetBrokenDebugInfo;
    extern fn ZigLLVMGetBrokenDebugInfo(C: *Context) bool;

    pub const intType = LLVMIntTypeInContext;
    extern fn LLVMIntTypeInContext(C: *Context, NumBits: c_uint) *Type;
};

/// Opaque handle to an LLVM module containing functions, globals, and metadata.
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

/// Opaque handle to LLVM target data layout information.
pub const TargetData = opaque {
    pub const dispose = LLVMDisposeTargetData;
    extern fn LLVMDisposeTargetData(*TargetData) void;

    pub const abiAlignmentOfType = LLVMABIAlignmentOfType;
    extern fn LLVMABIAlignmentOfType(TD: *TargetData, Ty: *Type) c_uint;

    pub const copyStringRepOfTargetData = LLVMCopyStringRepOfTargetData;
    extern fn LLVMCopyStringRepOfTargetData(*TargetData) [*:0]const u8;
};

/// Opaque handle to an LLVM type (integer, float, struct, etc.).
pub const Type = opaque {};

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

/// Initializes AArch64 (ARM64) target information.
pub extern fn LLVMInitializeAArch64TargetInfo() void;
/// Initializes AMD GPU target information.
pub extern fn LLVMInitializeAMDGPUTargetInfo() void;
/// Initializes ARM (32-bit) target information.
pub extern fn LLVMInitializeARMTargetInfo() void;
/// Initializes AVR microcontroller target information.
pub extern fn LLVMInitializeAVRTargetInfo() void;
/// Initializes BPF (Berkeley Packet Filter) target information.
pub extern fn LLVMInitializeBPFTargetInfo() void;
/// Initializes Hexagon DSP target information.
pub extern fn LLVMInitializeHexagonTargetInfo() void;
/// Initializes Lanai target information.
pub extern fn LLVMInitializeLanaiTargetInfo() void;
/// Initializes MIPS target information.
pub extern fn LLVMInitializeMipsTargetInfo() void;
/// Initializes MSP430 microcontroller target information.
pub extern fn LLVMInitializeMSP430TargetInfo() void;
/// Initializes NVIDIA PTX target information.
pub extern fn LLVMInitializeNVPTXTargetInfo() void;
/// Initializes PowerPC target information.
pub extern fn LLVMInitializePowerPCTargetInfo() void;
/// Initializes RISC-V target information.
pub extern fn LLVMInitializeRISCVTargetInfo() void;
/// Initializes SPARC target information.
pub extern fn LLVMInitializeSparcTargetInfo() void;
/// Initializes SystemZ (s390x) target information.
pub extern fn LLVMInitializeSystemZTargetInfo() void;
/// Initializes WebAssembly target information.
pub extern fn LLVMInitializeWebAssemblyTargetInfo() void;
/// Initializes x86/x86-64 target information.
pub extern fn LLVMInitializeX86TargetInfo() void;
/// Initializes XCore target information.
pub extern fn LLVMInitializeXCoreTargetInfo() void;
/// Initializes LoongArch target information.
pub extern fn LLVMInitializeLoongArchTargetInfo() void;

/// Initializes AArch64 code generation.
pub extern fn LLVMInitializeAArch64Target() void;
/// Initializes AMD GPU code generation.
pub extern fn LLVMInitializeAMDGPUTarget() void;
/// Initializes ARM code generation.
pub extern fn LLVMInitializeARMTarget() void;
/// Initializes AVR code generation.
pub extern fn LLVMInitializeAVRTarget() void;
/// Initializes BPF code generation.
pub extern fn LLVMInitializeBPFTarget() void;
/// Initializes Hexagon code generation.
pub extern fn LLVMInitializeHexagonTarget() void;
/// Initializes Lanai code generation.
pub extern fn LLVMInitializeLanaiTarget() void;
/// Initializes MIPS code generation.
pub extern fn LLVMInitializeMipsTarget() void;
/// Initializes MSP430 code generation.
pub extern fn LLVMInitializeMSP430Target() void;
/// Initializes NVIDIA PTX code generation.
pub extern fn LLVMInitializeNVPTXTarget() void;
/// Initializes PowerPC code generation.
pub extern fn LLVMInitializePowerPCTarget() void;
/// Initializes RISC-V code generation.
pub extern fn LLVMInitializeRISCVTarget() void;
/// Initializes SPARC code generation.
pub extern fn LLVMInitializeSparcTarget() void;
/// Initializes SystemZ code generation.
pub extern fn LLVMInitializeSystemZTarget() void;
/// Initializes WebAssembly code generation.
pub extern fn LLVMInitializeWebAssemblyTarget() void;
/// Initializes x86/x86-64 code generation.
pub extern fn LLVMInitializeX86Target() void;
/// Initializes XCore code generation.
pub extern fn LLVMInitializeXCoreTarget() void;
/// Initializes LoongArch code generation.
pub extern fn LLVMInitializeLoongArchTarget() void;

/// Initializes AArch64 machine code components.
pub extern fn LLVMInitializeAArch64TargetMC() void;
/// Initializes AMD GPU machine code components.
pub extern fn LLVMInitializeAMDGPUTargetMC() void;
/// Initializes ARM machine code components.
pub extern fn LLVMInitializeARMTargetMC() void;
/// Initializes AVR machine code components.
pub extern fn LLVMInitializeAVRTargetMC() void;
/// Initializes BPF machine code components.
pub extern fn LLVMInitializeBPFTargetMC() void;
/// Initializes Hexagon machine code components.
pub extern fn LLVMInitializeHexagonTargetMC() void;
/// Initializes Lanai machine code components.
pub extern fn LLVMInitializeLanaiTargetMC() void;
/// Initializes MIPS machine code components.
pub extern fn LLVMInitializeMipsTargetMC() void;
/// Initializes MSP430 machine code components.
pub extern fn LLVMInitializeMSP430TargetMC() void;
/// Initializes NVIDIA PTX machine code components.
pub extern fn LLVMInitializeNVPTXTargetMC() void;
/// Initializes PowerPC machine code components.
pub extern fn LLVMInitializePowerPCTargetMC() void;
/// Initializes RISC-V machine code components.
pub extern fn LLVMInitializeRISCVTargetMC() void;
/// Initializes SPARC machine code components.
pub extern fn LLVMInitializeSparcTargetMC() void;
/// Initializes SystemZ machine code components.
pub extern fn LLVMInitializeSystemZTargetMC() void;
/// Initializes WebAssembly machine code components.
pub extern fn LLVMInitializeWebAssemblyTargetMC() void;
/// Initializes x86/x86-64 machine code components.
pub extern fn LLVMInitializeX86TargetMC() void;
/// Initializes XCore machine code components.
pub extern fn LLVMInitializeXCoreTargetMC() void;
/// Initializes LoongArch machine code components.
pub extern fn LLVMInitializeLoongArchTargetMC() void;

/// Initializes AArch64 assembly printer.
pub extern fn LLVMInitializeAArch64AsmPrinter() void;
/// Initializes AMD GPU assembly printer.
pub extern fn LLVMInitializeAMDGPUAsmPrinter() void;
/// Initializes ARM assembly printer.
pub extern fn LLVMInitializeARMAsmPrinter() void;
/// Initializes AVR assembly printer.
pub extern fn LLVMInitializeAVRAsmPrinter() void;
/// Initializes BPF assembly printer.
pub extern fn LLVMInitializeBPFAsmPrinter() void;
/// Initializes Hexagon assembly printer.
pub extern fn LLVMInitializeHexagonAsmPrinter() void;
/// Initializes Lanai assembly printer.
pub extern fn LLVMInitializeLanaiAsmPrinter() void;
/// Initializes MIPS assembly printer.
pub extern fn LLVMInitializeMipsAsmPrinter() void;
/// Initializes MSP430 assembly printer.
pub extern fn LLVMInitializeMSP430AsmPrinter() void;
/// Initializes NVIDIA PTX assembly printer.
pub extern fn LLVMInitializeNVPTXAsmPrinter() void;
/// Initializes PowerPC assembly printer.
pub extern fn LLVMInitializePowerPCAsmPrinter() void;
/// Initializes RISC-V assembly printer.
pub extern fn LLVMInitializeRISCVAsmPrinter() void;
/// Initializes SPARC assembly printer.
pub extern fn LLVMInitializeSparcAsmPrinter() void;
/// Initializes SystemZ assembly printer.
pub extern fn LLVMInitializeSystemZAsmPrinter() void;
/// Initializes WebAssembly assembly printer.
pub extern fn LLVMInitializeWebAssemblyAsmPrinter() void;
/// Initializes x86/x86-64 assembly printer.
pub extern fn LLVMInitializeX86AsmPrinter() void;
/// Initializes XCore assembly printer.
pub extern fn LLVMInitializeXCoreAsmPrinter() void;
/// Initializes LoongArch assembly printer.
pub extern fn LLVMInitializeLoongArchAsmPrinter() void;

/// Initializes AArch64 assembly parser.
pub extern fn LLVMInitializeAArch64AsmParser() void;
/// Initializes AMD GPU assembly parser.
pub extern fn LLVMInitializeAMDGPUAsmParser() void;
/// Initializes ARM assembly parser.
pub extern fn LLVMInitializeARMAsmParser() void;
/// Initializes AVR assembly parser.
pub extern fn LLVMInitializeAVRAsmParser() void;
/// Initializes BPF assembly parser.
pub extern fn LLVMInitializeBPFAsmParser() void;
/// Initializes Hexagon assembly parser.
pub extern fn LLVMInitializeHexagonAsmParser() void;
/// Initializes Lanai assembly parser.
pub extern fn LLVMInitializeLanaiAsmParser() void;
/// Initializes MIPS assembly parser.
pub extern fn LLVMInitializeMipsAsmParser() void;
/// Initializes MSP430 assembly parser.
pub extern fn LLVMInitializeMSP430AsmParser() void;
/// Initializes PowerPC assembly parser.
pub extern fn LLVMInitializePowerPCAsmParser() void;
/// Initializes RISC-V assembly parser.
pub extern fn LLVMInitializeRISCVAsmParser() void;
/// Initializes SPARC assembly parser.
pub extern fn LLVMInitializeSparcAsmParser() void;
/// Initializes SystemZ assembly parser.
pub extern fn LLVMInitializeSystemZAsmParser() void;
/// Initializes WebAssembly assembly parser.
pub extern fn LLVMInitializeWebAssemblyAsmParser() void;
/// Initializes x86/x86-64 assembly parser.
pub extern fn LLVMInitializeX86AsmParser() void;
/// Initializes LoongArch assembly parser.
pub extern fn LLVMInitializeLoongArchAsmParser() void;

// LLD linker functions
extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkELF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkMachO(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkWasm(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;

/// Links object files using LLD for Windows COFF format.
pub const LinkCOFF = ZigLLDLinkCOFF;
/// Links object files using LLD for Unix ELF format.
pub const LinkELF = ZigLLDLinkELF;
/// Links object files using LLD for macOS Mach-O format.
pub const LinkMachO = ZigLLDLinkMachO;
/// Links object files using LLD for WebAssembly format.
pub const LinkWasm = ZigLLDLinkWasm;

// ORC JIT C API bindings

/// Represents an address in the executor process (64-bit).
pub const OrcExecutorAddress = u64;

/// Error reference type for ORC JIT operations.
pub const OrcError = ?*anyopaque;

/// Opaque handle to an ORC LLJIT instance for just-in-time compilation.
pub const OrcLLJIT = opaque {
    pub const dispose = LLVMOrcDisposeLLJIT;
    extern fn LLVMOrcDisposeLLJIT(*OrcLLJIT) OrcError;

    pub const getMainJITDylib = LLVMOrcLLJITGetMainJITDylib;
    extern fn LLVMOrcLLJITGetMainJITDylib(*OrcLLJIT) *OrcJITDylib;

    pub const addLLVMIRModule = LLVMOrcLLJITAddLLVMIRModule;
    extern fn LLVMOrcLLJITAddLLVMIRModule(*OrcLLJIT, *OrcJITDylib, *OrcThreadSafeModule) OrcError;

    pub const lookup = LLVMOrcLLJITLookup;
    extern fn LLVMOrcLLJITLookup(*OrcLLJIT, *OrcExecutorAddress, [*:0]const u8) OrcError;

    pub const getGlobalPrefix = LLVMOrcLLJITGetGlobalPrefix;
    extern fn LLVMOrcLLJITGetGlobalPrefix(*OrcLLJIT) u8;

    pub const getTripleString = LLVMOrcLLJITGetTripleString;
    extern fn LLVMOrcLLJITGetTripleString(*OrcLLJIT) [*:0]const u8;
};

/// Opaque handle to an ORC LLJIT builder for configuring JIT compilation.
pub const OrcLLJITBuilder = opaque {
    pub const create = LLVMOrcCreateLLJITBuilder;
    extern fn LLVMOrcCreateLLJITBuilder() *OrcLLJITBuilder;

    pub const dispose = LLVMOrcDisposeLLJITBuilder;
    extern fn LLVMOrcDisposeLLJITBuilder(*OrcLLJITBuilder) void;
};

/// Opaque handle to an ORC JIT dynamic library for symbol resolution.
pub const OrcJITDylib = opaque {};

/// Opaque handle to a thread-safe LLVM context for concurrent JIT compilation.
pub const OrcThreadSafeContext = opaque {
    pub const create = LLVMOrcCreateNewThreadSafeContext;
    extern fn LLVMOrcCreateNewThreadSafeContext() *OrcThreadSafeContext;

    pub const dispose = LLVMOrcDisposeThreadSafeContext;
    extern fn LLVMOrcDisposeThreadSafeContext(*OrcThreadSafeContext) void;

    pub const getContext = LLVMOrcThreadSafeContextGetContext;
    extern fn LLVMOrcThreadSafeContextGetContext(*OrcThreadSafeContext) *Context;
};

/// Opaque handle to a thread-safe LLVM module for JIT compilation.
pub const OrcThreadSafeModule = opaque {
    pub const create = LLVMOrcCreateNewThreadSafeModule;
    extern fn LLVMOrcCreateNewThreadSafeModule(*Module, *OrcThreadSafeContext) *OrcThreadSafeModule;

    pub const dispose = LLVMOrcDisposeThreadSafeModule;
    extern fn LLVMOrcDisposeThreadSafeModule(*OrcThreadSafeModule) void;
};

/// Creates an LLJIT instance from a builder configuration.
pub const createLLJIT = LLVMOrcCreateLLJIT;
extern fn LLVMOrcCreateLLJIT(**OrcLLJIT, *OrcLLJITBuilder) OrcError;

/// Opaque handle to a definition generator for dynamic symbol lookup.
pub const OrcDefinitionGenerator = opaque {
    pub const dispose = LLVMOrcDisposeDefinitionGenerator;
    extern fn LLVMOrcDisposeDefinitionGenerator(*OrcDefinitionGenerator) void;
};

/// Creates a dynamic library search generator to resolve symbols from the current process.
pub const createDynamicLibrarySearchGeneratorForProcess = LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess;
extern fn LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess(
    result: **OrcDefinitionGenerator,
    global_prefix: u8,
    filter: ?*const fn (*anyopaque, *anyopaque) callconv(.c) c_int,
    filter_ctx: ?*anyopaque,
) OrcError;

/// Adds a definition generator to a JITDylib for symbol resolution.
pub const jitDylibAddGenerator = LLVMOrcJITDylibAddGenerator;
extern fn LLVMOrcJITDylibAddGenerator(*OrcJITDylib, *OrcDefinitionGenerator) void;

/// Gets the error message string from an ORC error.
pub const getErrorMessage = LLVMGetErrorMessage;
extern fn LLVMGetErrorMessage(OrcError) [*:0]const u8;

/// Frees an error message string returned by getErrorMessage.
pub const disposeErrorMessage = LLVMDisposeErrorMessage;
extern fn LLVMDisposeErrorMessage([*:0]const u8) void;

/// Consumes an ORC error, releasing its resources.
pub const consumeError = LLVMConsumeError;
extern fn LLVMConsumeError(OrcError) void;

/// Archive file format types supported by LLVM.
pub const ArchiveKind = enum(c_int) {
    GNU,
    GNU64,
    BSD,
    DARWIN,
    DARWIN64,
    COFF,
    AIXBIG,
};

/// Creates a static archive file from a list of object files.
pub const WriteArchive = ZigLLVMWriteArchive;
extern fn ZigLLVMWriteArchive(
    archive_name: [*:0]const u8,
    file_names_ptr: [*]const [*:0]const u8,
    file_names_len: usize,
    archive_kind: ArchiveKind,
) bool;

/// Parses LLVM command-line options for debugging and configuration.
pub const ParseCommandLineOptions = ZigLLVMParseCommandLineOptions;
extern fn ZigLLVMParseCommandLineOptions(argc: usize, argv: [*]const [*:0]const u8) void;

/// Returns the name of the host CPU (e.g., "skylake", "apple-m1").
pub const GetHostCPUName = LLVMGetHostCPUName;
extern fn LLVMGetHostCPUName() ?[*:0]u8;

/// Returns the feature string for the host CPU (e.g., "+sse4.2,+avx").
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
