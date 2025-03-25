use inkwell::{
    targets::{
        CodeModel, InitializationConfig, RelocMode, Target as LlvmTarget, TargetMachine,
        TargetTriple,
    },
    OptimizationLevel,
};
use roc_error_macros::internal_error;
use roc_mono::ir::OptLevel;
use roc_target::{Architecture, Target};

pub fn target_triple_str(target: Target) -> &'static str {
    // Best guide I've found on how to determine these magic strings:
    //
    // https://stackoverflow.com/questions/15036909/clang-how-to-list-supported-target-architectures
    match target {
        Target::LinuxArm64 => "aarch64-unknown-linux-gnu",
        Target::LinuxX32 => "i386-unknown-linux-gnu",
        Target::LinuxX64 => "x86_64-unknown-linux-gnu",
        Target::MacArm64 => "aarch64-apple-darwin",
        Target::MacX64 => "x86_64-unknown-darwin10",
        Target::Wasm32 => "wasm32-unknown-unknown",
        Target::WinX64 => "x86_64-pc-windows-gnu",
        _ => internal_error!("TODO gracefully handle unsupported target: {:?}", target),
    }
}

pub fn init_arch(target: Target) {
    match target.architecture() {
        Architecture::X86_64 | Architecture::X86_32
            if cfg!(any(feature = "target-x86", feature = "target-x86_64")) =>
        {
            LlvmTarget::initialize_x86(&InitializationConfig::default());
        }
        Architecture::Aarch64 if cfg!(feature = "target-aarch64") => {
            LlvmTarget::initialize_aarch64(&InitializationConfig::default());
        }
        Architecture::Aarch32 if cfg!(feature = "target-arm") => {
            LlvmTarget::initialize_arm(&InitializationConfig::default());
        }
        Architecture::Wasm32 if cfg!(feature = "target-wasm32") => {
            LlvmTarget::initialize_webassembly(&InitializationConfig::default());
        }
        _ => internal_error!(
            "TODO gracefully handle unsupported target architecture: {:?}",
            target.architecture()
        ),
    }
}

/// NOTE: arch_str is *not* the same as the beginning of the magic target triple
/// string! For example, if it's "x86-64" here, the magic target triple string
/// will begin with "x86_64" (with an underscore) instead.
pub fn arch_str(target: Target) -> &'static str {
    // Best guide I've found on how to determine these magic strings:
    //
    // https://stackoverflow.com/questions/15036909/clang-how-to-list-supported-target-architectures
    match target.architecture() {
        roc_target::Architecture::X86_64 if cfg!(feature = "target-x86_64") => "x86-64",
        roc_target::Architecture::X86_32 if cfg!(feature = "target-x86") => "x86",
        roc_target::Architecture::Aarch64 if cfg!(feature = "target-aarch64") => "aarch64",
        roc_target::Architecture::Aarch32 if cfg!(feature = "target-arm") => "arm",
        roc_target::Architecture::Wasm32 if cfg!(feature = "target-wasm32") => "wasm32",
        _ => internal_error!(
            "TODO gracefully handle unsupported target architecture: {:?}",
            target.architecture()
        ),
    }
}

pub fn target_machine(
    target: Target,
    opt: OptimizationLevel,
    reloc: RelocMode,
) -> Option<TargetMachine> {
    let arch = arch_str(target);

    init_arch(target);

    let code_model = match target {
        Target::MacArm64 => {
            // We used to have a problem that LLVM 12 would not compile our programs without a large code model.
            // The reason was not totally clear to us, but one guess is a few special-cases in
            //   llvm/lib/Target/AArch64/AArch64ISelLowering.cpp (instructions)
            //   llvm/lib/Target/AArch64/AArch64Subtarget.cpp (GoT tables)
            // Revisit when upgrading to LLVM 13.
            //
            // Most recently, we seem to only see this problem on macOS ARM64; removing this
            // failed macOS CI here: https://github.com/roc-lang/roc/pull/5644
            CodeModel::Large
        }
        _ => CodeModel::Default,
    };

    LlvmTarget::from_name(arch).unwrap().create_target_machine(
        &TargetTriple::create(target_triple_str(target)),
        "generic",
        "",
        opt,
        reloc,
        code_model,
    )
}

pub fn convert_opt_level(level: OptLevel) -> OptimizationLevel {
    match level {
        OptLevel::Development | OptLevel::Normal => OptimizationLevel::None,
        // Default is O2/Os. If we want Oz, we have to explicitly turn of loop vectorization as well.
        OptLevel::Size => OptimizationLevel::Default,
        OptLevel::Optimize => OptimizationLevel::Aggressive,
    }
}
