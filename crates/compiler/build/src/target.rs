use inkwell::{
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
    OptimizationLevel,
};
use roc_error_macros::internal_error;
use roc_mono::ir::OptLevel;
use target_lexicon::{Architecture, Environment, OperatingSystem, Triple};

pub fn target_triple_str(target: &Triple) -> &'static str {
    // Best guide I've found on how to determine these magic strings:
    //
    // https://stackoverflow.com/questions/15036909/clang-how-to-list-supported-target-architectures
    match target {
        Triple {
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Linux,
            ..
        } => "x86_64-unknown-linux-gnu",
        Triple {
            architecture: Architecture::X86_32(target_lexicon::X86_32Architecture::I386),
            operating_system: OperatingSystem::Linux,
            ..
        } => "i386-unknown-linux-gnu",
        Triple {
            architecture: Architecture::Wasm32,
            ..
        } => "wasm32-unknown-unknown",
        Triple {
            architecture: Architecture::Aarch64(_),
            operating_system: OperatingSystem::Linux,
            ..
        } => "aarch64-unknown-linux-gnu",
        Triple {
            architecture: Architecture::Aarch64(_),
            operating_system: OperatingSystem::Darwin,
            ..
        } => "aarch64-apple-darwin",
        Triple {
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Darwin,
            ..
        } => "x86_64-unknown-darwin10",
        Triple {
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Windows,
            ..
        } => "x86_64-pc-windows-gnu",
        _ => internal_error!("TODO gracefully handle unsupported target: {:?}", target),
    }
}

pub fn target_zig_str(target: &Triple) -> &'static str {
    // Zig has its own architecture mappings, defined here:
    // https://github.com/ziglang/zig/blob/master/tools/process_headers.zig
    //
    // and an open proposal to unify them with the more typical "target triples":
    // https://github.com/ziglang/zig/issues/4911
    match target {
        Triple {
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Linux,
            environment: Environment::Musl,
            ..
        } => "x86_64-linux-musl",
        Triple {
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Linux,
            ..
        } => "x86_64-linux-gnu",
        Triple {
            architecture: Architecture::X86_32(target_lexicon::X86_32Architecture::I386),
            operating_system: OperatingSystem::Linux,
            environment: Environment::Musl,
            ..
        } => "i386-linux-musl",
        Triple {
            architecture: Architecture::X86_32(target_lexicon::X86_32Architecture::I386),
            operating_system: OperatingSystem::Linux,
            ..
        } => "i386-linux-gnu",
        Triple {
            architecture: Architecture::Aarch64(_),
            operating_system: OperatingSystem::Linux,
            ..
        } => "aarch64-linux-gnu",
        Triple {
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Darwin,
            ..
        } => "x86_64-macos-gnu",
        Triple {
            architecture: Architecture::Aarch64(_),
            operating_system: OperatingSystem::Darwin,
            ..
        } => "aarch64-macos-gnu",
        _ => internal_error!("TODO gracefully handle unsupported target: {:?}", target),
    }
}

pub fn init_arch(target: &Triple) {
    match target.architecture {
        Architecture::X86_64 | Architecture::X86_32(_)
            if cfg!(any(feature = "target-x86", feature = "target-x86_64")) =>
        {
            Target::initialize_x86(&InitializationConfig::default());
        }
        Architecture::Aarch64(_) if cfg!(feature = "target-aarch64") => {
            Target::initialize_aarch64(&InitializationConfig::default());
        }
        Architecture::Arm(_) if cfg!(feature = "target-arm") => {
            Target::initialize_arm(&InitializationConfig::default());
        }
        Architecture::Wasm32 if cfg!(feature = "target-wasm32") => {
            Target::initialize_webassembly(&InitializationConfig::default());
        }
        _ => internal_error!(
            "TODO gracefully handle unsupported target architecture: {:?}",
            target.architecture
        ),
    }
}

/// NOTE: arch_str is *not* the same as the beginning of the magic target triple
/// string! For example, if it's "x86-64" here, the magic target triple string
/// will begin with "x86_64" (with an underscore) instead.
pub fn arch_str(target: &Triple) -> &'static str {
    // Best guide I've found on how to determine these magic strings:
    //
    // https://stackoverflow.com/questions/15036909/clang-how-to-list-supported-target-architectures
    match target.architecture {
        Architecture::X86_64 if cfg!(feature = "target-x86_64") => "x86-64",
        Architecture::X86_32(_) if cfg!(feature = "target-x86") => "x86",
        Architecture::Aarch64(_) if cfg!(feature = "target-aarch64") => "aarch64",
        Architecture::Arm(_) if cfg!(feature = "target-arm") => "arm",
        Architecture::Wasm32 if cfg!(feature = "target-webassembly") => "wasm32",
        _ => internal_error!(
            "TODO gracefully handle unsupported target architecture: {:?}",
            target.architecture
        ),
    }
}

pub fn target_machine(
    target: &Triple,
    opt: OptimizationLevel,
    reloc: RelocMode,
) -> Option<TargetMachine> {
    let arch = arch_str(target);

    init_arch(target);

    let code_model = match target.architecture {
        // LLVM 12 will not compile our programs without a large code model.
        // The reason is not totally clear to me, but my guess is a few special-cases in
        //   llvm/lib/Target/AArch64/AArch64ISelLowering.cpp (instructions)
        //   llvm/lib/Target/AArch64/AArch64Subtarget.cpp (GoT tables)
        // Revisit when upgrading to LLVM 13.
        Architecture::Aarch64(..) => CodeModel::Large,
        _ => CodeModel::Default,
    };

    Target::from_name(arch).unwrap().create_target_machine(
        &TargetTriple::create(target_triple_str(target)),
        "generic",
        "", // TODO: this probably should be TargetMachine::get_host_cpu_features() to enable all features.
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
