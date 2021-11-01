#[cfg(feature = "llvm")]
use inkwell::{
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
    OptimizationLevel,
};
#[cfg(feature = "llvm")]
use roc_mono::ir::OptLevel;
use target_lexicon::{Architecture, OperatingSystem, Triple};
use anyhow::anyhow;
use anyhow::Result;

pub fn target_triple_str(target: &Triple) -> Result<&'static str> {
    // Best guide I've found on how to determine these magic strings:
    //
    // https://stackoverflow.com/questions/15036909/clang-how-to-list-supported-target-architectures
    match target {
        Triple {
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Linux,
            ..
        } => Ok("x86_64-unknown-linux-gnu"),
        Triple {
            architecture: Architecture::X86_32(target_lexicon::X86_32Architecture::I386),
            operating_system: OperatingSystem::Linux,
            ..
        } => Ok("i386-unknown-linux-gnu"),
        Triple {
            architecture: Architecture::Wasm32,
            ..
        } => Ok("wasm32-unknown-unknown"),
        Triple {
            architecture: Architecture::Aarch64(_),
            operating_system: OperatingSystem::Linux,
            ..
        } => Ok("aarch64-unknown-linux-gnu"),
        Triple {
            architecture: Architecture::Aarch64(_),
            operating_system: OperatingSystem::Darwin,
            ..
        } => Ok("aarch64-apple-darwin"),
        Triple {
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Darwin,
            ..
        } => Ok("x86_64-unknown-darwin10"),
        _ => anyhow::bail!("TODO gracefully handle unsupported target: {:?}", target),
    }
}

#[cfg(feature = "llvm")]
pub fn init_arch(target: &Triple) -> Result<()> {
    match target.architecture {
        Architecture::X86_64 | Architecture::X86_32(_) => {
            Target::initialize_x86(&InitializationConfig::default());
        }
        Architecture::Aarch64(_) if cfg!(feature = "target-aarch64") => {
            Target::initialize_aarch64(&InitializationConfig::default());
        }
        Architecture::Arm(_) if cfg!(feature = "target-arm") => {
            Target::initialize_arm(&InitializationConfig::default());
        }
        Architecture::Wasm32 if cfg!(feature = "target-webassembly") => {
            Target::initialize_webassembly(&InitializationConfig::default());
        }
        _ => anyhow::bail!(
            "TODO gracefully handle unsupported target architecture: {:?}",
            target.architecture
        ),
    }
    Ok(())
}

/// NOTE: arch_str is *not* the same as the beginning of the magic target triple
/// string! For example, if it's "x86-64" here, the magic target triple string
/// will begin with "x86_64" (with an underscore) instead.
pub fn arch_str(target: &Triple) -> Result<&'static str> {
    // Best guide I've found on how to determine these magic strings:
    //
    // https://stackoverflow.com/questions/15036909/clang-how-to-list-supported-target-architectures
    match target.architecture {
        Architecture::X86_64 => Ok("x86-64"),
        Architecture::X86_32(_) => Ok("x86"),
        Architecture::Aarch64(_) if cfg!(feature = "target-aarch64") => Ok("aarch64"),
        Architecture::Arm(_) if cfg!(feature = "target-arm") => Ok("arm"),
        Architecture::Wasm32 if cfg!(feature = "target-webassembly") => Ok("wasm32"),
        _ => anyhow::bail!(
            "TODO gracefully handle unsupported target architecture: {:?}",
            target.architecture
        ),
    }
}

#[cfg(feature = "llvm")]
pub fn target_machine(
    target: &Triple,
    opt: OptimizationLevel,
    reloc: RelocMode,
    model: CodeModel,
) -> Result<Option<TargetMachine>> {
    let arch = arch_str(target)?;

    init_arch(target)?;

    let target_triple = TargetTriple::create(target_triple_str(target)?);

    let target = Target::from_name(arch)
        .ok_or_else(|| anyhow!("No target with name '{}' found", arch))?;

    Ok(target.create_target_machine(
        &target_triple,
        "generic",
        "", // TODO: this probably should be TargetMachine::get_host_cpu_features() to enable all features.
        opt,
        reloc,
        model,
    ))
}

#[cfg(feature = "llvm")]
pub fn convert_opt_level(level: OptLevel) -> OptimizationLevel {
    match level {
        OptLevel::Development | OptLevel::Normal => OptimizationLevel::None,
        OptLevel::Optimize => OptimizationLevel::Aggressive,
    }
}
