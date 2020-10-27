use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::OptimizationLevel;
use target_lexicon::{Architecture, OperatingSystem, Triple};

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
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Darwin,
            ..
        } => "x86_64-unknown-darwin10",
        _ => panic!("TODO gracefully handle unsupported target: {:?}", target),
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
        Architecture::X86_64 => {
            Target::initialize_x86(&InitializationConfig::default());

            "x86-64"
        }
        Architecture::Arm(_) if cfg!(feature = "target-arm") => {
            // NOTE: why not enable arm and wasm by default?
            //
            // We had some trouble getting them to link properly. This may be resolved in the
            // future, or maybe it was just some weird configuration on one machine.
            Target::initialize_arm(&InitializationConfig::default());

            "arm"
        }
        Architecture::Wasm32 if cfg!(feature = "target-webassembly") => {
            Target::initialize_webassembly(&InitializationConfig::default());

            "wasm32"
        }
        _ => panic!(
            "TODO gracefully handle unsupported target architecture: {:?}",
            target.architecture
        ),
    }
}

pub fn target_machine(
    target: &Triple,
    opt: OptimizationLevel,
    reloc: RelocMode,
    model: CodeModel,
) -> Option<TargetMachine> {
    let arch = arch_str(target);

    Target::from_name(arch).unwrap().create_target_machine(
        &TargetTriple::create(target_triple_str(target)),
        arch,
        // TODO: this probably should be TargetMachine::get_host_cpu_features() to enable all features.
        // For now disabling all avx512 because valgrind does not seem to support it.
        "-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq", 
        opt,
        reloc,
        model,
    )
}
