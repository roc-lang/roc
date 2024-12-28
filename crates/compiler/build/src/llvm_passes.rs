use bumpalo::{collections::String, Bump};
use roc_error_macros::internal_error;
use roc_mono::ir::OptLevel;
use roc_target::Target;
use std::path::Path;

// can see the exact passes run using `opt --passes="default<Oz>" --print-pipeline-passes example.ll`
//
// we also include a "globaldce" pass at the beginning of each pipeline here, which is required
// to prevent bugs with the surgical linker, and also improves the build time.
pub fn get_llvm_passes_string(arena: &Bump, opt_level: OptLevel) -> &str {
    (String::from_str_in("globaldce,", arena)
        + match opt_level {
            OptLevel::Development | OptLevel::Normal => "default<O0>",
            OptLevel::Size => "default<Oz>",
            OptLevel::Optimize => "default<O3>",
        })
    .into_bump_str()
}

pub fn optimize_llvm_ir(
    env: &roc_gen_llvm::llvm::build::Env,
    target: Target,
    opt_level: OptLevel,
    emit_debug_info: bool,
    ll_file_path: &Path,
) {
    env.dibuilder.finalize();

    if !emit_debug_info {
        env.module.strip_debug_info();
    }

    // TODO: double check how much time is spent verifying here and below.
    // For real compilation, we may not want to pay the cost.

    // Verify the module before optimizing
    if let Err(errors) = env.module.verify() {
        // write the ll code to a file, so we can modify it
        env.module.print_to_file(ll_file_path).unwrap();

        internal_error!(
            "😱 LLVM errors when defining module; I wrote the full LLVM IR to {:?}\n\n {}",
            ll_file_path,
            errors.to_string(),
        );
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    let inkwell_opt_level = crate::target::convert_opt_level(opt_level);
    let inkwell_llvm_passes = get_llvm_passes_string(env.arena, opt_level);
    let inkwell_target_machine =
        crate::target::target_machine(target, inkwell_opt_level, inkwell::targets::RelocMode::PIC)
            .unwrap_or_else(|| internal_error!("invalid target machine"));

    env.module
        .run_passes(
            inkwell_llvm_passes,
            &inkwell_target_machine,
            inkwell::passes::PassBuilderOptions::create(),
        )
        .unwrap_or_else(|e| internal_error!("invalid llvm optimization passes: {:?}", e));

    // Verify the module after optimizing
    if let Err(errors) = env.module.verify() {
        // write the ll code to a file, so we can modify it
        env.module.print_to_file(ll_file_path).unwrap();

        internal_error!(
            "😱 LLVM errors when optimizing module; I wrote the full LLVM IR to {:?}\n\n {}",
            ll_file_path,
            errors.to_string(),
        );
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();
}
