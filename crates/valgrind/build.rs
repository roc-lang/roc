#[cfg(all(target_os = "linux"))]
fn main() {
    use roc_build::link::preprocessed_host_filename;
    use roc_linker::build_and_preprocess_host;

    let platform_main_roc = std::env::current_dir()
        .unwrap()
        .join("zig-platform/main.roc");

    // tests always run on the host
    let target = target_lexicon::Triple::host();

    // the preprocessed host is stored beside the platform's main.roc
    let preprocessed_host_path =
        platform_main_roc.with_file_name(preprocessed_host_filename(&target).unwrap());

    build_and_preprocess_host(
        roc_mono::ir::OptLevel::Normal,
        &target,
        &platform_main_roc,
        &preprocessed_host_path,
        vec![String::from("mainForHost")],
        vec![],
    );
}

#[cfg(not(all(target_os = "linux")))]
fn main() {}
