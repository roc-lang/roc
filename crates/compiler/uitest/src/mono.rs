use std::io;

use bumpalo::Bump;
use roc_collections::MutMap;
use roc_load::{ExecutionMode, LoadConfig, LoadMonomorphizedError, Threading};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::{
    ir::{Proc, ProcLayout},
    layout::STLayoutInterner,
};

pub fn write_compiled_ir(writer: &mut impl io::Write, module_source: &str) -> io::Result<()> {
    use roc_packaging::cache::RocCacheDir;
    use std::path::PathBuf;

    let exec_mode = ExecutionMode::Executable;

    let arena = &Bump::new();

    let filename = PathBuf::from("Test.roc");
    let src_dir = PathBuf::from("fake/test/path");

    let load_config = LoadConfig {
        target_info: roc_target::TargetInfo::default_x86_64(),
        threading: Threading::Single,
        render: roc_reporting::report::RenderTarget::Generic,
        palette: roc_reporting::report::DEFAULT_PALETTE,
        exec_mode,
    };
    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        filename,
        module_source,
        src_dir,
        RocCacheDir::Disallowed,
        load_config,
    );

    let loaded = match loaded {
        Ok(x) => x,
        Err(LoadMonomorphizedError::LoadingProblem(roc_load::LoadingProblem::FormattedReport(
            report,
        ))) => {
            println!("{}", report);
            panic!();
        }
        Err(e) => panic!("{:?}", e),
    };

    use roc_load::MonomorphizedModule;
    let MonomorphizedModule {
        procedures,
        exposed_to_host,
        mut layout_interner,
        interns,
        ..
    } = loaded;

    let main_fn_symbol = exposed_to_host.top_level_values.keys().copied().next();

    check_procedures(arena, &interns, &mut layout_interner, &procedures);

    write_procedures(writer, layout_interner, procedures, main_fn_symbol)
}

fn check_procedures<'a>(
    arena: &'a Bump,
    interns: &Interns,
    interner: &mut STLayoutInterner<'a>,
    procedures: &MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    use roc_mono::debug::{check_procs, format_problems};
    let problems = check_procs(arena, interner, procedures);
    if problems.is_empty() {
        return;
    }
    let formatted = format_problems(interns, interner, problems);
    panic!("IR problems found:\n{formatted}");
}

fn write_procedures<'a>(
    writer: &mut impl io::Write,
    interner: STLayoutInterner<'a>,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    opt_main_fn_symbol: Option<Symbol>,
) -> io::Result<()> {
    let mut procs_strings = procedures
        .values()
        .map(|proc| proc.to_pretty(&interner, 200, false))
        .collect::<Vec<_>>();

    let opt_main_fn = opt_main_fn_symbol.map(|main_fn_symbol| {
        let index = procedures
            .keys()
            .position(|(s, _)| *s == main_fn_symbol)
            .unwrap();
        procs_strings.swap_remove(index)
    });

    procs_strings.sort();

    if let Some(main_fn) = opt_main_fn {
        procs_strings.push(main_fn);
    }

    let mut procs = procs_strings.iter().peekable();
    while let Some(proc) = procs.next() {
        if procs.peek().is_some() {
            writeln!(writer, "{}", proc)?;
        } else {
            write!(writer, "{}", proc)?;
        }
    }

    Ok(())
}
