use std::io;

use bumpalo::Bump;
use roc_collections::MutMap;
use roc_load::{ExecutionMode, LoadConfig, LoadMonomorphizedError, Threading};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::{
    ir::{Proc, ProcLayout},
    layout::STLayoutInterner,
};
use tempfile::tempdir;
use test_solve_helpers::format_problems;

use crate::CompilerSettings;

#[derive(Default)]
pub(crate) struct MonoOptions {
    pub no_check: bool,
}

pub(crate) fn write_compiled_ir<'a>(
    writer: &mut impl io::Write,
    test_module: &str,
    dependencies: impl IntoIterator<Item = (&'a str, &'a str)>,
    options: MonoOptions,
    compiler_settings: CompilerSettings,
    allow_can_errors: bool,
) -> io::Result<()> {
    use roc_packaging::cache::RocCacheDir;
    use std::path::PathBuf;

    let exec_mode = ExecutionMode::Executable;

    let arena = &Bump::new();

    let dir = tempdir()?;

    for (file, source) in dependencies {
        std::fs::write(dir.path().join(format!("{file}.roc")), source)?;
    }

    let filename = PathBuf::from("Test.roc");
    let file_path = dir.path().join(filename);

    let load_config = LoadConfig {
        target: roc_target::Target::LinuxX64,
        function_kind: compiler_settings.function_kind,
        threading: Threading::Single,
        render: roc_reporting::report::RenderTarget::Generic,
        palette: roc_reporting::report::DEFAULT_PALETTE,
        exec_mode,
    };
    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        file_path,
        test_module,
        dir.path().to_path_buf(),
        None,
        RocCacheDir::Disallowed,
        load_config,
    );

    let loaded = match loaded {
        Ok(x) => x,
        Err(LoadMonomorphizedError::LoadingProblem(roc_load::LoadingProblem::FormattedReport(
            report,
            _,
        ))) => {
            println!("{report}");
            panic!();
        }
        Err(e) => panic!("{e:?}"),
    };

    use roc_load::MonomorphizedModule;
    let MonomorphizedModule {
        procedures,
        exposed_to_host,
        mut layout_interner,
        interns,
        can_problems,
        mut type_problems,
        sources,
        ..
    } = loaded;

    let main_fn_symbol = exposed_to_host.top_level_values.keys().copied().next();

    for (module, can_problems) in can_problems.into_iter() {
        let type_problems = type_problems.remove(&module).unwrap_or_default();

        let source = sources.get(&module).unwrap();

        let (can_problems, type_problems) =
            format_problems(&source.1, module, &interns, can_problems, type_problems);

        if !can_problems.is_empty() && !allow_can_errors {
            panic!("Canonicalization problems: {can_problems}");
        }
        if !type_problems.is_empty() {
            panic!("Type problems: {type_problems}");
        }
    }

    if !options.no_check {
        check_procedures(arena, &interns, &mut layout_interner, &procedures);
    }

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
            writeln!(writer, "{proc}")?;
        } else {
            write!(writer, "{proc}")?;
        }
    }

    Ok(())
}
