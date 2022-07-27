use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use inkwell::context::Context;
use roc_build::link::llvm_module_to_dylib;
use roc_collections::{MutSet, VecMap};
use roc_gen_llvm::llvm::{build::LlvmBackendMode, externs::add_default_roc_externs};
use roc_load::{Expectations, MonomorphizedModule};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::OptLevel;
use roc_region::all::Region;
use roc_reporting::{error::expect::Renderer, report::RenderTarget};
use roc_target::TargetInfo;
use target_lexicon::Triple;

#[allow(clippy::too_many_arguments)]
pub fn run_expects<W: std::io::Write>(
    writer: &mut W,
    render_target: RenderTarget,
    arena: &Bump,
    interns: &Interns,
    lib: &libloading::Library,
    expectations: &mut VecMap<ModuleId, Expectations>,
    shared_ptr: *mut u8,
    expects: bumpalo::collections::Vec<'_, ToplevelExpect<'_>>,
) -> std::io::Result<(usize, usize)> {
    let mut failed = 0;
    let mut passed = 0;

    for expect in expects {
        let result = run_expect(
            writer,
            render_target,
            arena,
            interns,
            lib,
            expectations,
            shared_ptr,
            expect,
        )?;

        match result {
            true => passed += 1,
            false => failed += 1,
        }
    }

    Ok((failed, passed))
}

#[allow(clippy::too_many_arguments)]
fn run_expect<W: std::io::Write>(
    writer: &mut W,
    render_target: RenderTarget,
    arena: &Bump,
    interns: &Interns,
    lib: &libloading::Library,
    expectations: &mut VecMap<ModuleId, Expectations>,
    shared_ptr: *mut u8,
    expect: ToplevelExpect<'_>,
) -> std::io::Result<bool> {
    use roc_gen_llvm::try_run_jit_function;

    let sequence = ExpectSequence::new(shared_ptr.cast());

    let result: Result<(), String> = try_run_jit_function!(lib, expect.name, (), |v: ()| v);

    let shared_memory_ptr: *const u8 = shared_ptr.cast();

    if result.is_err() || sequence.count_failures() > 0 {
        let module_id = expect.symbol.module_id();
        let data = expectations.get_mut(&module_id).unwrap();

        let path = &data.path;
        let filename = data.path.to_owned();
        let source = std::fs::read_to_string(path).unwrap();

        let renderer = Renderer::new(arena, interns, render_target, module_id, filename, &source);

        if let Err(roc_panic_message) = result {
            renderer.render_panic(writer, &roc_panic_message, expect.region)?;
        } else {
            let mut offset = ExpectSequence::START_OFFSET;

            for _ in 0..sequence.count_failures() {
                offset += render_expect_failure(
                    writer,
                    &renderer,
                    arena,
                    Some(expect),
                    expectations,
                    interns,
                    shared_memory_ptr,
                    offset,
                )?;
            }
        }

        writeln!(writer)?;

        Ok(false)
    } else {
        Ok(true)
    }
}

pub fn roc_dev_expect(
    writer: &mut impl std::io::Write,
    arena: &Bump,
    expectations: &mut VecMap<ModuleId, Expectations>,
    interns: &Interns,
    shared_ptr: *mut u8,
) -> std::io::Result<usize> {
    let frame = ExpectFrame::at_offset(shared_ptr, ExpectSequence::START_OFFSET);
    let module_id = frame.module_id;

    let data = expectations.get_mut(&module_id).unwrap();
    let filename = data.path.to_owned();
    let source = std::fs::read_to_string(&data.path).unwrap();

    let renderer = Renderer::new(
        arena,
        interns,
        RenderTarget::ColorTerminal,
        module_id,
        filename,
        &source,
    );

    render_expect_failure(
        writer,
        &renderer,
        arena,
        None,
        expectations,
        interns,
        shared_ptr,
        ExpectSequence::START_OFFSET,
    )
}

#[allow(clippy::too_many_arguments)]
fn render_expect_failure<'a>(
    writer: &mut impl std::io::Write,
    renderer: &Renderer,
    arena: &'a Bump,
    expect: Option<ToplevelExpect>,
    expectations: &mut VecMap<ModuleId, Expectations>,
    interns: &'a Interns,
    start: *const u8,
    offset: usize,
) -> std::io::Result<usize> {
    // we always run programs as the host
    let target_info = (&target_lexicon::Triple::host()).into();

    let frame = ExpectFrame::at_offset(start, offset);
    let module_id = frame.module_id;

    let failure_region = frame.region;
    let expect_region = expect.map(|e| e.region);

    let data = expectations.get_mut(&module_id).unwrap();

    let current = match data.expectations.get(&failure_region) {
        None => panic!("region not in list of expects"),
        Some(current) => current,
    };
    let subs = arena.alloc(&mut data.subs);

    let (symbols, variables): (Vec<_>, Vec<_>) = current.iter().map(|(a, b)| (*a, *b)).unzip();

    let (offset, expressions) = crate::get_values(
        target_info,
        arena,
        subs,
        interns,
        start,
        frame.start_offset,
        &variables,
    )
    .unwrap();

    renderer.render_failure(
        writer,
        subs,
        &symbols,
        &variables,
        &expressions,
        expect_region,
        failure_region,
    )?;

    Ok(offset)
}

struct ExpectSequence {
    ptr: *const u8,
}

impl ExpectSequence {
    const START_OFFSET: usize = 16;

    const COUNT_INDEX: usize = 0;
    const OFFSET_INDEX: usize = 1;

    fn new(ptr: *mut u8) -> Self {
        unsafe {
            let ptr = ptr as *mut usize;
            *ptr.add(Self::COUNT_INDEX) = 0;
            *ptr.add(Self::OFFSET_INDEX) = Self::START_OFFSET;
        }

        Self {
            ptr: ptr as *const u8,
        }
    }

    fn count_failures(&self) -> usize {
        unsafe { *(self.ptr as *const usize).add(Self::COUNT_INDEX) }
    }
}

struct ExpectFrame {
    region: Region,
    module_id: ModuleId,
    start_offset: usize,
}

impl ExpectFrame {
    fn at_offset(start: *const u8, offset: usize) -> Self {
        let region_bytes: [u8; 8] = unsafe { *(start.add(offset).cast()) };
        let region: Region = unsafe { std::mem::transmute(region_bytes) };

        let module_id_bytes: [u8; 4] = unsafe { *(start.add(offset + 8).cast()) };
        let module_id: ModuleId = unsafe { std::mem::transmute(module_id_bytes) };

        // skip to frame, 8 bytes for region, 4 for module id
        let start_offset = offset + 12;

        Self {
            region,
            module_id,
            start_offset,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ToplevelExpect<'a> {
    pub name: &'a str,
    pub symbol: Symbol,
    pub region: Region,
}

pub fn expect_mono_module_to_dylib<'a>(
    arena: &'a Bump,
    target: Triple,
    loaded: MonomorphizedModule<'a>,
    opt_level: OptLevel,
    mode: LlvmBackendMode,
) -> Result<(libloading::Library, BumpVec<'a, ToplevelExpect<'a>>), libloading::Error> {
    let target_info = TargetInfo::from(&target);

    let MonomorphizedModule {
        toplevel_expects,
        procedures,
        entry_point,
        interns,
        ..
    } = loaded;

    let context = Context::create();
    let builder = context.create_builder();
    let module = arena.alloc(roc_gen_llvm::llvm::build::module_from_builtins(
        &target, &context, "",
    ));

    let module = arena.alloc(module);
    let (module_pass, _function_pass) =
        roc_gen_llvm::llvm::build::construct_optimization_passes(module, opt_level);

    let (dibuilder, compile_unit) = roc_gen_llvm::llvm::build::Env::new_debug_info(module);

    // Compile and add all the Procs before adding main
    let env = roc_gen_llvm::llvm::build::Env {
        arena,
        builder: &builder,
        dibuilder: &dibuilder,
        compile_unit: &compile_unit,
        context: &context,
        interns,
        module,
        target_info,
        mode,
        // important! we don't want any procedures to get the C calling convention
        exposed_to_host: MutSet::default(),
    };

    // Add roc_alloc, roc_realloc, and roc_dealloc, since the repl has no
    // platform to provide them.
    add_default_roc_externs(&env);

    let expect_names = roc_gen_llvm::llvm::build::build_procedures_expose_expects(
        &env,
        opt_level,
        toplevel_expects.unzip_slices().0,
        procedures,
        entry_point,
    );

    let expects = bumpalo::collections::Vec::from_iter_in(
        toplevel_expects
            .into_iter()
            .zip(expect_names.into_iter())
            .map(|((symbol, region), name)| ToplevelExpect {
                symbol,
                region,
                name,
            }),
        env.arena,
    );

    env.dibuilder.finalize();

    // we don't use the debug info, and it causes weird errors.
    module.strip_debug_info();

    // Uncomment this to see the module's un-optimized LLVM instruction output:
    // env.module.print_to_stderr();

    module_pass.run_on(env.module);

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    // Verify the module
    if let Err(errors) = env.module.verify() {
        env.module.print_to_file("/tmp/test.ll").unwrap();
        panic!(
            "Errors defining module:\n{}\n\nUncomment things nearby to see more details. IR written to `/tmp/test.ll`",
            errors.to_string()
        );
    }

    env.module.print_to_file("/tmp/test.ll").unwrap();

    llvm_module_to_dylib(env.module, &target, opt_level).map(|lib| (lib, expects))
}
