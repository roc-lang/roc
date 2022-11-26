use std::{os::unix::process::parent_id, sync::Arc};

use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use inkwell::context::Context;
use roc_build::link::llvm_module_to_dylib;
use roc_can::expr::ExpectLookup;
use roc_collections::{MutSet, VecMap};
use roc_error_macros::internal_error;
use roc_gen_llvm::{
    llvm::{build::LlvmBackendMode, externs::add_default_roc_externs},
    run_roc::RocCallResult,
    run_roc_dylib,
};
use roc_intern::{GlobalInterner, SingleThreadedInterner};
use roc_load::{EntryPoint, Expectations, MonomorphizedModule};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::{ir::OptLevel, layout::Layout};
use roc_region::all::Region;
use roc_reporting::{error::expect::Renderer, report::RenderTarget};
use roc_target::TargetInfo;
use roc_types::subs::{Subs, Variable};
use target_lexicon::Triple;

pub struct ExpectMemory<'a> {
    ptr: *mut u8,
    length: usize,
    shm_name: Option<std::ffi::CString>,
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> ExpectMemory<'a> {
    const SHM_SIZE: usize = 1024;

    #[cfg(test)]
    pub(crate) fn from_slice(slice: &mut [u8]) -> Self {
        Self {
            ptr: slice.as_mut_ptr(),
            length: slice.len(),
            shm_name: None,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn create_or_reuse_mmap(shm_name: &str) -> Self {
        let cstring = std::ffi::CString::new(shm_name).unwrap();
        Self::mmap_help(cstring, libc::O_RDWR | libc::O_CREAT)
    }

    fn reuse_mmap(&mut self) -> Option<Self> {
        let shm_name = self.shm_name.as_ref()?.clone();
        Some(Self::mmap_help(shm_name, libc::O_RDWR))
    }

    fn mmap_help(cstring: std::ffi::CString, shm_flags: i32) -> Self {
        let ptr = unsafe {
            let shared_fd = libc::shm_open(cstring.as_ptr().cast(), shm_flags, 0o666);
            if shared_fd == -1 {
                internal_error!("failed to shm_open fd");
            }

            let mut stat: libc::stat = std::mem::zeroed();
            if libc::fstat(shared_fd, &mut stat) == -1 {
                internal_error!("failed to stat shared file, does it exist?");
            }
            if stat.st_size < Self::SHM_SIZE as _
                && libc::ftruncate(shared_fd, Self::SHM_SIZE as _) == -1
            {
                internal_error!("failed to truncate shared file, are the permissions wrong?");
            }

            let ptr = libc::mmap(
                std::ptr::null_mut(),
                Self::SHM_SIZE,
                libc::PROT_WRITE | libc::PROT_READ,
                libc::MAP_SHARED,
                shared_fd,
                0,
            );

            if ptr as usize == usize::MAX {
                // ptr = -1
                roc_error_macros::internal_error!("failed to mmap shared pointer")
            }

            // fill the buffer with a fill pattern
            libc::memset(ptr, 0xAA, Self::SHM_SIZE);

            ptr
        };

        // puts in the initial header
        let _ = ExpectSequence::new(ptr as *mut u8);

        Self {
            ptr: ptr.cast(),
            length: Self::SHM_SIZE,
            shm_name: Some(cstring),
            _marker: std::marker::PhantomData,
        }
    }

    fn set_shared_buffer(&mut self, lib: &libloading::Library) {
        let set_shared_buffer = run_roc_dylib!(lib, "set_shared_buffer", (*mut u8, usize), ());
        let mut result = RocCallResult::default();
        unsafe { set_shared_buffer((self.ptr, self.length), &mut result) };
    }
}

#[allow(clippy::too_many_arguments)]
pub fn run_inline_expects<'a, W: std::io::Write>(
    writer: &mut W,
    render_target: RenderTarget,
    arena: &'a Bump,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
    lib: &libloading::Library,
    expectations: &mut VecMap<ModuleId, Expectations>,
    expects: ExpectFunctions<'_>,
) -> std::io::Result<(usize, usize)> {
    let shm_name = format!("/roc_expect_buffer_{}", std::process::id());
    let mut memory = ExpectMemory::create_or_reuse_mmap(&shm_name);

    run_expects_with_memory(
        writer,
        render_target,
        arena,
        interns,
        layout_interner,
        lib,
        expectations,
        expects,
        &mut memory,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn run_toplevel_expects<'a, W: std::io::Write>(
    writer: &mut W,
    render_target: RenderTarget,
    arena: &'a Bump,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
    lib: &libloading::Library,
    expectations: &mut VecMap<ModuleId, Expectations>,
    expects: ExpectFunctions<'_>,
) -> std::io::Result<(usize, usize)> {
    let shm_name = format!("/roc_expect_buffer_{}", std::process::id());
    let mut memory = ExpectMemory::create_or_reuse_mmap(&shm_name);

    run_expects_with_memory(
        writer,
        render_target,
        arena,
        interns,
        layout_interner,
        lib,
        expectations,
        expects,
        &mut memory,
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_expects_with_memory<'a, W: std::io::Write>(
    writer: &mut W,
    render_target: RenderTarget,
    arena: &'a Bump,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
    lib: &libloading::Library,
    expectations: &mut VecMap<ModuleId, Expectations>,
    expects: ExpectFunctions<'_>,
    memory: &mut ExpectMemory,
) -> std::io::Result<(usize, usize)> {
    let mut failed = 0;
    let mut passed = 0;

    for expect in expects.fx {
        let result = run_expect_fx(
            writer,
            render_target,
            arena,
            interns,
            layout_interner,
            lib,
            expectations,
            memory,
            expect,
        )?;

        match result {
            true => passed += 1,
            false => failed += 1,
        }
    }

    memory.set_shared_buffer(lib);

    for expect in expects.pure {
        let result = run_expect_pure(
            writer,
            render_target,
            arena,
            interns,
            layout_interner,
            lib,
            expectations,
            memory,
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
fn run_expect_pure<'a, W: std::io::Write>(
    writer: &mut W,
    render_target: RenderTarget,
    arena: &'a Bump,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
    lib: &libloading::Library,
    expectations: &mut VecMap<ModuleId, Expectations>,
    shared_memory: &mut ExpectMemory,
    expect: ToplevelExpect<'_>,
) -> std::io::Result<bool> {
    use roc_gen_llvm::try_run_jit_function;

    let sequence = ExpectSequence::new(shared_memory.ptr.cast());

    let result: Result<(), (String, _)> = try_run_jit_function!(lib, expect.name, (), |v: ()| v);

    let shared_memory_ptr: *const u8 = shared_memory.ptr.cast();

    if result.is_err() || sequence.count_failures() > 0 {
        let module_id = expect.symbol.module_id();
        let data = expectations.get_mut(&module_id).unwrap();

        let path = &data.path;
        let filename = data.path.to_owned();
        let source = std::fs::read_to_string(path).unwrap();

        let renderer = Renderer::new(arena, interns, render_target, module_id, filename, &source);

        if let Err((roc_panic_message, _roc_panic_tag)) = result {
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
                    layout_interner,
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

#[allow(clippy::too_many_arguments)]
fn run_expect_fx<'a, W: std::io::Write>(
    writer: &mut W,
    render_target: RenderTarget,
    arena: &'a Bump,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
    lib: &libloading::Library,
    expectations: &mut VecMap<ModuleId, Expectations>,
    parent_memory: &mut ExpectMemory,
    expect: ToplevelExpect<'_>,
) -> std::io::Result<bool> {
    use signal_hook::{consts::signal::SIGCHLD, consts::signal::SIGUSR1, iterator::Signals};

    let mut signals = Signals::new([SIGCHLD, SIGUSR1]).unwrap();

    match unsafe { libc::fork() } {
        0 => unsafe {
            // we are the child

            use roc_gen_llvm::try_run_jit_function;

            let mut child_memory = parent_memory.reuse_mmap().unwrap();

            let sequence = ExpectSequence::new(child_memory.ptr);

            child_memory.set_shared_buffer(lib);

            let result: Result<(), (String, _)> =
                try_run_jit_function!(lib, expect.name, (), |v: ()| v);

            if let Err((msg, _)) = result {
                panic!("roc panic {}", msg);
            }

            if sequence.count_failures() > 0 {
                libc::kill(parent_id() as _, SIGUSR1);
            }

            std::process::exit(0)
        },
        -1 => {
            // something failed

            // Display a human-friendly error message
            println!("Error {:?}", std::io::Error::last_os_error());

            std::process::exit(1)
        }
        1.. => {
            let mut has_succeeded = true;

            for sig in &mut signals {
                match sig {
                    SIGCHLD => {
                        // done!
                        return Ok(has_succeeded);
                    }
                    SIGUSR1 => {
                        // this is the signal we use for an expect failure. Let's see what the child told us
                        has_succeeded = false;

                        let frame =
                            ExpectFrame::at_offset(parent_memory.ptr, ExpectSequence::START_OFFSET);
                        let module_id = frame.module_id;

                        let data = expectations.get_mut(&module_id).unwrap();
                        let filename = data.path.to_owned();
                        let source = std::fs::read_to_string(&data.path).unwrap();

                        let renderer = Renderer::new(
                            arena,
                            interns,
                            render_target,
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
                            layout_interner,
                            parent_memory.ptr,
                            ExpectSequence::START_OFFSET,
                        )?;
                    }
                    _ => println!("received signal {}", sig),
                }
            }

            Ok(true)
        }
        _ => unreachable!(),
    }
}

pub fn render_expects_in_memory<'a>(
    writer: &mut impl std::io::Write,
    arena: &'a Bump,
    expectations: &mut VecMap<ModuleId, Expectations>,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
    memory: &ExpectMemory,
) -> std::io::Result<usize> {
    let shared_ptr = memory.ptr;

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
        layout_interner,
        shared_ptr,
        ExpectSequence::START_OFFSET,
    )
}

pub fn render_dbgs_in_memory<'a>(
    writer: &mut impl std::io::Write,
    arena: &'a Bump,
    expectations: &mut VecMap<ModuleId, Expectations>,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
    memory: &ExpectMemory,
) -> std::io::Result<usize> {
    let shared_ptr = memory.ptr;

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

    render_dbg_failure(
        writer,
        &renderer,
        arena,
        expectations,
        interns,
        layout_interner,
        shared_ptr,
        ExpectSequence::START_OFFSET,
    )
}

fn split_expect_lookups(subs: &Subs, lookups: &[ExpectLookup]) -> (Vec<Symbol>, Vec<Variable>) {
    lookups
        .iter()
        .filter_map(
            |ExpectLookup {
                 symbol,
                 var,
                 ability_info: _,
             }| {
                // mono will have dropped lookups that resolve to functions, so we should not keep
                // them either.
                if subs.is_function(*var) {
                    None
                } else {
                    Some((*symbol, *var))
                }
            },
        )
        .unzip()
}

#[allow(clippy::too_many_arguments)]
fn render_dbg_failure<'a>(
    writer: &mut impl std::io::Write,
    renderer: &Renderer,
    arena: &'a Bump,
    expectations: &mut VecMap<ModuleId, Expectations>,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
    start: *const u8,
    offset: usize,
) -> std::io::Result<usize> {
    // we always run programs as the host
    let target_info = (&target_lexicon::Triple::host()).into();

    let frame = ExpectFrame::at_offset(start, offset);
    let module_id = frame.module_id;

    let failure_region = frame.region;
    let dbg_symbol = unsafe { std::mem::transmute::<_, Symbol>(failure_region) };
    let expect_region = Some(Region::zero());

    let data = expectations.get_mut(&module_id).unwrap();

    let current = match data.dbgs.get(&dbg_symbol) {
        None => panic!("region {failure_region:?} not in list of dbgs"),
        Some(current) => current,
    };
    let failure_region = current.region;

    let subs = arena.alloc(&mut data.subs);

    let current = ExpectLookup {
        symbol: current.symbol,
        var: current.var,
        ability_info: current.ability_info,
    };

    let (_symbols, variables) = split_expect_lookups(subs, &[current]);

    let (offset, expressions) = crate::get_values(
        target_info,
        arena,
        subs,
        interns,
        layout_interner,
        start,
        frame.start_offset,
        &variables,
    );

    renderer.render_dbg(writer, &expressions, expect_region, failure_region)?;

    Ok(offset)
}

#[allow(clippy::too_many_arguments)]
fn render_expect_failure<'a>(
    writer: &mut impl std::io::Write,
    renderer: &Renderer,
    arena: &'a Bump,
    expect: Option<ToplevelExpect>,
    expectations: &mut VecMap<ModuleId, Expectations>,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
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
        None => panic!("region {failure_region:?} not in list of expects"),
        Some(current) => current,
    };
    let subs = arena.alloc(&mut data.subs);

    let (symbols, variables) = split_expect_lookups(subs, current);

    let (offset, expressions) = crate::get_values(
        target_info,
        arena,
        subs,
        interns,
        layout_interner,
        start,
        frame.start_offset,
        &variables,
    );

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
            std::ptr::write_unaligned(ptr.add(Self::COUNT_INDEX), 0);
            std::ptr::write_unaligned(ptr.add(Self::OFFSET_INDEX), Self::START_OFFSET);
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

#[derive(Debug)]
pub struct ExpectFunctions<'a> {
    pub pure: BumpVec<'a, ToplevelExpect<'a>>,
    pub fx: BumpVec<'a, ToplevelExpect<'a>>,
}

pub fn expect_mono_module_to_dylib<'a>(
    arena: &'a Bump,
    target: Triple,
    loaded: MonomorphizedModule<'a>,
    opt_level: OptLevel,
    mode: LlvmBackendMode,
) -> Result<
    (
        libloading::Library,
        ExpectFunctions<'a>,
        SingleThreadedInterner<'a, Layout<'a>>,
    ),
    libloading::Error,
> {
    let target_info = TargetInfo::from(&target);

    let MonomorphizedModule {
        toplevel_expects,
        procedures,
        entry_point,
        interns,
        layout_interner,
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
        layout_interner: &layout_interner,
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

    let opt_entry_point = match entry_point {
        EntryPoint::Executable { symbol, layout, .. } => {
            Some(roc_mono::ir::EntryPoint { symbol, layout })
        }
        EntryPoint::Test => None,
    };

    let capacity = toplevel_expects.pure.len() + toplevel_expects.fx.len();
    let mut expect_symbols = BumpVec::with_capacity_in(capacity, env.arena);

    expect_symbols.extend(toplevel_expects.pure.keys().copied());
    expect_symbols.extend(toplevel_expects.fx.keys().copied());

    let expect_names = roc_gen_llvm::llvm::build::build_procedures_expose_expects(
        &env,
        opt_level,
        &expect_symbols,
        procedures,
        opt_entry_point,
    );

    let expects_fx = bumpalo::collections::Vec::from_iter_in(
        toplevel_expects
            .fx
            .into_iter()
            .zip(expect_names.iter().skip(toplevel_expects.pure.len()))
            .map(|((symbol, region), name)| ToplevelExpect {
                symbol,
                region,
                name,
            }),
        env.arena,
    );

    let expects_pure = bumpalo::collections::Vec::from_iter_in(
        toplevel_expects
            .pure
            .into_iter()
            .zip(expect_names.iter())
            .map(|((symbol, region), name)| ToplevelExpect {
                symbol,
                region,
                name,
            }),
        env.arena,
    );

    let expects = ExpectFunctions {
        pure: expects_pure,
        fx: expects_fx,
    };

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
        let path = std::env::temp_dir().join("test.ll");
        env.module.print_to_file(&path).unwrap();
        panic!(
            "Errors defining module:\n{}\n\nUncomment things nearby to see more details. IR written to `{:?}`",
            errors.to_string(), path,
        );
    }

    llvm_module_to_dylib(env.module, &target, opt_level).map(|lib| (lib, expects, layout_interner))
}
