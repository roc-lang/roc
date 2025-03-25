use std::sync::{
    atomic::{AtomicBool, AtomicU32},
    Arc,
};

use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use inkwell::context::Context;
use roc_build::link::llvm_module_to_dylib;
use roc_can::expr::ExpectLookup;
use roc_collections::{MutMap, MutSet, VecMap};
use roc_error_macros::internal_error;
use roc_gen_llvm::{
    llvm::{build::LlvmBackendMode, externs::add_default_roc_externs},
    run_roc::RocCallResult,
    run_roc_dylib,
};
use roc_load::{Expectations, MonomorphizedModule};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::{
    ir::OptLevel,
    layout::{GlobalLayoutInterner, STLayoutInterner},
};
use roc_region::all::Region;
use roc_reporting::{error::expect::Renderer, report::RenderTarget};
use roc_target::Target;
use roc_types::subs::Subs;

pub struct ExpectMemory<'a> {
    ptr: *mut u8,
    length: usize,
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> ExpectMemory<'a> {
    const SHM_SIZE: usize = 1024;

    #[cfg(test)]
    pub(crate) fn from_slice(slice: &mut [u8]) -> Self {
        Self {
            ptr: slice.as_mut_ptr(),
            length: slice.len(),
            _marker: std::marker::PhantomData,
        }
    }

    pub fn create_or_reuse_mmap(shm_name: &str) -> Self {
        let cstring = std::ffi::CString::new(shm_name).unwrap();
        Self::mmap_help(cstring, libc::O_RDWR | libc::O_CREAT)
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
            _marker: std::marker::PhantomData,
        }
    }

    fn set_shared_buffer(&mut self, lib: &libloading::Library) {
        let set_shared_buffer = run_roc_dylib!(lib, "set_shared_buffer", (*mut u8, usize), ());
        let mut result = RocCallResult::default();
        unsafe { set_shared_buffer((self.ptr, self.length), &mut result) };
    }

    pub fn wait_for_child(&self, sigchld: Arc<AtomicBool>) -> ChildProcessMsg {
        let sequence = ExpectSequence { ptr: self.ptr };
        sequence.wait_for_child(sigchld)
    }

    pub fn reset(&mut self) {
        let mut sequence = ExpectSequence { ptr: self.ptr };
        sequence.reset();
    }
}

#[allow(clippy::too_many_arguments)]
pub fn run_toplevel_expects<'a, W: std::io::Write>(
    writer: &mut W,
    render_target: RenderTarget,
    arena: &'a Bump,
    interns: &'a Interns,
    layout_interner: &GlobalLayoutInterner<'a>,
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
    layout_interner: &GlobalLayoutInterner<'a>,
    lib: &libloading::Library,
    expectations: &mut VecMap<ModuleId, Expectations>,
    expects: ExpectFunctions<'_>,
    memory: &mut ExpectMemory,
) -> std::io::Result<(usize, usize)> {
    let mut failed = 0;
    let mut passed = 0;

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
    layout_interner: &GlobalLayoutInterner<'a>,
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
                offset = render_expect_failure(
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

pub fn render_expects_in_memory<'a>(
    writer: &mut impl std::io::Write,
    arena: &'a Bump,
    expectations: &mut VecMap<ModuleId, Expectations>,
    interns: &'a Interns,
    layout_interner: &GlobalLayoutInterner<'a>,
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

fn split_expect_lookups(subs: &Subs, lookups: &[ExpectLookup]) -> Vec<Symbol> {
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
                    Some(*symbol)
                }
            },
        )
        .collect()
}

#[allow(clippy::too_many_arguments)]
fn render_expect_failure<'a>(
    writer: &mut impl std::io::Write,
    renderer: &Renderer,
    arena: &'a Bump,
    expect: Option<ToplevelExpect>,
    expectations: &mut VecMap<ModuleId, Expectations>,
    interns: &'a Interns,
    layout_interner: &GlobalLayoutInterner<'a>,
    start: *const u8,
    offset: usize,
) -> std::io::Result<usize> {
    // we always run programs as the host
    let target = target_lexicon::Triple::host().into();

    let frame = ExpectFrame::at_offset(start, offset);
    let module_id = frame.module_id;

    let failure_region = frame.region;
    let expect_region = expect.map(|e| e.region);

    let data = expectations.get_mut(&module_id).unwrap();

    let current = match data.expectations.get(&failure_region) {
        None => internal_error!("region {failure_region:?} not in list of expects"),
        Some(current) => current,
    };

    let symbols = split_expect_lookups(&data.subs, current);

    let (offset, expressions, variables) = crate::get_values(
        target,
        arena,
        &data.subs,
        interns,
        layout_interner,
        start,
        frame.start_offset,
        symbols.len(),
    );

    renderer.render_failure(
        writer,
        &mut data.subs,
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
    const START_OFFSET: usize = 8 + 8 + 8;

    const COUNT_INDEX: usize = 0;
    const OFFSET_INDEX: usize = 1;
    const LOCK_INDEX: usize = 2;

    fn new(ptr: *mut u8) -> Self {
        unsafe {
            let ptr = ptr as *mut usize;
            std::ptr::write_unaligned(ptr.add(Self::COUNT_INDEX), 0);
            std::ptr::write_unaligned(ptr.add(Self::OFFSET_INDEX), Self::START_OFFSET);
            std::ptr::write_unaligned(ptr.add(Self::LOCK_INDEX), 0);
        }

        Self {
            ptr: ptr as *const u8,
        }
    }

    fn count_failures(&self) -> usize {
        unsafe { *(self.ptr as *const usize).add(Self::COUNT_INDEX) }
    }

    fn wait_for_child(&self, sigchld: Arc<AtomicBool>) -> ChildProcessMsg {
        use std::sync::atomic::Ordering;
        let ptr = self.ptr as *const u32;
        let atomic_ptr: *const AtomicU32 = unsafe { ptr.add(5).cast() };
        let atomic = unsafe { &*atomic_ptr };

        loop {
            if sigchld.load(Ordering::Relaxed) {
                break ChildProcessMsg::Terminate;
            }

            match atomic.load(Ordering::Acquire) {
                0 => std::hint::spin_loop(),
                1 => break ChildProcessMsg::Expect,
                n => internal_error!("invalid atomic value set by the child: {n:#x}"),
            }
        }
    }

    fn reset(&mut self) {
        unsafe {
            let ptr = self.ptr as *mut usize;
            std::ptr::write_unaligned(ptr.add(Self::COUNT_INDEX), 0);
            std::ptr::write_unaligned(ptr.add(Self::OFFSET_INDEX), Self::START_OFFSET);
            std::ptr::write_unaligned(ptr.add(Self::LOCK_INDEX), 0);
        }
    }
}

pub enum ChildProcessMsg {
    Expect = 1,
    Terminate = 2,
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

        // skip to frame
        let start_offset = offset + 8 + 4;

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
}

pub fn expect_mono_module_to_dylib<'a>(
    arena: &'a Bump,
    target: Target,
    loaded: MonomorphizedModule<'a>,
    opt_level: OptLevel,
    mode: LlvmBackendMode,
) -> Result<
    (
        libloading::Library,
        MutMap<ModuleId, ExpectFunctions<'a>>,
        STLayoutInterner<'a>,
    ),
    libloading::Error,
> {
    let MonomorphizedModule {
        toplevel_expects,
        procedures,
        interns,
        layout_interner,
        ..
    } = loaded;

    let context = Context::create();
    let builder = context.create_builder();
    let module = arena.alloc(roc_gen_llvm::llvm::build::module_from_builtins(
        target, &context, "",
    ));

    let module = arena.alloc(module);

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
        target,
        mode,
        // important! we don't want any procedures to get the C calling convention
        exposed_to_host: MutSet::default(),
    };

    // Add roc_alloc, roc_realloc, and roc_dealloc, since the repl has no
    // platform to provide them.
    add_default_roc_externs(&env);

    let expects_symbols = toplevel_expects
        .iter()
        .map(|(module_id, expects)| {
            (
                *module_id,
                bumpalo::collections::Vec::from_iter_in(expects.pure.keys().copied(), env.arena),
            )
        })
        .collect();

    let expect_names = roc_gen_llvm::llvm::build::build_procedures_expose_expects(
        &env,
        &layout_interner,
        opt_level,
        expects_symbols,
        procedures,
    );

    let mut modules_expects: MutMap<ModuleId, ExpectFunctions> = MutMap::default();

    for (module_id, expects) in toplevel_expects.into_iter() {
        let expect_names = expect_names.get(&module_id).unwrap();

        let expects_pure =
            bumpalo::collections::Vec::from_iter_in(
                expects.pure.into_iter().zip(expect_names.iter()).map(
                    |((symbol, region), name)| ToplevelExpect {
                        symbol,
                        region,
                        name,
                    },
                ),
                env.arena,
            );

        let expect_funs = ExpectFunctions { pure: expects_pure };

        modules_expects.insert(module_id, expect_funs);
    }

    let emit_debug_info = true;
    let ll_file_path = std::env::temp_dir().join("test.ll");
    roc_build::llvm_passes::optimize_llvm_ir(
        &env,
        target,
        opt_level,
        emit_debug_info,
        &ll_file_path,
    );

    if let Ok(path) = std::env::var("ROC_DEBUG_LLVM") {
        env.module.print_to_file(path).unwrap();
    }

    llvm_module_to_dylib(env.module, target, opt_level)
        .map(|dy_lib| (dy_lib, modules_expects, layout_interner))
}
