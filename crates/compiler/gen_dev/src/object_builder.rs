use crate::generic64::{aarch64, new_backend_64bit, x86_64};
use crate::{Backend, Env, Relocation};
use bumpalo::collections::Vec;
use object::write::{self, SectionId, SymbolId};
use object::write::{Object, StandardSection, StandardSegment, Symbol, SymbolSection};
use object::{
    Architecture, BinaryFormat, Endianness, RelocationEncoding, RelocationKind, SectionKind,
    SymbolFlags, SymbolKind, SymbolScope,
};
use roc_collections::all::MutMap;
use roc_error_macros::internal_error;
use roc_module::symbol;
use roc_module::symbol::Interns;
use roc_mono::ir::{Call, CallSpecId, Expr, UpdateModeId};
use roc_mono::ir::{Proc, ProcLayout, Stmt};
use roc_mono::layout::{LambdaName, Layout, LayoutIds, LayoutInterner, Niche, STLayoutInterner};
use roc_target::TargetInfo;
use target_lexicon::{Architecture as TargetArch, BinaryFormat as TargetBF, Triple};

// This is used by some code below which is currently commented out.
// See that code for more details!
// const VERSION: &str = env!("CARGO_PKG_VERSION");

/// build_module is the high level builder/delegator.
/// It takes the request to build a module and output the object file for the module.
pub fn build_module<'a, 'r>(
    env: &'r Env<'a>,
    interns: &'r mut Interns,
    layout_interner: &'r mut STLayoutInterner<'a>,
    target: &Triple,
    procedures: MutMap<(symbol::Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Object<'a> {
    match target {
        Triple {
            architecture: TargetArch::X86_64,
            binary_format: TargetBF::Elf,
            ..
        } if cfg!(feature = "target-x86_64") => {
            let backend = new_backend_64bit::<
                x86_64::X86_64GeneralReg,
                x86_64::X86_64FloatReg,
                x86_64::X86_64Assembler,
                x86_64::X86_64SystemV,
            >(env, TargetInfo::default_x86_64(), interns, layout_interner);
            // Newer version of `ld` require `.note.GNU-stack` for security reasons.
            // It specifies that we will not execute code stored on the stack.
            let mut object =
                Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
            object.add_section(
                vec![],
                b".note.GNU-stack".to_vec(),
                SectionKind::Elf(object::elf::SHT_PROGBITS),
            );
            build_object(procedures, backend, object)
        }
        Triple {
            architecture: TargetArch::X86_64,
            binary_format: TargetBF::Macho,
            ..
        } if cfg!(feature = "target-x86_64") => {
            let backend = new_backend_64bit::<
                x86_64::X86_64GeneralReg,
                x86_64::X86_64FloatReg,
                x86_64::X86_64Assembler,
                x86_64::X86_64SystemV,
            >(env, TargetInfo::default_x86_64(), interns, layout_interner);
            build_object(
                procedures,
                backend,
                Object::new(
                    BinaryFormat::MachO,
                    Architecture::X86_64,
                    Endianness::Little,
                ),
            )
        }
        Triple {
            architecture: TargetArch::Aarch64(_),
            binary_format: TargetBF::Elf,
            ..
        } if cfg!(feature = "target-aarch64") => {
            let backend =
                new_backend_64bit::<
                    aarch64::AArch64GeneralReg,
                    aarch64::AArch64FloatReg,
                    aarch64::AArch64Assembler,
                    aarch64::AArch64Call,
                >(env, TargetInfo::default_aarch64(), interns, layout_interner);
            build_object(
                procedures,
                backend,
                Object::new(BinaryFormat::Elf, Architecture::Aarch64, Endianness::Little),
            )
        }
        Triple {
            architecture: TargetArch::Aarch64(_),
            binary_format: TargetBF::Macho,
            ..
        } if cfg!(feature = "target-aarch64") => {
            let backend =
                new_backend_64bit::<
                    aarch64::AArch64GeneralReg,
                    aarch64::AArch64FloatReg,
                    aarch64::AArch64Assembler,
                    aarch64::AArch64Call,
                >(env, TargetInfo::default_aarch64(), interns, layout_interner);
            build_object(
                procedures,
                backend,
                Object::new(
                    BinaryFormat::MachO,
                    Architecture::Aarch64,
                    Endianness::Little,
                ),
            )
        }
        x => unimplemented!("the target, {:?}", x),
    }
}

fn define_setlongjmp_buffer(output: &mut Object) -> SymbolId {
    let bss_section = output.section_id(StandardSection::Data);

    // 8 registers + 3 words for a RocStr
    const SIZE: usize = (8 + 3) * core::mem::size_of::<u64>();

    let symbol = Symbol {
        name: b"setlongjmp_buffer".to_vec(),
        value: 0,
        size: SIZE as u64,
        kind: SymbolKind::Data,
        scope: SymbolScope::Dynamic,
        weak: false,
        section: SymbolSection::Section(bss_section),
        flags: SymbolFlags::None,
    };

    let symbol_id = output.add_symbol(symbol);
    output.add_symbol_data(symbol_id, bss_section, &[0x00; SIZE], 8);

    symbol_id
}

fn generate_setjmp<'a, B: Backend<'a>>(backend: &mut B, output: &mut Object) {
    let text_section = output.section_id(StandardSection::Text);
    let proc_symbol = Symbol {
        name: b"roc_setjmp".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Dynamic,
        weak: false,
        section: SymbolSection::Section(text_section),
        flags: SymbolFlags::None,
    };
    let proc_id = output.add_symbol(proc_symbol);
    let proc_data = backend.build_roc_setjmp();

    output.add_symbol_data(proc_id, text_section, proc_data, 16);
}

fn generate_longjmp<'a, B: Backend<'a>>(backend: &mut B, output: &mut Object) {
    let text_section = output.section_id(StandardSection::Text);
    let proc_symbol = Symbol {
        name: b"roc_longjmp".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Dynamic,
        weak: false,
        section: SymbolSection::Section(text_section),
        flags: SymbolFlags::None,
    };
    let proc_id = output.add_symbol(proc_symbol);
    let proc_data = backend.build_roc_longjmp();

    output.add_symbol_data(proc_id, text_section, proc_data, 16);
}

// a roc_panic to be used in tests; relies on setjmp/longjmp
fn generate_roc_panic<'a, B: Backend<'a>>(backend: &mut B, output: &mut Object) {
    let text_section = output.section_id(StandardSection::Text);
    let proc_symbol = Symbol {
        name: b"roc_panic".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Dynamic,
        weak: false,
        section: SymbolSection::Section(text_section),
        flags: SymbolFlags::None,
    };
    let proc_id = output.add_symbol(proc_symbol);
    let (proc_data, relocs) = backend.build_roc_panic();

    let proc_offset = output.add_symbol_data(proc_id, text_section, proc_data, 16);

    for r in relocs {
        let relocation = match r {
            Relocation::LocalData { offset, .. } => unreachable!(),
            Relocation::LinkedFunction { offset, .. } => unreachable!(),
            Relocation::LinkedData { offset, name } => {
                if let Some(sym_id) = output.symbol_id(name.as_bytes()) {
                    write::Relocation {
                        offset: offset + proc_offset,
                        size: 32,
                        kind: RelocationKind::GotRelative,
                        encoding: RelocationEncoding::Generic,
                        symbol: sym_id,
                        addend: -4,
                    }
                } else {
                    internal_error!("failed to find data symbol for {:?}", name);
                }
            }
            Relocation::JmpToReturn { offset, .. } => unreachable!(),
        };

        output.add_relocation(text_section, relocation).unwrap();
    }
}

fn generate_wrapper<'a, B: Backend<'a>>(
    backend: &mut B,
    output: &mut Object,
    wrapper_name: String,
    wraps: String,
) {
    let text_section = output.section_id(StandardSection::Text);
    let proc_symbol = Symbol {
        name: wrapper_name.as_bytes().to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Dynamic,
        weak: false,
        section: SymbolSection::Section(text_section),
        flags: SymbolFlags::None,
    };
    let proc_id = output.add_symbol(proc_symbol);
    let (proc_data, offset) = backend.build_wrapped_jmp();
    let proc_offset = output.add_symbol_data(proc_id, text_section, proc_data, 16);

    let name = wraps.as_str().as_bytes();
    // If the symbol is an undefined zig builtin, we need to add it here.
    let symbol = Symbol {
        name: name.to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Dynamic,
        weak: true,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    };
    output.add_symbol(symbol);
    if let Some(sym_id) = output.symbol_id(name) {
        let reloc = write::Relocation {
            offset: offset + proc_offset,
            size: 32,
            kind: RelocationKind::PltRelative,
            encoding: RelocationEncoding::X86Branch,
            symbol: sym_id,
            addend: -4,
        };

        match output.add_relocation(text_section, reloc) {
            Ok(obj) => obj,
            Err(e) => internal_error!("{:?}", e),
        }
    } else {
        internal_error!("failed to find fn symbol for {:?}", wraps);
    }
}

fn build_object<'a, B: Backend<'a>>(
    procedures: MutMap<(symbol::Symbol, ProcLayout<'a>), Proc<'a>>,
    mut backend: B,
    mut output: Object<'a>,
) -> Object<'a> {
    let data_section = output.section_id(StandardSection::Data);

    let arena = backend.env().arena;

    /*
    // Commented out because we couldn't figure out how to get it to work on mac - see https://github.com/roc-lang/roc/pull/1323
    let comment = output.add_section(vec![], b".comment".to_vec(), SectionKind::OtherString);
    output.append_section_data(
        comment,
        format!("\0roc dev backend version {} \0", VERSION).as_bytes(),
        1,
    );
    */

    define_setlongjmp_buffer(&mut output);

    generate_roc_panic(&mut backend, &mut output);
    generate_setjmp(&mut backend, &mut output);
    generate_longjmp(&mut backend, &mut output);

    if backend.env().mode.generate_allocators() {
        generate_wrapper(
            &mut backend,
            &mut output,
            "roc_alloc".into(),
            "malloc".into(),
        );
        generate_wrapper(
            &mut backend,
            &mut output,
            "roc_realloc".into(),
            "realloc".into(),
        );
        generate_wrapper(
            &mut backend,
            &mut output,
            "roc_dealloc".into(),
            "free".into(),
        );

        //        generate_wrapper(
        //            &mut backend,
        //            &mut output,
        //            "roc_panic".into(),
        //            "roc_builtins.utils.test_panic".into(),
        //        );

        // Extra symbols only required on unix systems.
        if matches!(output.format(), BinaryFormat::Elf | BinaryFormat::MachO) {
            generate_wrapper(
                &mut backend,
                &mut output,
                "roc_getppid".into(),
                "getppid".into(),
            );
            generate_wrapper(&mut backend, &mut output, "roc_mmap".into(), "mmap".into());
            generate_wrapper(
                &mut backend,
                &mut output,
                "roc_shm_open".into(),
                "shm_open".into(),
            );
        }
    }

    // Setup layout_ids for procedure calls.
    let mut layout_ids = LayoutIds::default();
    let mut procs = Vec::with_capacity_in(procedures.len(), arena);

    // Names and linker data for user procedures
    for ((sym, layout), proc) in procedures {
        debug_assert_eq!(sym, proc.name.name());

        if backend.env().exposed_to_host.contains(&sym) {
            let exposed_proc = build_exposed_proc(&mut backend, &proc);
            let exposed_generic_proc = build_exposed_generic_proc(&mut backend, &proc);

            //        ModuleId,
            //        &mut STLayoutInterner<'a>,
            //        &mut Interns,
            //        &mut CodeGenHelp<'a>,
            //        &mut Vec<'a, CallerProc<'a>>,

            let (module_id, layout_interner, interns, code_gen_help, _) =
                backend.module_interns_helpers_mut();

            let ident_ids = interns.all_ident_ids.get_mut(&module_id).unwrap();

            let test_helper = roc_mono::code_gen_help::test_helper(
                code_gen_help,
                ident_ids,
                layout_interner,
                &proc,
            );

            #[cfg(debug_assertions)]
            {
                let module_id = exposed_generic_proc.name.name().module_id();
                let ident_ids = backend
                    .interns_mut()
                    .all_ident_ids
                    .get_mut(&module_id)
                    .unwrap();
                module_id.register_debug_idents(ident_ids);
            }

            println!("{}", test_helper.to_pretty(backend.interner(), 200, true));

            build_proc_symbol(
                &mut output,
                &mut layout_ids,
                &mut procs,
                &mut backend,
                layout,
                test_helper,
                Exposed::TestMain,
            );

            build_proc_symbol(
                &mut output,
                &mut layout_ids,
                &mut procs,
                &mut backend,
                layout,
                exposed_proc,
                Exposed::Exposed,
            );

            build_proc_symbol(
                &mut output,
                &mut layout_ids,
                &mut procs,
                &mut backend,
                layout,
                exposed_generic_proc,
                Exposed::ExposedGeneric,
            );
        }

        build_proc_symbol(
            &mut output,
            &mut layout_ids,
            &mut procs,
            &mut backend,
            layout,
            proc,
            Exposed::NotExposed,
        )
    }

    // Build procedures from user code
    let mut relocations = bumpalo::vec![in arena];
    for (fn_name, section_id, proc_id, proc) in procs {
        build_proc(
            &mut output,
            &mut backend,
            &mut relocations,
            &mut layout_ids,
            data_section,
            fn_name,
            section_id,
            proc_id,
            proc,
        )
    }

    // Generate IR for specialized helper procs (refcounting & equality)
    let empty = bumpalo::collections::Vec::new_in(arena);
    let mut helper_symbols_and_layouts =
        std::mem::replace(backend.helper_proc_symbols_mut(), empty);

    let helper_procs = {
        let (module_id, _interner, interns, helper_proc_gen, caller_procs) =
            backend.module_interns_helpers_mut();

        let mut owned_caller_procs = bumpalo::collections::Vec::new_in(arena);
        std::mem::swap(caller_procs, &mut owned_caller_procs);

        let ident_ids = interns.all_ident_ids.get_mut(&module_id).unwrap();
        let mut helper_procs = helper_proc_gen.take_procs();

        for caller_proc in owned_caller_procs {
            helper_symbols_and_layouts.push((caller_proc.proc_symbol, caller_proc.proc_layout));
            helper_procs.push(caller_proc.proc);
        }

        if false {
            module_id.register_debug_idents(ident_ids);

            for p in &helper_procs {
                println!("{}", p.to_pretty(_interner, 200, true));
            }
        }

        helper_procs
    };

    let mut helper_names_symbols_procs = Vec::with_capacity_in(helper_procs.len(), arena);

    debug_assert_eq!(helper_symbols_and_layouts.len(), helper_procs.len());

    // Names and linker data for helpers
    for ((sym, layout), proc) in helper_symbols_and_layouts.into_iter().zip(helper_procs) {
        debug_assert_eq!(sym, proc.name.name());

        let fn_name = backend.lambda_name_to_string(
            LambdaName::no_niche(sym),
            layout.arguments.iter().copied(),
            None,
            layout.result,
        );

        if let Some(proc_id) = output.symbol_id(fn_name.as_bytes()) {
            if let SymbolSection::Section(section_id) = output.symbol(proc_id).section {
                helper_names_symbols_procs.push((fn_name, section_id, proc_id, proc));
                continue;
            }
        } else {
            // The symbol isn't defined yet and will just be used by other rc procs.
            let section_id = output.add_section(
                output.segment_name(StandardSegment::Text).to_vec(),
                format!(".text.{:x}", sym.as_u64()).as_bytes().to_vec(),
                SectionKind::Text,
            );

            let rc_symbol = Symbol {
                name: fn_name.as_bytes().to_vec(),
                value: 0,
                size: 0,
                kind: SymbolKind::Text,
                scope: SymbolScope::Linkage,
                weak: false,
                section: SymbolSection::Section(section_id),
                flags: SymbolFlags::None,
            };
            let proc_id = output.add_symbol(rc_symbol);
            helper_names_symbols_procs.push((fn_name, section_id, proc_id, proc));
            continue;
        }
        internal_error!("failed to create rc fn for symbol {:?}", sym);
    }

    // Build helpers
    for (fn_name, section_id, proc_id, proc) in helper_names_symbols_procs {
        build_proc(
            &mut output,
            &mut backend,
            &mut relocations,
            &mut layout_ids,
            data_section,
            fn_name,
            section_id,
            proc_id,
            proc,
        )
    }

    // Relocations for all procedures (user code & helpers)
    for (section_id, reloc) in relocations {
        match output.add_relocation(section_id, reloc) {
            Ok(obj) => obj,
            Err(e) => internal_error!("{:?}", e),
        }
    }
    output
}

fn build_exposed_proc<'a, B: Backend<'a>>(backend: &mut B, proc: &Proc<'a>) -> Proc<'a> {
    let arena = backend.env().arena;
    let interns = backend.interns();

    let sym = proc.name.name();
    let platform = sym.module_id();

    let fn_name = sym.as_str(interns).to_string();
    let generic_proc_name = backend.debug_symbol_in(platform, &fn_name);
    let s4 = backend.debug_symbol_in(platform, "s4");

    let call_args = bumpalo::collections::Vec::from_iter_in(proc.args.iter().map(|t| t.1), arena);
    let call_layouts =
        bumpalo::collections::Vec::from_iter_in(proc.args.iter().map(|t| t.0), arena);
    let call = Call {
        call_type: roc_mono::ir::CallType::ByName {
            name: proc.name,
            ret_layout: proc.ret_layout,
            arg_layouts: call_layouts.into_bump_slice(),
            specialization_id: CallSpecId::BACKEND_DUMMY,
        },
        arguments: call_args.into_bump_slice(),
    };

    let body = Stmt::Let(
        s4,
        Expr::Call(call),
        proc.ret_layout,
        arena.alloc(Stmt::Ret(s4)),
    );

    Proc {
        name: LambdaName::no_niche(generic_proc_name),
        args: proc.args,
        body,
        closure_data_layout: None,
        ret_layout: proc.ret_layout,
        is_self_recursive: roc_mono::ir::SelfRecursive::NotSelfRecursive,
        host_exposed_layouts: roc_mono::ir::HostExposedLayouts::NotHostExposed,
        is_erased: proc.is_erased,
    }
}

fn build_exposed_generic_proc<'a, B: Backend<'a>>(backend: &mut B, proc: &Proc<'a>) -> Proc<'a> {
    let arena = backend.env().arena;
    let interns = backend.interns();

    let sym = proc.name.name();
    let platform = sym.module_id();

    let fn_name = sym.as_str(interns).to_string();
    let generic_proc_name = backend.debug_symbol_in(platform, &fn_name);
    let arg_generic = backend.debug_symbol_in(platform, "arg_generic");

    let s1 = backend.debug_symbol_in(platform, "s1");
    let s2 = backend.debug_symbol_in(platform, "s2");
    let s3 = backend.debug_symbol_in(platform, "s3");

    let box_layout = backend
        .interner_mut()
        .insert_direct_no_semantic(roc_mono::layout::LayoutRepr::Ptr(proc.ret_layout));

    let mut args = bumpalo::collections::Vec::new_in(arena);
    args.extend(proc.args);
    args.push((box_layout, arg_generic));

    let call_args = bumpalo::collections::Vec::from_iter_in(proc.args.iter().map(|t| t.1), arena);
    let call_layouts =
        bumpalo::collections::Vec::from_iter_in(proc.args.iter().map(|t| t.0), arena);
    let call = Call {
        call_type: roc_mono::ir::CallType::ByName {
            name: proc.name,
            ret_layout: proc.ret_layout,
            arg_layouts: call_layouts.into_bump_slice(),
            specialization_id: CallSpecId::BACKEND_DUMMY,
        },
        arguments: call_args.into_bump_slice(),
    };

    let box_write = Call {
        call_type: roc_mono::ir::CallType::LowLevel {
            op: roc_module::low_level::LowLevel::PtrStore,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: arena.alloc([arg_generic, s1]),
    };

    let body = Stmt::Let(
        s1,
        Expr::Call(call),
        proc.ret_layout,
        arena.alloc(
            //
            Stmt::Let(
                s2,
                Expr::Call(box_write),
                box_layout,
                arena.alloc(
                    //
                    Stmt::Let(
                        s3,
                        Expr::Struct(&[]),
                        Layout::UNIT,
                        arena.alloc(
                            //
                            Stmt::Ret(s3),
                        ),
                    ),
                ),
            ),
        ),
    );

    Proc {
        name: LambdaName::no_niche(generic_proc_name),
        args: args.into_bump_slice(),
        body,
        closure_data_layout: None,
        ret_layout: roc_mono::layout::Layout::UNIT,
        is_self_recursive: roc_mono::ir::SelfRecursive::NotSelfRecursive,
        host_exposed_layouts: roc_mono::ir::HostExposedLayouts::NotHostExposed,
        is_erased: proc.is_erased,
    }
}

#[allow(clippy::enum_variant_names)]
enum Exposed {
    ExposedGeneric,
    Exposed,
    NotExposed,
    TestMain,
}

fn build_proc_symbol<'a, B: Backend<'a>>(
    output: &mut Object<'a>,
    layout_ids: &mut LayoutIds<'a>,
    procs: &mut Vec<'a, (String, SectionId, SymbolId, Proc<'a>)>,
    backend: &mut B,
    layout: ProcLayout<'a>,
    proc: Proc<'a>,
    exposed: Exposed,
) {
    let sym = proc.name.name();

    let section_id = output.add_section(
        output.segment_name(StandardSegment::Text).to_vec(),
        format!(".text.{:x}", sym.as_u64()).as_bytes().to_vec(),
        SectionKind::Text,
    );

    let fn_name = match exposed {
        Exposed::ExposedGeneric => layout_ids
            .get_toplevel(sym, &layout)
            .to_exposed_generic_symbol_string(sym, backend.interns()),
        Exposed::Exposed => layout_ids
            .get_toplevel(sym, &layout)
            .to_exposed_symbol_string(sym, backend.interns()),
        Exposed::NotExposed => backend.lambda_name_to_string(
            proc.name,
            layout.arguments.iter().copied(),
            None,
            layout.result,
        ),
        Exposed::TestMain => String::from("test_main"),
    };

    let proc_symbol = Symbol {
        name: fn_name.as_bytes().to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        // TODO: Depending on whether we are building a static or dynamic lib, this should change.
        // We should use Dynamic -> anyone, Linkage -> static link, Compilation -> this module only.
        scope: match exposed {
            Exposed::ExposedGeneric | Exposed::Exposed | Exposed::TestMain => SymbolScope::Dynamic,
            Exposed::NotExposed => SymbolScope::Linkage,
        },
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    };
    let proc_id = output.add_symbol(proc_symbol);
    procs.push((fn_name, section_id, proc_id, proc));
}

#[allow(clippy::too_many_arguments)]
fn build_proc<'a, B: Backend<'a>>(
    output: &mut Object,
    backend: &mut B,
    relocations: &mut Vec<'a, (SectionId, object::write::Relocation)>,
    layout_ids: &mut LayoutIds<'a>,
    data_section: SectionId,
    fn_name: String,
    section_id: SectionId,
    proc_id: SymbolId,
    proc: Proc<'a>,
) {
    let mut local_data_index = 0;
    let (proc_data, relocs, rc_proc_names) = backend.build_proc(proc, layout_ids);
    let proc_offset = output.add_symbol_data(proc_id, section_id, &proc_data, 16);
    for reloc in relocs.iter() {
        let elfreloc = match reloc {
            Relocation::LocalData { offset, data } => {
                let data_symbol = write::Symbol {
                    name: format!("{fn_name}.data{local_data_index}")
                        .as_bytes()
                        .to_vec(),
                    value: 0,
                    size: 0,
                    kind: SymbolKind::Data,
                    scope: SymbolScope::Compilation,
                    weak: false,
                    section: SymbolSection::Section(data_section),
                    flags: SymbolFlags::None,
                };
                local_data_index += 1;
                let data_id = output.add_symbol(data_symbol);
                output.add_symbol_data(data_id, data_section, data, 4);
                write::Relocation {
                    offset: offset + proc_offset,
                    size: 32,
                    kind: RelocationKind::Relative,
                    encoding: RelocationEncoding::Generic,
                    symbol: data_id,
                    addend: -4,
                }
            }
            Relocation::LinkedData { offset, name } => {
                if let Some(sym_id) = output.symbol_id(name.as_bytes()) {
                    write::Relocation {
                        offset: offset + proc_offset,
                        size: 32,
                        kind: RelocationKind::GotRelative,
                        encoding: RelocationEncoding::Generic,
                        symbol: sym_id,
                        addend: -4,
                    }
                } else {
                    internal_error!("failed to find data symbol for {:?}", name);
                }
            }
            Relocation::LinkedFunction { offset, name } => {
                // If the symbol is an undefined roc function, we need to add it here.
                if output.symbol_id(name.as_bytes()).is_none() && name.starts_with("roc_") {
                    let builtin_symbol = Symbol {
                        name: name.as_bytes().to_vec(),
                        value: 0,
                        size: 0,
                        kind: SymbolKind::Text,
                        scope: SymbolScope::Linkage,
                        weak: false,
                        section: SymbolSection::Undefined,
                        flags: SymbolFlags::None,
                    };
                    output.add_symbol(builtin_symbol);
                }

                // If the symbol is an undefined reference counting procedure, we need to add it here.
                if output.symbol_id(name.as_bytes()).is_none() {
                    for (sym, rc_name) in rc_proc_names.iter() {
                        if name == rc_name {
                            let section_id = output.add_section(
                                output.segment_name(StandardSegment::Text).to_vec(),
                                format!(".text.{:x}", sym.as_u64()).as_bytes().to_vec(),
                                SectionKind::Text,
                            );

                            let rc_symbol = Symbol {
                                name: name.as_bytes().to_vec(),
                                value: 0,
                                size: 0,
                                kind: SymbolKind::Text,
                                scope: SymbolScope::Linkage,
                                weak: false,
                                section: SymbolSection::Section(section_id),
                                flags: SymbolFlags::None,
                            };
                            output.add_symbol(rc_symbol);
                        }
                    }
                }

                if let Some(sym_id) = output.symbol_id(name.as_bytes()) {
                    write::Relocation {
                        offset: offset + proc_offset,
                        size: 32,
                        kind: RelocationKind::PltRelative,
                        encoding: RelocationEncoding::X86Branch,
                        symbol: sym_id,
                        addend: -4,
                    }
                } else {
                    internal_error!("failed to find fn symbol for {:?}", name);
                }
            }
            Relocation::JmpToReturn { .. } => unreachable!(),
        };
        relocations.push((section_id, elfreloc));
    }
}
