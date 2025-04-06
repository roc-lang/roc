// https://github.com/morphic-lang/morphic_lib/issues/19
#![allow(clippy::result_large_err)]

use bumpalo::Bump;
use morphic_lib::TypeContext;
use morphic_lib::{
    BlockExpr, BlockId, CalleeSpecVar, ConstDefBuilder, ConstName, EntryPointName, ExprContext,
    FuncDef, FuncDefBuilder, FuncName, ModDefBuilder, ModName, ProgramBuilder, Result,
    TypeDefBuilder, TypeId, TypeName, UpdateModeVar, ValueId,
};
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::internal_error;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;

use roc_mono::ir::{
    Call, CallType, EntryPoint, ErasedField, Expr, HigherOrderLowLevel, HostExposedLambdaSet,
    ListLiteralElement, Literal, ModifyRc, OptLevel, Proc, ProcLayout, SingleEntryPoint, Stmt,
};
use roc_mono::layout::{
    Builtin, InLayout, Layout, LayoutInterner, LayoutRepr, Niche, RawFunctionLayout,
    STLayoutInterner, UnionLayout,
};

// just using one module for now
pub const MOD_APP: ModName = ModName(b"UserApp");

pub const STATIC_STR_NAME: ConstName = ConstName(&Symbol::STR_ALIAS_ANALYSIS_STATIC.to_ne_bytes());
pub const STATIC_LIST_NAME: ConstName = ConstName(b"THIS IS A STATIC LIST");

const DEFAULT_ENTRY_POINT_NAME: &[u8] = b"main_for_host";

pub fn func_name_bytes(proc: &Proc) -> [u8; SIZE] {
    let bytes = func_name_bytes_help(
        proc.name.name(),
        proc.args.iter().map(|x| x.0),
        proc.name.niche(),
        proc.ret_layout,
    );
    bytes
}

#[inline(always)]
fn debug() -> bool {
    use roc_debug_flags::dbg_do;

    #[cfg(debug_assertions)]
    use roc_debug_flags::ROC_DEBUG_ALIAS_ANALYSIS;

    dbg_do!(ROC_DEBUG_ALIAS_ANALYSIS, {
        return true;
    });
    false
}

const SIZE: usize = 16;

#[derive(Debug, Clone, Copy, Hash)]
struct TagUnionId(u64);

fn recursive_tag_union_name_bytes(union_layout: &UnionLayout) -> TagUnionId {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hash;
    use std::hash::Hasher;

    let mut hasher = DefaultHasher::new();
    union_layout.hash(&mut hasher);

    TagUnionId(hasher.finish())
}

impl TagUnionId {
    const fn as_bytes(&self) -> [u8; 8] {
        self.0.to_ne_bytes()
    }
}

pub fn func_name_bytes_help<'a, I>(
    symbol: Symbol,
    argument_layouts: I,
    niche: Niche<'a>,
    return_layout: InLayout<'a>,
) -> [u8; SIZE]
where
    I: IntoIterator<Item = InLayout<'a>>,
{
    let mut name_bytes = [0u8; SIZE];

    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hash;
    use std::hash::Hasher;

    let layout_hash = {
        let mut hasher = DefaultHasher::new();

        for layout in argument_layouts {
            layout.hash(&mut hasher);
        }

        niche.hash(&mut hasher);

        return_layout.hash(&mut hasher);

        hasher.finish()
    };

    let sbytes = symbol.to_ne_bytes();
    let lbytes = layout_hash.to_ne_bytes();

    let it = sbytes
        .iter()
        .chain(lbytes.iter())
        .zip(name_bytes.iter_mut());

    for (source, target) in it {
        *target = *source;
    }

    if debug() {
        for (i, c) in (format!("{symbol:?}")).chars().take(25).enumerate() {
            name_bytes[25 + i] = c as u8;
        }
    }

    name_bytes
}

fn bytes_as_ascii(bytes: &[u8]) -> String {
    use std::fmt::Write;

    let mut buf = String::new();

    for byte in bytes {
        write!(buf, "{byte:02X}").unwrap();
    }

    buf
}

pub fn spec_program<'a, 'r, I1, I2>(
    arena: &'a Bump,
    interner: &'r STLayoutInterner<'a>,
    opt_level: OptLevel,
    entry_point: roc_mono::ir::EntryPoint<'a>,
    procs: I1,
    hels: I2,
) -> Result<morphic_lib::Solutions>
where
    I1: Iterator<Item = &'r Proc<'a>>,
    I2: Iterator<Item = &'r HostExposedLambdaSet<'a>>,
{
    let mut entry_point_names = bumpalo::vec![in arena;];
    let main_module = {
        let mut m = ModDefBuilder::new();

        // a const that models all static strings
        let static_str_def = {
            let mut cbuilder = ConstDefBuilder::new();
            let block = cbuilder.add_block();
            let cell = cbuilder.add_new_heap_cell(block)?;
            let value_id = cbuilder.add_make_tuple(block, &[cell])?;
            let root = BlockExpr(block, value_id);
            let str_type_id = str_type(&mut cbuilder)?;

            cbuilder.build(str_type_id, root)?
        };
        m.add_const(STATIC_STR_NAME, static_str_def)?;

        // a const that models all static lists
        let static_list_def = {
            let mut cbuilder = ConstDefBuilder::new();
            let block = cbuilder.add_block();
            let cell = cbuilder.add_new_heap_cell(block)?;

            let unit_type = cbuilder.add_tuple_type(&[])?;
            let bag = cbuilder.add_empty_bag(block, unit_type)?;
            let value_id = cbuilder.add_make_tuple(block, &[cell, bag])?;
            let root = BlockExpr(block, value_id);
            let list_type_id = static_list_type(&mut cbuilder)?;

            cbuilder.build(list_type_id, root)?
        };
        m.add_const(STATIC_LIST_NAME, static_list_def)?;

        let mut type_definitions = MutSet::default();
        let mut host_exposed_functions = Vec::new();
        let mut erased_functions = Vec::new();

        for hels in hels {
            match hels.raw_function_layout {
                RawFunctionLayout::Function(_, _, _) => {
                    let it = hels.proc_layout.arguments.iter().copied();
                    let bytes =
                        func_name_bytes_help(hels.symbol, it, Niche::NONE, hels.proc_layout.result);

                    host_exposed_functions.push((bytes, hels.proc_layout.arguments));
                }
                RawFunctionLayout::ErasedFunction(..) => {
                    let it = hels.proc_layout.arguments.iter().copied();
                    let bytes =
                        func_name_bytes_help(hels.symbol, it, Niche::NONE, hels.proc_layout.result);

                    host_exposed_functions.push((bytes, hels.proc_layout.arguments));
                }
                RawFunctionLayout::ZeroArgumentThunk(_) => {
                    let bytes =
                        func_name_bytes_help(hels.symbol, [], Niche::NONE, hels.proc_layout.result);

                    host_exposed_functions.push((bytes, hels.proc_layout.arguments));
                }
            }
        }

        // all other functions
        for proc in procs {
            let bytes = func_name_bytes(proc);
            let func_name = FuncName(&bytes);

            if debug() {
                eprintln!(
                    "{:?}: {:?} with {:?} args",
                    proc.name,
                    bytes_as_ascii(&bytes),
                    (proc.args, proc.ret_layout),
                );
            }

            let (spec, type_names) = proc_spec(arena, interner, proc)?;

            if proc.is_erased {
                let args = &*arena.alloc_slice_fill_iter(proc.args.iter().map(|(lay, _)| *lay));
                erased_functions.push((bytes, args));
            }

            type_definitions.extend(type_names);

            m.add_func(func_name, spec)?;
        }

        match entry_point {
            EntryPoint::Program(entry_points) => {
                for SingleEntryPoint {
                    name: entry_point_name,
                    symbol: entry_point_symbol,
                    layout: entry_point_layout,
                } in entry_points
                {
                    let roc_main_bytes = func_name_bytes_help(
                        *entry_point_symbol,
                        entry_point_layout.arguments.iter().copied(),
                        Niche::NONE,
                        entry_point_layout.result,
                    );
                    let roc_main = FuncName(&roc_main_bytes);

                    let mut env = Env::new();

                    let entry_point_function = build_entry_point(
                        &mut env,
                        interner,
                        *entry_point_layout,
                        Some(roc_main),
                        &host_exposed_functions,
                        &erased_functions,
                    )?;

                    type_definitions.extend(env.type_names);

                    entry_point_names.push(entry_point_name.as_bytes());
                    let entry_point_name = FuncName(entry_point_name.as_bytes());
                    m.add_func(entry_point_name, entry_point_function)?;
                }
            }
            EntryPoint::Expects { symbols } => {
                // construct a big pattern match picking one of the expects at random
                let layout: ProcLayout<'a> = ProcLayout {
                    arguments: &[],
                    result: Layout::UNIT,
                    niche: Niche::NONE,
                };

                let host_exposed: Vec<_> = symbols
                    .iter()
                    .map(|symbol| {
                        (
                            func_name_bytes_help(*symbol, [], Niche::NONE, layout.result),
                            [].as_slice(),
                        )
                    })
                    .collect();

                let mut env = Env::new();
                let entry_point_function = build_entry_point(
                    &mut env,
                    interner,
                    layout,
                    None,
                    &host_exposed,
                    &erased_functions,
                )?;

                type_definitions.extend(env.type_names);

                entry_point_names.push(DEFAULT_ENTRY_POINT_NAME);
                let entry_point_name = FuncName(DEFAULT_ENTRY_POINT_NAME);
                m.add_func(entry_point_name, entry_point_function)?;
            }
        }

        for union_layout in type_definitions {
            let type_name_bytes = recursive_tag_union_name_bytes(&union_layout).as_bytes();
            let type_name = TypeName(&type_name_bytes);

            let mut builder = TypeDefBuilder::new();

            let mut env = Env::new();
            let variant_types =
                recursive_variant_types(&mut env, &mut builder, interner, &union_layout)?;

            // FIXME: dropping additional env.type_names here!

            let root_type = if let UnionLayout::NonNullableUnwrapped(_) = union_layout {
                debug_assert_eq!(variant_types.len(), 1);
                variant_types[0]
            } else {
                let cell_type = builder.add_heap_cell_type();
                let data_type = builder.add_union_type(&variant_types)?;

                builder.add_tuple_type(&[cell_type, data_type])?
            };

            let type_def = builder.build(root_type)?;

            m.add_named_type(type_name, type_def)?;
        }

        m.build()?
    };

    let program = {
        let mut p = ProgramBuilder::new();
        p.add_mod(MOD_APP, main_module)?;

        for entry_point_name in entry_point_names {
            p.add_entry_point(
                EntryPointName(entry_point_name),
                MOD_APP,
                FuncName(entry_point_name),
            )?;
        }

        p.build()?
    };

    if debug() {
        eprintln!("{}", program.to_source_string());
    }

    match opt_level {
        OptLevel::Development | OptLevel::Normal => morphic_lib::solve_trivial(program),
        // TODO(#7367): Change this back to `morphic_lib::solve`.
        // For now, using solve_trivial to avoid bug with loops.
        // Note: when disabling this, there was not much of a change in performance.
        // Notably, NQueens was about 5% slower. False interpreter was 0-5% faster (depending on input).
        // c_fold and derive saw minor gains ~1.5%. r_b_tree_ck saw a big gain of ~4%.
        // This feels wrong, morphic should not really be able to slow down code.
        // Likely, noise or the bug and wrong inplace mutation lead to these perf changes.
        // When re-enabling this, we should analysis the perf and inplace mutations of a few apps.
        // It might be the case that our current benchmarks just aren't affected by morphic much.
        OptLevel::Optimize | OptLevel::Size => morphic_lib::solve_trivial(program),
    }
}

/// if you want an "escape hatch" which allows you construct "best-case scenario" values
/// of an arbitrary type in much the same way that 'unknown_with' allows you to construct
/// "worst-case scenario" values of an arbitrary type, you can use the following terrible hack:
/// use 'add_make_union' to construct an instance of variant 0 of a union type 'union {(), your_type}',
/// and then use 'add_unwrap_union' to extract variant 1 from the value you just constructed.
/// In the current implementation (but not necessarily in future versions),
/// I can promise this will effectively give you a value of type 'your_type'
/// all of whose heap cells are considered unique and mutable.
fn terrible_hack(builder: &mut FuncDefBuilder, block: BlockId, type_id: TypeId) -> Result<ValueId> {
    let variant_types = vec![builder.add_tuple_type(&[])?, type_id];
    let unit = builder.add_make_tuple(block, &[])?;
    let value = builder.add_make_union(block, &variant_types, 0, unit)?;

    builder.add_unwrap_union(block, value, 1)
}

fn build_entry_point<'a>(
    env: &mut Env<'a>,
    interner: &STLayoutInterner<'a>,
    layout: roc_mono::ir::ProcLayout<'a>,
    entry_point_function: Option<FuncName>,
    host_exposed_functions: &[([u8; SIZE], &'a [InLayout<'a>])],
    erased_functions: &[([u8; SIZE], &'a [InLayout<'a>])],
) -> Result<FuncDef> {
    let mut builder = FuncDefBuilder::new();
    let outer_block = builder.add_block();

    let mut cases = Vec::new();

    if let Some(entry_point_function) = entry_point_function {
        let block = builder.add_block();

        // to the modelling language, the arguments appear out of thin air
        let argument_type = build_tuple_type(env, &mut builder, interner, layout.arguments)?;

        // does not make any assumptions about the input
        // let argument = builder.add_unknown_with(block, &[], argument_type)?;

        // assumes the input can be updated in-place
        let argument = terrible_hack(&mut builder, block, argument_type)?;

        let name_bytes = [0; 16];
        let spec_var = CalleeSpecVar(&name_bytes);
        let result = builder.add_call(block, spec_var, MOD_APP, entry_point_function, argument)?;

        // to the modelling language, the result disappears into the void
        let unit_type = builder.add_tuple_type(&[])?;
        let unit_value = builder.add_unknown_with(block, &[result], unit_type)?;

        cases.push(BlockExpr(block, unit_value));
    }

    // add fake calls to host-exposed functions so they are specialized
    for (name_bytes, layouts) in host_exposed_functions.iter().chain(erased_functions) {
        let host_exposed_func_name = FuncName(name_bytes);

        if Some(host_exposed_func_name) == entry_point_function {
            continue;
        }

        let block = builder.add_block();

        let struct_layout = LayoutRepr::struct_(layouts);
        let type_id = layout_spec(env, &mut builder, interner, struct_layout)?;

        let argument = builder.add_unknown_with(block, &[], type_id)?;

        let spec_var = CalleeSpecVar(name_bytes);
        let result =
            builder.add_call(block, spec_var, MOD_APP, host_exposed_func_name, argument)?;

        let unit_type = builder.add_tuple_type(&[])?;
        let unit_value = builder.add_unknown_with(block, &[result], unit_type)?;

        cases.push(BlockExpr(block, unit_value));
    }

    let unit_type = builder.add_tuple_type(&[])?;
    let unit_value = if cases.is_empty() {
        builder.add_make_tuple(outer_block, &[])?
    } else {
        builder.add_choice(outer_block, &cases)?
    };

    let root = BlockExpr(outer_block, unit_value);
    let spec = builder.build(unit_type, unit_type, root)?;

    Ok(spec)
}

fn proc_spec<'a>(
    arena: &'a Bump,
    interner: &STLayoutInterner<'a>,
    proc: &Proc<'a>,
) -> Result<(FuncDef, MutSet<UnionLayout<'a>>)> {
    let mut builder = FuncDefBuilder::new();
    let mut env = Env::new();

    let block = builder.add_block();

    // introduce the arguments
    let mut argument_layouts = bumpalo::collections::Vec::with_capacity_in(proc.args.len(), arena);
    for (i, (layout, symbol)) in proc.args.iter().enumerate() {
        let value_id = builder.add_get_tuple_field(block, builder.get_argument(), i as u32)?;
        env.symbols.insert(*symbol, value_id);

        argument_layouts.push(*layout);
    }

    let value_id = stmt_spec(
        &mut builder,
        interner,
        &mut env,
        block,
        proc.ret_layout,
        &proc.body,
    )?;

    let root = BlockExpr(block, value_id);
    let args_struct_layout = LayoutRepr::struct_(argument_layouts.into_bump_slice());
    let arg_type_id = layout_spec(&mut env, &mut builder, interner, args_struct_layout)?;
    let ret_type_id = layout_spec(
        &mut env,
        &mut builder,
        interner,
        interner.get_repr(proc.ret_layout),
    )?;

    let spec = builder.build(arg_type_id, ret_type_id, root)?;

    Ok((spec, env.type_names))
}

struct Env<'a> {
    symbols: MutMap<Symbol, ValueId>,
    join_points: MutMap<roc_mono::ir::JoinPointId, morphic_lib::ContinuationId>,
    type_names: MutSet<UnionLayout<'a>>,
}

impl<'a> Env<'a> {
    fn new() -> Self {
        Self {
            symbols: Default::default(),
            join_points: Default::default(),
            type_names: Default::default(),
        }
    }
}

fn apply_refcount_operation(
    builder: &mut FuncDefBuilder,
    env: &mut Env<'_>,
    block: BlockId,
    modify_rc: &ModifyRc,
) -> Result<()> {
    match modify_rc {
        ModifyRc::Inc(symbol, _) => {
            let argument = env.symbols[symbol];

            // a recursive touch is never worse for optimizations than a normal touch
            // and a bit more permissive in its type
            builder.add_recursive_touch(block, argument)?;
        }

        ModifyRc::Dec(symbol) => {
            let argument = env.symbols[symbol];
            builder.add_recursive_touch(block, argument)?;
        }
        ModifyRc::DecRef(symbol) => {
            // this is almost certainly suboptimal, but not incorrect
            let argument = env.symbols[symbol];
            builder.add_recursive_touch(block, argument)?;
        }
        ModifyRc::Free(symbol) => {
            // this is almost certainly suboptimal, but not incorrect
            let argument = env.symbols[symbol];
            builder.add_recursive_touch(block, argument)?;
        }
    }

    Ok(())
}

fn stmt_spec<'a>(
    builder: &mut FuncDefBuilder,
    interner: &STLayoutInterner<'a>,
    env: &mut Env<'a>,
    block: BlockId,
    layout: InLayout<'a>,
    stmt: &Stmt<'a>,
) -> Result<ValueId> {
    use Stmt::*;

    match stmt {
        Let(symbol, expr, expr_layout, mut continuation) => {
            let value_id = expr_spec(builder, interner, env, block, *expr_layout, expr)?;
            env.symbols.insert(*symbol, value_id);

            let mut queue = vec![symbol];

            loop {
                match continuation {
                    Let(symbol, expr, expr_layout, c) => {
                        let value_id =
                            expr_spec(builder, interner, env, block, *expr_layout, expr)?;
                        env.symbols.insert(*symbol, value_id);

                        queue.push(symbol);
                        continuation = c;
                    }
                    Refcounting(modify_rc, c) => {
                        // in practice it is common to see a chain of `Let`s interspersed with
                        // Inc/Dec. For e.g. the False interpreter, this caused stack overflows.
                        // so we handle RC operations here to limit recursion depth
                        apply_refcount_operation(builder, env, block, modify_rc)?;

                        continuation = c;
                    }
                    _ => break,
                }
            }

            let result = stmt_spec(builder, interner, env, block, layout, continuation)?;

            for symbol in queue {
                env.symbols.remove(symbol);
            }

            Ok(result)
        }
        Switch {
            cond_symbol: _,
            cond_layout: _,
            branches,
            default_branch,
            ret_layout: _lies,
        } => {
            let mut cases = Vec::with_capacity(branches.len() + 1);

            let it = branches
                .iter()
                .map(|(_, _, body)| body)
                .chain(std::iter::once(default_branch.1));

            for branch in it {
                let block = builder.add_block();
                let value_id = stmt_spec(builder, interner, env, block, layout, branch)?;
                cases.push(BlockExpr(block, value_id));
            }

            builder.add_choice(block, &cases)
        }
        Dbg { remainder, .. } => stmt_spec(builder, interner, env, block, layout, remainder),
        Expect { remainder, .. } => stmt_spec(builder, interner, env, block, layout, remainder),
        Ret(symbol) => Ok(env.symbols[symbol]),
        Refcounting(modify_rc, continuation) => {
            apply_refcount_operation(builder, env, block, modify_rc)?;

            stmt_spec(builder, interner, env, block, layout, continuation)
        }
        Join {
            id,
            parameters,
            body,
            remainder,
        } => {
            let mut type_ids = Vec::new();

            for p in parameters.iter() {
                type_ids.push(layout_spec(
                    env,
                    builder,
                    interner,
                    interner.get_repr(p.layout),
                )?);
            }

            let ret_type_id = layout_spec(env, builder, interner, interner.get_repr(layout))?;

            let jp_arg_type_id = builder.add_tuple_type(&type_ids)?;

            let (jpid, jp_argument) =
                builder.declare_continuation(block, jp_arg_type_id, ret_type_id)?;

            // NOTE join point arguments can shadow variables from the outer scope
            // the ordering of steps here is important

            // add this ID so both body and remainder can reference it
            env.join_points.insert(*id, jpid);

            // first, with the current variable bindings, process the remainder
            let cont_block = builder.add_block();
            let cont_value_id = stmt_spec(builder, interner, env, cont_block, layout, remainder)?;

            // only then introduce variables bound by the jump point, and process its body
            let join_body_sub_block = {
                let jp_body_block = builder.add_block();

                // unpack the argument
                for (i, p) in parameters.iter().enumerate() {
                    let value_id =
                        builder.add_get_tuple_field(jp_body_block, jp_argument, i as u32)?;

                    env.symbols.insert(p.symbol, value_id);
                }

                let jp_body_value_id =
                    stmt_spec(builder, interner, env, jp_body_block, layout, body)?;

                BlockExpr(jp_body_block, jp_body_value_id)
            };

            env.join_points.remove(id);
            builder.define_continuation(jpid, join_body_sub_block)?;

            builder.add_sub_block(block, BlockExpr(cont_block, cont_value_id))
        }
        Jump(id, symbols) => {
            let ret_type_id = layout_spec(env, builder, interner, interner.get_repr(layout))?;
            let argument = build_tuple_value(builder, env, block, symbols)?;

            let jpid = env.join_points[id];
            builder.add_jump(block, jpid, argument, ret_type_id)
        }
        Crash(msg, _) => {
            // Model this as a foreign call rather than TERMINATE because
            // we want ownership of the message.
            let result_type = layout_spec(env, builder, interner, interner.get_repr(layout))?;

            builder.add_unknown_with(block, &[env.symbols[msg]], result_type)
        }
    }
}

fn build_tuple_value(
    builder: &mut FuncDefBuilder,
    env: &Env,
    block: BlockId,
    symbols: &[Symbol],
) -> Result<ValueId> {
    let mut value_ids = Vec::new();

    for field in symbols.iter() {
        let value_id = match env.symbols.get(field) {
            None => panic!(
                "Symbol {:?} is not defined in environment {:?}",
                field, &env.symbols
            ),
            Some(x) => *x,
        };
        value_ids.push(value_id);
    }

    builder.add_make_tuple(block, &value_ids)
}

fn build_recursive_tuple_type<'a>(
    env: &mut Env<'a>,
    builder: &mut impl TypeContext,
    interner: &STLayoutInterner<'a>,
    layouts: &[InLayout<'a>],
) -> Result<TypeId> {
    let mut field_types = Vec::new();

    for field in layouts.iter() {
        let type_id = layout_spec_help(env, builder, interner, interner.get_repr(*field))?;
        field_types.push(type_id);
    }

    builder.add_tuple_type(&field_types)
}

fn build_tuple_type<'a>(
    env: &mut Env<'a>,
    builder: &mut impl TypeContext,
    interner: &STLayoutInterner<'a>,
    layouts: &[InLayout<'a>],
) -> Result<TypeId> {
    let mut field_types = Vec::new();

    for field in layouts.iter() {
        field_types.push(layout_spec(
            env,
            builder,
            interner,
            interner.get_repr(*field),
        )?);
    }

    builder.add_tuple_type(&field_types)
}

fn add_loop(
    builder: &mut FuncDefBuilder,
    block: BlockId,
    state_type: TypeId,
    init_state: ValueId,
    make_body: impl for<'a> FnOnce(&'a mut FuncDefBuilder, BlockId, ValueId) -> Result<ValueId>,
) -> Result<ValueId> {
    let sub_block = builder.add_block();
    let (loop_cont, loop_arg) = builder.declare_continuation(sub_block, state_type, state_type)?;
    let body = builder.add_block();
    let ret_branch = builder.add_block();
    let loop_branch = builder.add_block();
    let new_state = make_body(builder, loop_branch, loop_arg)?;
    let unreachable = builder.add_jump(loop_branch, loop_cont, new_state, state_type)?;
    let result = builder.add_choice(
        body,
        &[
            BlockExpr(ret_branch, loop_arg),
            BlockExpr(loop_branch, unreachable),
        ],
    )?;
    builder.define_continuation(loop_cont, BlockExpr(body, result))?;
    let unreachable = builder.add_jump(sub_block, loop_cont, init_state, state_type)?;
    builder.add_sub_block(block, BlockExpr(sub_block, unreachable))
}

fn call_spec<'a>(
    builder: &mut FuncDefBuilder,
    interner: &STLayoutInterner<'a>,
    env: &mut Env<'a>,
    block: BlockId,
    layout: InLayout<'a>,
    call: &Call<'a>,
) -> Result<ValueId> {
    use CallType::*;

    match &call.call_type {
        ByName {
            name,
            ret_layout,
            arg_layouts,
            specialization_id,
        } => {
            let array = specialization_id.to_bytes();
            let spec_var = CalleeSpecVar(&array);

            let arg_value_id = build_tuple_value(builder, env, block, call.arguments)?;
            let args_it = arg_layouts.iter().copied();
            let captures_niche = name.niche();
            let bytes = func_name_bytes_help(name.name(), args_it, captures_niche, *ret_layout);
            let name = FuncName(&bytes);
            let module = MOD_APP;
            builder.add_call(block, spec_var, module, name, arg_value_id)
        }
        ByPointer {
            pointer,
            ret_layout,
            arg_layouts: _,
        } => {
            let result_type = layout_spec(env, builder, interner, interner.get_repr(*ret_layout))?;
            let fnptr = env.symbols[pointer];
            let arg_value_id = build_tuple_value(builder, env, block, call.arguments)?;
            builder.add_unknown_with(block, &[fnptr, arg_value_id], result_type)
        }
        Foreign {
            foreign_symbol: _,
            ret_layout,
        } => {
            let arguments: Vec<_> = call
                .arguments
                .iter()
                .map(|symbol| env.symbols[symbol])
                .collect();

            let result_type = layout_spec(env, builder, interner, interner.get_repr(*ret_layout))?;

            builder.add_unknown_with(block, &arguments, result_type)
        }
        LowLevel { op, update_mode } => lowlevel_spec(
            builder,
            interner,
            env,
            block,
            layout,
            op,
            *update_mode,
            call.arguments,
        ),
        HigherOrder(HigherOrderLowLevel {
            closure_env_layout,
            update_mode,
            op,
            passed_function,
            ..
        }) => {
            use roc_mono::low_level::HigherOrder::*;

            let array = passed_function.specialization_id.to_bytes();
            let spec_var = CalleeSpecVar(&array);

            let mode = update_mode.to_bytes();
            let update_mode_var = UpdateModeVar(&mode);

            let args_it = passed_function.argument_layouts.iter().copied();
            let captures_niche = passed_function.name.niche();
            let bytes = func_name_bytes_help(
                passed_function.name.name(),
                args_it,
                captures_niche,
                passed_function.return_layout,
            );
            let name = FuncName(&bytes);
            let module = MOD_APP;

            let closure_env = env.symbols[&passed_function.captured_environment];

            let argument_layouts = passed_function.argument_layouts;

            macro_rules! call_function {
                ($builder: expr, $block:expr, [$($arg:expr),+ $(,)?]) => {{
                    let argument = if closure_env_layout.is_none() {
                        $builder.add_make_tuple($block, &[$($arg),+])?
                    } else {
                        $builder.add_make_tuple($block, &[$($arg),+, closure_env])?
                    };

                    $builder.add_call($block, spec_var, module, name, argument)?
                }};
            }

            match op {
                ListSortWith { xs } => {
                    let list = env.symbols[xs];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let bag = builder.add_get_tuple_field(block, state, LIST_BAG_INDEX)?;
                        let cell = builder.add_get_tuple_field(block, state, LIST_CELL_INDEX)?;

                        let element_1 = builder.add_bag_get(block, bag)?;
                        let element_2 = builder.add_bag_get(block, bag)?;

                        let _ = call_function!(builder, block, [element_1, element_2]);

                        builder.add_update(block, update_mode_var, cell)?;

                        with_new_heap_cell(builder, block, bag)
                    };

                    let arg0_layout = argument_layouts[0];

                    let state_layout = LayoutRepr::Builtin(Builtin::List(arg0_layout));
                    let state_type = layout_spec(env, builder, interner, state_layout)?;
                    let init_state = list;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }
            }
        }
    }
}

fn list_append(
    builder: &mut FuncDefBuilder,
    block: BlockId,
    update_mode_var: UpdateModeVar,
    list: ValueId,
    to_insert: ValueId,
) -> Result<ValueId> {
    let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
    let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

    let _unit = builder.add_update(block, update_mode_var, cell)?;

    let new_bag = builder.add_bag_insert(block, bag, to_insert)?;

    with_new_heap_cell(builder, block, new_bag)
}

fn list_clone(
    builder: &mut FuncDefBuilder,
    block: BlockId,
    update_mode_var: UpdateModeVar,
    list: ValueId,
) -> Result<ValueId> {
    let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
    let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

    let _unit = builder.add_update(block, update_mode_var, cell)?;

    with_new_heap_cell(builder, block, bag)
}

#[allow(clippy::too_many_arguments)]
fn lowlevel_spec<'a>(
    builder: &mut FuncDefBuilder,
    interner: &STLayoutInterner<'a>,
    env: &mut Env<'a>,
    block: BlockId,
    layout: InLayout<'a>,
    op: &LowLevel,
    update_mode: roc_mono::ir::UpdateModeId,
    arguments: &[Symbol],
) -> Result<ValueId> {
    use LowLevel::*;

    let type_id = layout_spec(env, builder, interner, interner.get_repr(layout))?;
    let mode = update_mode.to_bytes();
    let update_mode_var = UpdateModeVar(&mode);

    match op {
        NumAdd | NumSub => {
            // NOTE these numeric operations panic (e.g. on overflow)

            let pass_block = {
                let block = builder.add_block();
                let value = new_num(builder, block)?;
                BlockExpr(block, value)
            };

            let fail_block = {
                let block = builder.add_block();
                let value = builder.add_terminate(block, type_id)?;
                BlockExpr(block, value)
            };

            let sub_block = {
                let block = builder.add_block();
                let choice = builder.add_choice(block, &[pass_block, fail_block])?;

                BlockExpr(block, choice)
            };

            builder.add_sub_block(block, sub_block)
        }
        NumToFrac => {
            // just dream up a unit value
            builder.add_make_tuple(block, &[])
        }
        Eq | NotEq => {
            // just dream up a unit value
            builder.add_make_tuple(block, &[])
        }
        NumLte | NumLt | NumGt | NumGte | NumCompare => {
            // just dream up a unit value
            builder.add_make_tuple(block, &[])
        }
        ListLenUsize | ListLenU64 => {
            // TODO should this touch the heap cell?
            // just dream up a unit value
            builder.add_make_tuple(block, &[])
        }
        ListGetUnsafe => {
            // NOTE the ListGet lowlevel op is only evaluated if the index is in-bounds
            let list = env.symbols[&arguments[0]];

            let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

            let _unit = builder.add_touch(block, cell)?;

            builder.add_bag_get(block, bag)
        }
        ListReplaceUnsafe => {
            let list = env.symbols[&arguments[0]];
            let to_insert = env.symbols[&arguments[2]];

            let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

            let _unit1 = builder.add_touch(block, cell)?;
            let _unit2 = builder.add_update(block, update_mode_var, cell)?;

            let new_bag = builder.add_bag_insert(block, bag, to_insert)?;

            let old_value = builder.add_bag_get(block, new_bag)?;
            let new_list = with_new_heap_cell(builder, block, new_bag)?;

            // depending on the types, the list or value will come first in the struct
            let fields = match interner.get_repr(layout) {
                LayoutRepr::Struct(field_layouts) => field_layouts,
                _ => unreachable!(),
            };

            match (interner.get_repr(fields[0]), interner.get_repr(fields[1])) {
                (LayoutRepr::Builtin(Builtin::List(_)), LayoutRepr::Builtin(Builtin::List(_))) => {
                    // field name is the tie breaker, list is first in
                    // { list : List a, value : a }
                    builder.add_make_tuple(block, &[new_list, old_value])
                }
                (LayoutRepr::Builtin(Builtin::List(_)), _) => {
                    builder.add_make_tuple(block, &[new_list, old_value])
                }
                (_, LayoutRepr::Builtin(Builtin::List(_))) => {
                    builder.add_make_tuple(block, &[old_value, new_list])
                }
                _ => unreachable!(),
            }
        }
        ListClone => {
            let list = env.symbols[&arguments[0]];

            let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

            let _unit = builder.add_update(block, update_mode_var, cell)?;

            with_new_heap_cell(builder, block, bag)
        }
        ListSwap => {
            let list = env.symbols[&arguments[0]];

            let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

            let _unit = builder.add_update(block, update_mode_var, cell)?;

            with_new_heap_cell(builder, block, bag)
        }
        ListWithCapacity => {
            // essentially an empty list, capacity is not relevant for morphic

            match interner.get_repr(layout) {
                LayoutRepr::Builtin(Builtin::List(element_layout)) => {
                    let type_id =
                        layout_spec(env, builder, interner, interner.get_repr(element_layout))?;
                    new_list(builder, block, type_id)
                }
                _ => unreachable!("empty array does not have a list layout"),
            }
        }
        ListReserve => {
            let list = env.symbols[&arguments[0]];

            list_clone(builder, block, update_mode_var, list)
        }
        ListReleaseExcessCapacity => {
            let list = env.symbols[&arguments[0]];

            list_clone(builder, block, update_mode_var, list)
        }
        ListAppendUnsafe => {
            let list = env.symbols[&arguments[0]];
            let to_insert = env.symbols[&arguments[1]];

            list_append(builder, block, update_mode_var, list, to_insert)
        }
        StrToUtf8 => {
            let string = env.symbols[&arguments[0]];

            let u8_type = builder.add_tuple_type(&[])?;
            let bag = builder.add_empty_bag(block, u8_type)?;
            let cell = builder.add_get_tuple_field(block, string, LIST_CELL_INDEX)?;

            builder.add_make_tuple(block, &[cell, bag])
        }
        StrFromUtf8 => {
            let list = env.symbols[&arguments[0]];

            let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;
            let string = builder.add_make_tuple(block, &[cell])?;

            let byte_index = builder.add_make_tuple(block, &[])?;
            let is_ok = builder.add_make_tuple(block, &[])?;
            let problem_code = builder.add_make_tuple(block, &[])?;

            builder.add_make_tuple(block, &[byte_index, string, is_ok, problem_code])
        }
        _other => {
            // println!("missing {:?}", _other);
            // TODO overly pessimstic
            let arguments: Vec<_> = arguments.iter().map(|symbol| env.symbols[symbol]).collect();

            let result_type = layout_spec(env, builder, interner, interner.get_repr(layout))?;

            builder.add_unknown_with(block, &arguments, result_type)
        }
    }
}

fn recursive_tag_variant<'a>(
    env: &mut Env<'a>,
    builder: &mut impl TypeContext,
    interner: &STLayoutInterner<'a>,
    fields: &[InLayout<'a>],
) -> Result<TypeId> {
    build_recursive_tuple_type(env, builder, interner, fields)
}

fn recursive_variant_types<'a>(
    env: &mut Env<'a>,
    builder: &mut impl TypeContext,
    interner: &STLayoutInterner<'a>,
    union_layout: &UnionLayout<'a>,
) -> Result<Vec<TypeId>> {
    use UnionLayout::*;

    let mut result;

    match union_layout {
        NonRecursive(_) => {
            unreachable!()
        }
        Recursive(tags) => {
            result = Vec::with_capacity(tags.len());

            for tag in tags.iter() {
                result.push(recursive_tag_variant(env, builder, interner, tag)?);
            }
        }
        NonNullableUnwrapped(fields) => {
            result = vec![recursive_tag_variant(env, builder, interner, fields)?];
        }
        NullableWrapped {
            nullable_id,
            other_tags: tags,
        } => {
            result = Vec::with_capacity(tags.len() + 1);

            let cutoff = *nullable_id as usize;

            for tag in tags[..cutoff].iter() {
                result.push(recursive_tag_variant(env, builder, interner, tag)?);
            }

            result.push(recursive_tag_variant(env, builder, interner, &[])?);

            for tag in tags[cutoff..].iter() {
                result.push(recursive_tag_variant(env, builder, interner, tag)?);
            }
        }
        NullableUnwrapped {
            nullable_id,
            other_fields: fields,
        } => {
            let unit = recursive_tag_variant(env, builder, interner, &[])?;
            let other_type = recursive_tag_variant(env, builder, interner, fields)?;

            if *nullable_id {
                // nullable_id == 1
                result = vec![other_type, unit];
            } else {
                result = vec![unit, other_type];
            }
        }
    }

    Ok(result)
}

fn expr_spec<'a>(
    builder: &mut FuncDefBuilder,
    interner: &STLayoutInterner<'a>,
    env: &mut Env<'a>,
    block: BlockId,
    layout: InLayout<'a>,
    expr: &Expr<'a>,
) -> Result<ValueId> {
    use Expr::*;

    match expr {
        Literal(literal) => literal_spec(builder, block, literal),
        NullPointer => {
            let pointer_type = layout_spec(env, builder, interner, interner.get_repr(layout))?;

            builder.add_unknown_with(block, &[], pointer_type)
        }
        Call(call) => call_spec(builder, interner, env, block, layout, call),
        Tag {
            tag_layout,
            tag_id,
            arguments,
            reuse: _,
        } => {
            let data_id = build_tuple_value(builder, env, block, arguments)?;

            let value_id = match tag_layout {
                UnionLayout::NonRecursive(tags) => {
                    let variant_types = non_recursive_variant_types(env, builder, interner, tags)?;
                    let value_id = build_tuple_value(builder, env, block, arguments)?;
                    return builder.add_make_union(block, &variant_types, *tag_id as u32, value_id);
                }
                UnionLayout::NonNullableUnwrapped(_) => {
                    let value_id = data_id;

                    let type_name_bytes = recursive_tag_union_name_bytes(tag_layout).as_bytes();
                    let type_name = TypeName(&type_name_bytes);

                    env.type_names.insert(*tag_layout);

                    return builder.add_make_named(block, MOD_APP, type_name, value_id);
                }
                UnionLayout::Recursive(_) => data_id,
                UnionLayout::NullableWrapped { .. } => data_id,
                UnionLayout::NullableUnwrapped { .. } => data_id,
            };

            let variant_types = recursive_variant_types(env, builder, interner, tag_layout)?;

            let union_id =
                builder.add_make_union(block, &variant_types, *tag_id as u32, value_id)?;

            let tag_value_id = with_new_heap_cell(builder, block, union_id)?;

            let type_name_bytes = recursive_tag_union_name_bytes(tag_layout).as_bytes();
            let type_name = TypeName(&type_name_bytes);

            env.type_names.insert(*tag_layout);

            builder.add_make_named(block, MOD_APP, type_name, tag_value_id)
        }
        Struct(fields) => build_tuple_value(builder, env, block, fields),
        UnionAtIndex {
            index,
            tag_id,
            structure,
            union_layout,
        } => match union_layout {
            UnionLayout::NonRecursive(_) => {
                let index = (*index) as u32;
                let tag_value_id = env.symbols[structure];
                let tuple_value_id =
                    builder.add_unwrap_union(block, tag_value_id, *tag_id as u32)?;

                builder.add_get_tuple_field(block, tuple_value_id, index)
            }
            UnionLayout::Recursive(_)
            | UnionLayout::NullableUnwrapped { .. }
            | UnionLayout::NullableWrapped { .. } => {
                let index = (*index) as u32;
                let tag_value_id = env.symbols[structure];

                let type_name_bytes = recursive_tag_union_name_bytes(union_layout).as_bytes();
                let type_name = TypeName(&type_name_bytes);

                // unwrap the named wrapper
                let union_id = builder.add_unwrap_named(block, MOD_APP, type_name, tag_value_id)?;

                // now we have a tuple (cell, union { ... }); decompose
                let heap_cell = builder.add_get_tuple_field(block, union_id, TAG_CELL_INDEX)?;
                let union_data = builder.add_get_tuple_field(block, union_id, TAG_DATA_INDEX)?;

                // we're reading from this value, so touch the heap cell
                builder.add_touch(block, heap_cell)?;

                // next, unwrap the union at the tag id that we've got
                let variant_id = builder.add_unwrap_union(block, union_data, *tag_id as u32)?;

                builder.add_get_tuple_field(block, variant_id, index)
            }
            UnionLayout::NonNullableUnwrapped { .. } => {
                let index = (*index) as u32;
                debug_assert!(*tag_id == 0);

                let tag_value_id = env.symbols[structure];

                let type_name_bytes = recursive_tag_union_name_bytes(union_layout).as_bytes();
                let type_name = TypeName(&type_name_bytes);

                // the unwrapped recursive tag variant
                let variant_id =
                    builder.add_unwrap_named(block, MOD_APP, type_name, tag_value_id)?;

                builder.add_get_tuple_field(block, variant_id, index)
            }
        },
        GetElementPointer {
            indices,
            structure,
            union_layout,
            ..
        } => {
            debug_assert!(indices.len() >= 2);
            let tag_id = indices[0] as u32;
            let index = indices[1];
            let tag_value_id = env.symbols[structure];

            let type_name_bytes = recursive_tag_union_name_bytes(union_layout).as_bytes();
            let type_name = TypeName(&type_name_bytes);

            // unwrap the named wrapper
            let union_id = builder.add_unwrap_named(block, MOD_APP, type_name, tag_value_id)?;

            // now we have a tuple (cell, union { ... }); decompose
            let heap_cell = builder.add_get_tuple_field(block, union_id, TAG_CELL_INDEX)?;
            let union_data = builder.add_get_tuple_field(block, union_id, TAG_DATA_INDEX)?;

            // we're reading from this value, so touch the heap cell
            builder.add_touch(block, heap_cell)?;

            // next, unwrap the union at the tag id that we've got
            let variant_id = builder.add_unwrap_union(block, union_data, tag_id)?;

            let value = builder.add_get_tuple_field(block, variant_id, index as u32)?;

            // construct the box. Here the heap_cell of the tag is re-used, I'm hoping that that
            // conveys to morphic that we're borrowing into the existing tag?!
            builder.add_make_tuple(block, &[heap_cell, value])
        }

        StructAtIndex {
            index, structure, ..
        } => {
            let value_id = env.symbols[structure];
            builder.add_get_tuple_field(block, value_id, *index as u32)
        }
        Array { elem_layout, elems } => {
            let type_id = layout_spec(env, builder, interner, interner.get_repr(*elem_layout))?;

            let list = new_list(builder, block, type_id)?;

            let mut bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
            let mut all_constants = true;

            for element in elems.iter() {
                let value_id = if let ListLiteralElement::Symbol(symbol) = element {
                    all_constants = false;
                    env.symbols[symbol]
                } else {
                    builder.add_make_tuple(block, &[]).unwrap()
                };

                bag = builder.add_bag_insert(block, bag, value_id)?;
            }

            if all_constants {
                new_static_list(builder, block)
            } else {
                with_new_heap_cell(builder, block, bag)
            }
        }

        EmptyArray => match interner.get_repr(layout) {
            LayoutRepr::Builtin(Builtin::List(element_layout)) => {
                let type_id =
                    layout_spec(env, builder, interner, interner.get_repr(element_layout))?;
                new_list(builder, block, type_id)
            }
            _ => unreachable!("empty array does not have a list layout"),
        },
        Reset {
            symbol,
            update_mode,
        }
        | ResetRef {
            symbol,
            update_mode,
        } => {
            let tag_value_id = env.symbols[symbol];

            let union_layout = match interner.get_repr(layout) {
                LayoutRepr::Union(ul) => ul,
                _ => unreachable!(),
            };

            let type_name_bytes = recursive_tag_union_name_bytes(&union_layout).as_bytes();
            let type_name = TypeName(&type_name_bytes);

            // unwrap the named wrapper
            let union_id = builder.add_unwrap_named(block, MOD_APP, type_name, tag_value_id)?;

            let heap_cell = builder.add_get_tuple_field(block, union_id, TAG_CELL_INDEX)?;
            let union_data = builder.add_get_tuple_field(block, union_id, TAG_DATA_INDEX)?;

            let mode = update_mode.to_bytes();
            let update_mode_var = UpdateModeVar(&mode);

            let _unit = builder.add_update(block, update_mode_var, heap_cell)?;

            let value = with_new_heap_cell(builder, block, union_data)?;
            builder.add_make_named(block, MOD_APP, type_name, value)
        }
        FunctionPointer { .. } => {
            let pointer_type = layout_spec(env, builder, interner, interner.get_repr(layout))?;

            builder.add_unknown_with(block, &[], pointer_type)
        }
        ErasedMake { callee, value } => {
            let value = match value {
                Some(v) => box_erasure_value_unknown(builder, block, env.symbols[v]),
                // model nullptr
                None => box_erasure_value_unknown_nullptr(builder, block),
            }?;

            let callee = env.symbols[callee];

            erasure_make(builder, block, value, callee)
        }
        ErasedLoad { symbol, field } => {
            let value = env.symbols[symbol];
            let loaded_type = layout_spec(env, builder, interner, interner.get_repr(layout))?;

            erasure_load(builder, block, value, *field, loaded_type)
        }
        GetTagId { .. } => {
            // TODO touch heap cell in recursive cases

            builder.add_make_tuple(block, &[])
        }
        Alloca { initializer, .. } => {
            let initializer = &initializer.as_ref().map(|s| env.symbols[s]);
            let values = match initializer {
                Some(initializer) => std::slice::from_ref(initializer),
                None => &[],
            };

            let type_id = layout_spec(env, builder, interner, interner.get_repr(layout))?;
            builder.add_unknown_with(block, values, type_id)
        }
    }
}

fn literal_spec(
    builder: &mut FuncDefBuilder,
    block: BlockId,
    literal: &Literal,
) -> Result<ValueId> {
    use Literal::*;

    match literal {
        Str(_) => new_static_string(builder, block),
        Int(_) | U128(_) | Float(_) | Decimal(_) | Bool(_) | Byte(_) => {
            builder.add_make_tuple(block, &[])
        }
    }
}

fn layout_spec<'a>(
    env: &mut Env<'a>,
    builder: &mut impl TypeContext,
    interner: &STLayoutInterner<'a>,
    layout: LayoutRepr<'a>,
) -> Result<TypeId> {
    layout_spec_help(env, builder, interner, layout)
}

fn non_recursive_variant_types<'a>(
    env: &mut Env<'a>,
    builder: &mut impl TypeContext,
    interner: &STLayoutInterner<'a>,
    tags: &[&[InLayout<'a>]],
) -> Result<Vec<TypeId>> {
    let mut result = Vec::with_capacity(tags.len());

    for tag in tags.iter() {
        result.push(build_tuple_type(env, builder, interner, tag)?);
    }

    Ok(result)
}

fn layout_spec_help<'a>(
    env: &mut Env<'a>,
    builder: &mut impl TypeContext,
    interner: &STLayoutInterner<'a>,
    layout: LayoutRepr<'a>,
) -> Result<TypeId> {
    use LayoutRepr::*;

    match layout {
        Builtin(builtin) => builtin_spec(env, builder, interner, &builtin),
        Struct(field_layouts) => build_recursive_tuple_type(env, builder, interner, field_layouts),
        LambdaSet(lambda_set) => layout_spec_help(
            env,
            builder,
            interner,
            interner.get_repr(lambda_set.runtime_representation()),
        ),
        Union(union_layout) => {
            match union_layout {
                UnionLayout::NonRecursive(&[]) => {
                    // must model Void as Unit, otherwise we run into problems where
                    // we have to construct values of the void type,
                    // which is of course not possible
                    builder.add_tuple_type(&[])
                }
                UnionLayout::NonRecursive(tags) => {
                    let variant_types = non_recursive_variant_types(env, builder, interner, tags)?;
                    builder.add_union_type(&variant_types)
                }
                UnionLayout::Recursive(_)
                | UnionLayout::NullableUnwrapped { .. }
                | UnionLayout::NullableWrapped { .. }
                | UnionLayout::NonNullableUnwrapped(_) => {
                    let type_name_bytes = recursive_tag_union_name_bytes(&union_layout).as_bytes();
                    let type_name = TypeName(&type_name_bytes);

                    env.type_names.insert(union_layout);

                    Ok(builder.add_named_type(MOD_APP, type_name))
                }
            }
        }

        Ptr(inner_layout) => {
            let inner_type =
                layout_spec_help(env, builder, interner, interner.get_repr(inner_layout))?;
            let cell_type = builder.add_heap_cell_type();

            builder.add_tuple_type(&[cell_type, inner_type])
        }

        // TODO(recursive-layouts): update once we have recursive pointer loops
        RecursivePointer(union_layout) => match interner.get_repr(union_layout) {
            LayoutRepr::Union(union_layout) => {
                assert!(!matches!(union_layout, UnionLayout::NonRecursive(..)));
                let type_name_bytes = recursive_tag_union_name_bytes(&union_layout).as_bytes();
                let type_name = TypeName(&type_name_bytes);

                Ok(builder.add_named_type(MOD_APP, type_name))
            }
            _ => internal_error!("somehow, a non-recursive layout is under a recursive pointer"),
        },

        FunctionPointer(_) => function_pointer_type(builder),
        Erased(_) => erasure_type(builder),
    }
}

fn builtin_spec<'a>(
    env: &mut Env<'a>,
    builder: &mut impl TypeContext,
    interner: &STLayoutInterner<'a>,
    builtin: &Builtin<'a>,
) -> Result<TypeId> {
    use Builtin::*;

    match builtin {
        Int(_) | Bool => builder.add_tuple_type(&[]),
        Decimal | Float(_) => builder.add_tuple_type(&[]),
        Str => str_type(builder),
        List(element_layout) => {
            let element_type =
                layout_spec_help(env, builder, interner, interner.get_repr(*element_layout))?;

            let cell = builder.add_heap_cell_type();
            let bag = builder.add_bag_type(element_type)?;

            builder.add_tuple_type(&[cell, bag])
        }
    }
}

fn str_type<TC: TypeContext>(builder: &mut TC) -> Result<TypeId> {
    let cell_id = builder.add_heap_cell_type();
    builder.add_tuple_type(&[cell_id])
}

fn static_list_type<TC: TypeContext>(builder: &mut TC) -> Result<TypeId> {
    let unit_type = builder.add_tuple_type(&[])?;
    let cell = builder.add_heap_cell_type();
    let bag = builder.add_bag_type(unit_type)?;

    builder.add_tuple_type(&[cell, bag])
}

const LIST_CELL_INDEX: u32 = 0;
const LIST_BAG_INDEX: u32 = 1;

const TAG_CELL_INDEX: u32 = 0;
const TAG_DATA_INDEX: u32 = 1;

fn with_new_heap_cell(
    builder: &mut FuncDefBuilder,
    block: BlockId,
    value: ValueId,
) -> Result<ValueId> {
    let cell = builder.add_new_heap_cell(block)?;
    builder.add_make_tuple(block, &[cell, value])
}

fn new_list(builder: &mut FuncDefBuilder, block: BlockId, element_type: TypeId) -> Result<ValueId> {
    let bag = builder.add_empty_bag(block, element_type)?;
    with_new_heap_cell(builder, block, bag)
}

fn new_static_string(builder: &mut FuncDefBuilder, block: BlockId) -> Result<ValueId> {
    let module = MOD_APP;

    builder.add_const_ref(block, module, STATIC_STR_NAME)
}

fn new_static_list(builder: &mut FuncDefBuilder, block: BlockId) -> Result<ValueId> {
    let module = MOD_APP;

    builder.add_const_ref(block, module, STATIC_LIST_NAME)
}

fn new_num(builder: &mut FuncDefBuilder, block: BlockId) -> Result<ValueId> {
    // we model all our numbers as unit values
    builder.add_make_tuple(block, &[])
}

fn function_pointer_type<TC: TypeContext>(builder: &mut TC) -> Result<TypeId> {
    builder.add_tuple_type(&[])
}

const ERASURE_CALEE_INDEX: u32 = 0;
const ERASURE_VALUE_INDEX: u32 = 1;

/// Erasure type modeled as
///
/// ```text
/// Tuple(callee: FnPtr, value: HeapCell)
/// ```
fn erasure_type<TC: TypeContext>(builder: &mut TC) -> Result<TypeId> {
    let value_cell_id = builder.add_heap_cell_type();
    let callee_id = function_pointer_type(builder)?;
    builder.add_tuple_type(&[value_cell_id, callee_id])
}

fn erasure_box_value_type<TC: TypeContext>(builder: &mut TC) -> TypeId {
    builder.add_heap_cell_type()
}

fn box_erasure_value_unknown(
    builder: &mut FuncDefBuilder,
    block: BlockId,
    value: ValueId,
) -> Result<ValueId> {
    let heap_cell = erasure_box_value_type(builder);
    builder.add_unknown_with(block, &[value], heap_cell)
}

fn box_erasure_value_unknown_nullptr(
    builder: &mut FuncDefBuilder,
    block: BlockId,
) -> Result<ValueId> {
    let heap_cell = erasure_box_value_type(builder);
    builder.add_unknown_with(block, &[], heap_cell)
}

/// Erasure value modeled as
///
/// ```text
/// callee = make_tuple(&[])
/// value = unknown(make_tuple(...captures))
///
/// x : Tuple(callee: FnPtr, value: HeapCell)
/// x = make_tuple(callee, value)
/// ```
fn erasure_make(
    builder: &mut FuncDefBuilder,
    block: BlockId,
    value: ValueId,
    callee: ValueId,
) -> Result<ValueId> {
    builder.add_make_tuple(block, &[value, callee])
}

fn erasure_load(
    builder: &mut FuncDefBuilder,
    block: BlockId,
    value: ValueId,
    field: ErasedField,
    loaded_type: TypeId,
) -> Result<ValueId> {
    match field {
        ErasedField::Callee => builder.add_get_tuple_field(block, value, ERASURE_CALEE_INDEX),
        ErasedField::Value | ErasedField::ValuePtr => {
            let unknown_heap_cell_value =
                builder.add_get_tuple_field(block, value, ERASURE_VALUE_INDEX)?;
            // Cast the unknown cell to the wanted type
            builder.add_unknown_with(block, &[unknown_heap_cell_value], loaded_type)
        }
    }
}
