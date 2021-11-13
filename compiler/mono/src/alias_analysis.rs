use morphic_lib::TypeContext;
use morphic_lib::{
    BlockExpr, BlockId, CalleeSpecVar, ConstDefBuilder, ConstName, EntryPointName, ExprContext,
    FuncDef, FuncDefBuilder, FuncName, ModDefBuilder, ModName, ProgramBuilder, Result,
    TypeDefBuilder, TypeId, TypeName, UpdateModeVar, ValueId,
};
use roc_collections::all::{MutMap, MutSet};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use std::convert::TryFrom;

use crate::ir::{
    Call, CallType, Expr, HigherOrderLowLevel, HostExposedLayouts, ListLiteralElement, Literal,
    ModifyRc, OptLevel, Proc, Stmt,
};
use crate::layout::{Builtin, Layout, ListLayout, RawFunctionLayout, UnionLayout};

// just using one module for now
pub const MOD_APP: ModName = ModName(b"UserApp");

pub const STATIC_STR_NAME: ConstName = ConstName(&Symbol::STR_ALIAS_ANALYSIS_STATIC.to_ne_bytes());
pub const STATIC_LIST_NAME: ConstName = ConstName(b"THIS IS A STATIC LIST");

const ENTRY_POINT_NAME: &[u8] = b"mainForHost";

pub fn func_name_bytes(proc: &Proc) -> [u8; SIZE] {
    func_name_bytes_help(proc.name, proc.args.iter().map(|x| x.0), &proc.ret_layout)
}

const DEBUG: bool = false;
const SIZE: usize = if DEBUG { 50 } else { 16 };

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
    return_layout: &Layout<'a>,
) -> [u8; SIZE]
where
    I: Iterator<Item = Layout<'a>>,
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

    if DEBUG {
        for (i, c) in (format!("{:?}", symbol)).chars().take(25).enumerate() {
            name_bytes[25 + i] = c as u8;
        }
    }

    name_bytes
}

fn bytes_as_ascii(bytes: &[u8]) -> String {
    use std::fmt::Write;

    let mut buf = String::new();

    for byte in bytes {
        write!(buf, "{:02X}", byte).unwrap();
    }

    buf
}

pub fn spec_program<'a, I>(
    opt_level: OptLevel,
    entry_point: crate::ir::EntryPoint<'a>,
    procs: I,
) -> Result<morphic_lib::Solutions>
where
    I: Iterator<Item = &'a Proc<'a>>,
{
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

        // all other functions
        for proc in procs {
            let bytes = func_name_bytes(proc);
            let func_name = FuncName(&bytes);

            if let HostExposedLayouts::HostExposed { aliases, .. } = &proc.host_exposed_layouts {
                for (_, (symbol, top_level, layout)) in aliases {
                    match layout {
                        RawFunctionLayout::Function(_, _, _) => {
                            let it = top_level.arguments.iter().copied();
                            let bytes = func_name_bytes_help(*symbol, it, &top_level.result);

                            host_exposed_functions.push((bytes, top_level.arguments));
                        }
                        RawFunctionLayout::ZeroArgumentThunk(_) => {
                            let it = std::iter::once(Layout::Struct(&[]));
                            let bytes = func_name_bytes_help(*symbol, it, &top_level.result);

                            host_exposed_functions.push((bytes, top_level.arguments));
                        }
                    }
                }
            }

            if DEBUG {
                eprintln!(
                    "{:?}: {:?} with {:?} args",
                    proc.name,
                    bytes_as_ascii(&bytes),
                    (proc.args, proc.ret_layout),
                );
            }

            let (spec, type_names) = proc_spec(proc)?;

            type_definitions.extend(type_names);

            m.add_func(func_name, spec)?;
        }

        // the entry point wrapper
        let roc_main_bytes = func_name_bytes_help(
            entry_point.symbol,
            entry_point.layout.arguments.iter().copied(),
            &entry_point.layout.result,
        );
        let roc_main = FuncName(&roc_main_bytes);

        let entry_point_function =
            build_entry_point(entry_point.layout, roc_main, &host_exposed_functions)?;
        let entry_point_name = FuncName(ENTRY_POINT_NAME);
        m.add_func(entry_point_name, entry_point_function)?;

        for union_layout in type_definitions {
            let type_name_bytes = recursive_tag_union_name_bytes(&union_layout).as_bytes();
            let type_name = TypeName(&type_name_bytes);

            let mut builder = TypeDefBuilder::new();

            let variant_types = build_variant_types(&mut builder, &union_layout)?;
            let root_type = if let UnionLayout::NonNullableUnwrapped(_) = union_layout {
                debug_assert_eq!(variant_types.len(), 1);
                variant_types[0]
            } else {
                builder.add_union_type(&variant_types)?
            };

            let type_def = builder.build(root_type)?;

            m.add_named_type(type_name, type_def)?;
        }

        m.build()?
    };

    let program = {
        let mut p = ProgramBuilder::new();
        p.add_mod(MOD_APP, main_module)?;

        let entry_point_name = FuncName(ENTRY_POINT_NAME);
        p.add_entry_point(EntryPointName(ENTRY_POINT_NAME), MOD_APP, entry_point_name)?;

        p.build()?
    };

    if DEBUG {
        eprintln!("{}", program.to_source_string());
    }

    match opt_level {
        OptLevel::Development | OptLevel::Normal => morphic_lib::solve_trivial(program),
        OptLevel::Optimize => morphic_lib::solve(program),
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

fn build_entry_point(
    layout: crate::ir::ProcLayout,
    func_name: FuncName,
    host_exposed_functions: &[([u8; SIZE], &[Layout])],
) -> Result<FuncDef> {
    let mut builder = FuncDefBuilder::new();
    let outer_block = builder.add_block();

    let mut cases = Vec::new();

    {
        let block = builder.add_block();

        // to the modelling language, the arguments appear out of thin air
        let argument_type = build_tuple_type(&mut builder, layout.arguments)?;

        // does not make any assumptions about the input
        // let argument = builder.add_unknown_with(block, &[], argument_type)?;

        // assumes the input can be updated in-place
        let argument = terrible_hack(&mut builder, block, argument_type)?;

        let name_bytes = [0; 16];
        let spec_var = CalleeSpecVar(&name_bytes);
        let result = builder.add_call(block, spec_var, MOD_APP, func_name, argument)?;

        // to the modelling language, the result disappears into the void
        let unit_type = builder.add_tuple_type(&[])?;
        let unit_value = builder.add_unknown_with(block, &[result], unit_type)?;

        cases.push(BlockExpr(block, unit_value));
    }

    // add fake calls to host-exposed functions so they are specialized
    for (name_bytes, layouts) in host_exposed_functions {
        let host_exposed_func_name = FuncName(name_bytes);

        if host_exposed_func_name == func_name {
            continue;
        }

        let block = builder.add_block();

        let type_id = layout_spec(&mut builder, &Layout::Struct(layouts))?;

        let argument = builder.add_unknown_with(block, &[], type_id)?;

        let spec_var = CalleeSpecVar(name_bytes);
        let result =
            builder.add_call(block, spec_var, MOD_APP, host_exposed_func_name, argument)?;

        let unit_type = builder.add_tuple_type(&[])?;
        let unit_value = builder.add_unknown_with(block, &[result], unit_type)?;

        cases.push(BlockExpr(block, unit_value));
    }

    let unit_type = builder.add_tuple_type(&[])?;
    let unit_value = builder.add_choice(outer_block, &cases)?;

    let root = BlockExpr(outer_block, unit_value);
    let spec = builder.build(unit_type, unit_type, root)?;

    Ok(spec)
}

fn proc_spec<'a>(proc: &Proc<'a>) -> Result<(FuncDef, MutSet<UnionLayout<'a>>)> {
    let mut builder = FuncDefBuilder::new();
    let mut env = Env::default();

    let block = builder.add_block();

    // introduce the arguments
    let mut argument_layouts = Vec::new();
    for (i, (layout, symbol)) in proc.args.iter().enumerate() {
        let value_id = builder.add_get_tuple_field(block, builder.get_argument(), i as u32)?;
        env.symbols.insert(*symbol, value_id);

        argument_layouts.push(*layout);
    }

    let value_id = stmt_spec(&mut builder, &mut env, block, &proc.ret_layout, &proc.body)?;

    let root = BlockExpr(block, value_id);
    let arg_type_id = layout_spec(&mut builder, &Layout::Struct(&argument_layouts))?;
    let ret_type_id = layout_spec(&mut builder, &proc.ret_layout)?;

    let spec = builder.build(arg_type_id, ret_type_id, root)?;

    Ok((spec, env.type_names))
}

#[derive(Default)]
struct Env<'a> {
    symbols: MutMap<Symbol, ValueId>,
    join_points: MutMap<crate::ir::JoinPointId, morphic_lib::ContinuationId>,
    type_names: MutSet<UnionLayout<'a>>,
}

fn stmt_spec<'a>(
    builder: &mut FuncDefBuilder,
    env: &mut Env<'a>,
    block: BlockId,
    layout: &Layout,
    stmt: &Stmt<'a>,
) -> Result<ValueId> {
    use Stmt::*;

    match stmt {
        Let(symbol, expr, expr_layout, mut continuation) => {
            let value_id = expr_spec(builder, env, block, expr_layout, expr)?;
            env.symbols.insert(*symbol, value_id);

            let mut queue = vec![symbol];

            while let Let(symbol, expr, expr_layout, c) = continuation {
                let value_id = expr_spec(builder, env, block, expr_layout, expr)?;
                env.symbols.insert(*symbol, value_id);

                queue.push(symbol);
                continuation = c;
            }

            let result = stmt_spec(builder, env, block, layout, continuation)?;

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
                let value_id = stmt_spec(builder, env, block, layout, branch)?;
                cases.push(BlockExpr(block, value_id));
            }

            builder.add_choice(block, &cases)
        }
        Ret(symbol) => Ok(env.symbols[symbol]),
        Refcounting(modify_rc, continuation) => match modify_rc {
            ModifyRc::Inc(symbol, _) => {
                let argument = env.symbols[symbol];

                // a recursive touch is never worse for optimizations than a normal touch
                // and a bit more permissive in its type
                builder.add_recursive_touch(block, argument)?;

                stmt_spec(builder, env, block, layout, continuation)
            }

            ModifyRc::Dec(symbol) => {
                let argument = env.symbols[symbol];

                builder.add_recursive_touch(block, argument)?;

                stmt_spec(builder, env, block, layout, continuation)
            }
            ModifyRc::DecRef(symbol) => {
                let argument = env.symbols[symbol];

                builder.add_recursive_touch(block, argument)?;

                stmt_spec(builder, env, block, layout, continuation)
            }
        },
        Join {
            id,
            parameters,
            body,
            remainder,
        } => {
            let mut type_ids = Vec::new();

            for p in parameters.iter() {
                type_ids.push(layout_spec(builder, &p.layout)?);
            }

            let ret_type_id = layout_spec(builder, layout)?;

            let jp_arg_type_id = builder.add_tuple_type(&type_ids)?;

            let (jpid, jp_argument) =
                builder.declare_continuation(block, jp_arg_type_id, ret_type_id)?;

            // NOTE join point arguments can shadow variables from the outer scope
            // the ordering of steps here is important

            // add this ID so both body and remainder can reference it
            env.join_points.insert(*id, jpid);

            // first, with the current variable bindings, process the remainder
            let cont_block = builder.add_block();
            let cont_value_id = stmt_spec(builder, env, cont_block, layout, remainder)?;

            // only then introduce variables bound by the jump point, and process its body
            let join_body_sub_block = {
                let jp_body_block = builder.add_block();

                // unpack the argument
                for (i, p) in parameters.iter().enumerate() {
                    let value_id =
                        builder.add_get_tuple_field(jp_body_block, jp_argument, i as u32)?;

                    env.symbols.insert(p.symbol, value_id);
                }

                let jp_body_value_id = stmt_spec(builder, env, jp_body_block, layout, body)?;

                BlockExpr(jp_body_block, jp_body_value_id)
            };

            env.join_points.remove(id);
            builder.define_continuation(jpid, join_body_sub_block)?;

            builder.add_sub_block(block, BlockExpr(cont_block, cont_value_id))
        }
        Jump(id, symbols) => {
            let ret_type_id = layout_spec(builder, layout)?;
            let argument = build_tuple_value(builder, env, block, symbols)?;

            let jpid = env.join_points[id];
            builder.add_jump(block, jpid, argument, ret_type_id)
        }
        RuntimeError(_) => {
            let type_id = layout_spec(builder, layout)?;

            builder.add_terminate(block, type_id)
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

#[derive(Clone, Debug, PartialEq)]
enum WhenRecursive<'a> {
    Unreachable,
    Loop(UnionLayout<'a>),
}

fn build_recursive_tuple_type(
    builder: &mut impl TypeContext,
    layouts: &[Layout],
    when_recursive: &WhenRecursive,
) -> Result<TypeId> {
    let mut field_types = Vec::new();

    for field in layouts.iter() {
        field_types.push(layout_spec_help(builder, field, when_recursive)?);
    }

    builder.add_tuple_type(&field_types)
}

fn build_tuple_type(builder: &mut impl TypeContext, layouts: &[Layout]) -> Result<TypeId> {
    let mut field_types = Vec::new();

    for field in layouts.iter() {
        field_types.push(layout_spec(builder, field)?);
    }

    builder.add_tuple_type(&field_types)
}

#[repr(u32)]
#[derive(Clone, Copy)]
enum KeepResult {
    Errs = ERR_TAG_ID,
    Oks = OK_TAG_ID,
}

impl KeepResult {
    fn invert(&self) -> Self {
        match self {
            KeepResult::Errs => KeepResult::Oks,
            KeepResult::Oks => KeepResult::Errs,
        }
    }
}

#[derive(Clone, Copy)]
enum ResultRepr<'a> {
    Int1,
    NonRecursive { err: Layout<'a>, ok: Layout<'a> },
}

impl<'a> ResultRepr<'a> {
    fn from_layout(layout: &Layout<'a>) -> Self {
        match layout {
            Layout::Union(UnionLayout::NonRecursive(tags)) => ResultRepr::NonRecursive {
                err: tags[ERR_TAG_ID as usize][0],
                ok: tags[OK_TAG_ID as usize][0],
            },
            Layout::Builtin(Builtin::Int1) => ResultRepr::Int1,
            other => unreachable!("unexpected layout: {:?}", other),
        }
    }

    fn unwrap(
        &self,
        builder: &mut FuncDefBuilder,
        block: BlockId,
        err_or_ok: ValueId,
        keep_tag_id: u32,
    ) -> Result<ValueId> {
        match self {
            ResultRepr::NonRecursive { .. } => {
                let unwrapped = builder.add_unwrap_union(block, err_or_ok, keep_tag_id)?;

                builder.add_get_tuple_field(block, unwrapped, 0)
            }
            ResultRepr::Int1 => builder.add_make_tuple(block, &[]),
        }
    }
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

fn call_spec(
    builder: &mut FuncDefBuilder,
    env: &Env,
    block: BlockId,
    layout: &Layout,
    call: &Call,
) -> Result<ValueId> {
    use CallType::*;

    match &call.call_type {
        ByName {
            name: symbol,
            ret_layout,
            arg_layouts,
            specialization_id,
        } => {
            let array = specialization_id.to_bytes();
            let spec_var = CalleeSpecVar(&array);

            let arg_value_id = build_tuple_value(builder, env, block, call.arguments)?;
            let it = arg_layouts.iter().copied();
            let bytes = func_name_bytes_help(*symbol, it, ret_layout);
            let name = FuncName(&bytes);
            let module = MOD_APP;
            builder.add_call(block, spec_var, module, name, arg_value_id)
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

            let result_type = layout_spec(builder, ret_layout)?;

            builder.add_unknown_with(block, &arguments, result_type)
        }
        LowLevel { op, update_mode } => lowlevel_spec(
            builder,
            env,
            block,
            layout,
            op,
            *update_mode,
            call.arguments,
        ),
        HigherOrder(HigherOrderLowLevel {
            specialization_id,
            closure_env_layout,
            update_mode,
            op,
            arg_layouts,
            ret_layout,
            function_name,
            function_env,
            ..
        }) => {
            use crate::low_level::HigherOrder::*;

            let array = specialization_id.to_bytes();
            let spec_var = CalleeSpecVar(&array);

            let mode = update_mode.to_bytes();
            let update_mode_var = UpdateModeVar(&mode);

            let it = arg_layouts.iter().copied();
            let bytes = func_name_bytes_help(*function_name, it, ret_layout);
            let name = FuncName(&bytes);
            let module = MOD_APP;

            let closure_env = env.symbols[function_env];

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
                DictWalk { xs, state } => {
                    let dict = env.symbols[xs];
                    let state = env.symbols[state];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let bag = builder.add_get_tuple_field(block, dict, DICT_BAG_INDEX)?;

                        let element = builder.add_bag_get(block, bag)?;

                        let key = builder.add_get_tuple_field(block, element, 0)?;
                        let val = builder.add_get_tuple_field(block, element, 1)?;

                        let new_state = call_function!(builder, block, [state, key, val]);

                        Ok(new_state)
                    };

                    let state_layout = arg_layouts[0];
                    let state_type = layout_spec(builder, &state_layout)?;
                    let init_state = state;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }

                ListWalk { xs, state } | ListWalkBackwards { xs, state } => {
                    let list = env.symbols[xs];
                    let state = env.symbols[state];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;

                        let element = builder.add_bag_get(block, bag)?;

                        let new_state = call_function!(builder, block, [state, element]);

                        Ok(new_state)
                    };

                    let state_layout = arg_layouts[0];
                    let state_type = layout_spec(builder, &state_layout)?;
                    let init_state = state;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }
                ListWalkUntil { xs, state } => {
                    let list = env.symbols[xs];
                    let state = env.symbols[state];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;

                        let element = builder.add_bag_get(block, bag)?;

                        let continue_or_stop = call_function!(builder, block, [state, element]);

                        // just assume it is a continue
                        let unwrapped = builder.add_unwrap_union(block, continue_or_stop, 0)?;
                        let new_state = builder.add_get_tuple_field(block, unwrapped, 0)?;

                        Ok(new_state)
                    };

                    let state_layout = arg_layouts[0];
                    let state_type = layout_spec(builder, &state_layout)?;
                    let init_state = state;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }

                ListMapWithIndex { xs } => {
                    let list = env.symbols[xs];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let input_bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;

                        let element = builder.add_bag_get(block, input_bag)?;
                        let index = builder.add_make_tuple(block, &[])?;

                        let new_element = call_function!(builder, block, [index, element]);

                        list_append(builder, block, update_mode_var, state, new_element)
                    };

                    let output_element_type = layout_spec(builder, ret_layout)?;

                    let state_layout = Layout::Builtin(Builtin::List(ret_layout));
                    let state_type = layout_spec(builder, &state_layout)?;

                    let init_state = new_list(builder, block, output_element_type)?;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }

                ListMap { xs } => {
                    let list = env.symbols[xs];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let input_bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;

                        let element = builder.add_bag_get(block, input_bag)?;

                        let new_element = call_function!(builder, block, [element]);

                        list_append(builder, block, update_mode_var, state, new_element)
                    };

                    let output_element_type = layout_spec(builder, ret_layout)?;

                    let state_layout = Layout::Builtin(Builtin::List(ret_layout));
                    let state_type = layout_spec(builder, &state_layout)?;

                    let init_state = new_list(builder, block, output_element_type)?;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }

                ListSortWith { xs } => {
                    let list = env.symbols[xs];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let bag = builder.add_get_tuple_field(block, state, LIST_BAG_INDEX)?;
                        let cell = builder.add_get_tuple_field(block, state, LIST_CELL_INDEX)?;

                        let element_1 = builder.add_bag_get(block, bag)?;
                        let element_2 = builder.add_bag_get(block, bag)?;

                        let _ = call_function!(builder, block, [element_1, element_2]);

                        builder.add_update(block, update_mode_var, cell)?;

                        let new_cell = builder.add_new_heap_cell(block)?;
                        builder.add_make_tuple(block, &[new_cell, bag])
                    };

                    let state_layout = Layout::Builtin(Builtin::List(&arg_layouts[0]));
                    let state_type = layout_spec(builder, &state_layout)?;
                    let init_state = list;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }

                ListMap2 { xs, ys } => {
                    let list1 = env.symbols[xs];
                    let list2 = env.symbols[ys];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let input_bag_1 =
                            builder.add_get_tuple_field(block, list1, LIST_BAG_INDEX)?;
                        let input_bag_2 =
                            builder.add_get_tuple_field(block, list2, LIST_BAG_INDEX)?;

                        let element_1 = builder.add_bag_get(block, input_bag_1)?;
                        let element_2 = builder.add_bag_get(block, input_bag_2)?;

                        let new_element = call_function!(builder, block, [element_1, element_2]);

                        list_append(builder, block, update_mode_var, state, new_element)
                    };

                    let output_element_type = layout_spec(builder, ret_layout)?;

                    let state_layout = Layout::Builtin(Builtin::List(ret_layout));
                    let state_type = layout_spec(builder, &state_layout)?;

                    let init_state = new_list(builder, block, output_element_type)?;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }

                ListMap3 { xs, ys, zs } => {
                    let list1 = env.symbols[xs];
                    let list2 = env.symbols[ys];
                    let list3 = env.symbols[zs];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let input_bag_1 =
                            builder.add_get_tuple_field(block, list1, LIST_BAG_INDEX)?;
                        let input_bag_2 =
                            builder.add_get_tuple_field(block, list2, LIST_BAG_INDEX)?;
                        let input_bag_3 =
                            builder.add_get_tuple_field(block, list3, LIST_BAG_INDEX)?;

                        let element_1 = builder.add_bag_get(block, input_bag_1)?;
                        let element_2 = builder.add_bag_get(block, input_bag_2)?;
                        let element_3 = builder.add_bag_get(block, input_bag_3)?;

                        let new_element =
                            call_function!(builder, block, [element_1, element_2, element_3]);

                        list_append(builder, block, update_mode_var, state, new_element)
                    };

                    let output_element_type = layout_spec(builder, ret_layout)?;

                    let state_layout = Layout::Builtin(Builtin::List(ret_layout));
                    let state_type = layout_spec(builder, &state_layout)?;

                    let init_state = new_list(builder, block, output_element_type)?;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }
                ListMap4 { xs, ys, zs, ws } => {
                    let list1 = env.symbols[xs];
                    let list2 = env.symbols[ys];
                    let list3 = env.symbols[zs];
                    let list4 = env.symbols[ws];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let input_bag_1 =
                            builder.add_get_tuple_field(block, list1, LIST_BAG_INDEX)?;
                        let input_bag_2 =
                            builder.add_get_tuple_field(block, list2, LIST_BAG_INDEX)?;
                        let input_bag_3 =
                            builder.add_get_tuple_field(block, list3, LIST_BAG_INDEX)?;
                        let input_bag_4 =
                            builder.add_get_tuple_field(block, list4, LIST_BAG_INDEX)?;

                        let element_1 = builder.add_bag_get(block, input_bag_1)?;
                        let element_2 = builder.add_bag_get(block, input_bag_2)?;
                        let element_3 = builder.add_bag_get(block, input_bag_3)?;
                        let element_4 = builder.add_bag_get(block, input_bag_4)?;

                        let new_element = call_function!(
                            builder,
                            block,
                            [element_1, element_2, element_3, element_4]
                        );

                        list_append(builder, block, update_mode_var, state, new_element)
                    };

                    let output_element_type = layout_spec(builder, ret_layout)?;

                    let state_layout = Layout::Builtin(Builtin::List(ret_layout));
                    let state_type = layout_spec(builder, &state_layout)?;

                    let init_state = new_list(builder, block, output_element_type)?;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }
                ListKeepIf { xs } => {
                    let list = env.symbols[xs];

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let bag = builder.add_get_tuple_field(block, state, LIST_BAG_INDEX)?;
                        let cell = builder.add_get_tuple_field(block, state, LIST_CELL_INDEX)?;

                        let element = builder.add_bag_get(block, bag)?;

                        let _ = call_function!(builder, block, [element]);

                        // NOTE: we assume the element is not kept
                        builder.add_update(block, update_mode_var, cell)?;

                        let removed = builder.add_bag_remove(block, bag)?;

                        // decrement the removed element
                        let removed_element = builder.add_get_tuple_field(block, removed, 1)?;
                        builder.add_recursive_touch(block, removed_element)?;

                        let new_bag = builder.add_get_tuple_field(block, removed, 0)?;
                        let new_cell = builder.add_new_heap_cell(block)?;

                        builder.add_make_tuple(block, &[new_cell, new_bag])
                    };

                    let state_layout = Layout::Builtin(Builtin::List(&arg_layouts[0]));
                    let state_type = layout_spec(builder, &state_layout)?;
                    let init_state = list;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }
                ListKeepOks { xs } | ListKeepErrs { xs } => {
                    let list = env.symbols[xs];

                    let keep_result = match op {
                        ListKeepOks { .. } => KeepResult::Oks,
                        ListKeepErrs { .. } => KeepResult::Errs,
                        _ => unreachable!(),
                    };

                    let result_repr = ResultRepr::from_layout(ret_layout);

                    let output_element_layout = match (keep_result, result_repr) {
                        (KeepResult::Errs, ResultRepr::NonRecursive { err, .. }) => err,
                        (KeepResult::Oks, ResultRepr::NonRecursive { ok, .. }) => ok,
                        (_, ResultRepr::Int1) => Layout::Struct(&[]),
                    };

                    let loop_body = |builder: &mut FuncDefBuilder, block, state| {
                        let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;

                        let element = builder.add_bag_get(block, bag)?;

                        let err_or_ok = call_function!(builder, block, [element]);

                        let kept_branch = builder.add_block();
                        let not_kept_branch = builder.add_block();

                        let element_kept = {
                            let block = kept_branch;

                            // a Result can be represented as a Int1
                            let new_element = result_repr.unwrap(
                                builder,
                                block,
                                err_or_ok,
                                keep_result as u32,
                            )?;

                            list_append(builder, block, update_mode_var, state, new_element)?
                        };

                        let element_not_kept = {
                            let block = not_kept_branch;

                            // a Result can be represented as a Int1
                            let dropped_element = result_repr.unwrap(
                                builder,
                                block,
                                err_or_ok,
                                keep_result.invert() as u32,
                            )?;

                            // decrement the element we will not keep
                            builder.add_recursive_touch(block, dropped_element)?;

                            state
                        };

                        builder.add_choice(
                            block,
                            &[
                                BlockExpr(not_kept_branch, element_not_kept),
                                BlockExpr(kept_branch, element_kept),
                            ],
                        )
                    };

                    let output_element_type = layout_spec(builder, &output_element_layout)?;
                    let init_state = new_list(builder, block, output_element_type)?;

                    let state_layout = Layout::Builtin(Builtin::List(&output_element_layout));
                    let state_type = layout_spec(builder, &state_layout)?;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }
                ListAny { xs } => {
                    let list = env.symbols[xs];

                    let loop_body = |builder: &mut FuncDefBuilder, block, _state| {
                        let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
                        let element = builder.add_bag_get(block, bag)?;

                        let new_state = call_function!(builder, block, [element]);

                        Ok(new_state)
                    };

                    let state_layout = Layout::Builtin(Builtin::Int1);
                    let state_type = layout_spec(builder, &state_layout)?;

                    let init_state = new_num(builder, block)?;

                    add_loop(builder, block, state_type, init_state, loop_body)
                }
                ListFindUnsafe { xs } => {
                    let list = env.symbols[xs];

                    // ListFindUnsafe returns { value: v, found: Bool=Int1 }
                    let output_layouts = vec![arg_layouts[0], Layout::Builtin(Builtin::Int1)];
                    let output_layout = Layout::Struct(&output_layouts);
                    let output_type = layout_spec(builder, &output_layout)?;

                    let loop_body = |builder: &mut FuncDefBuilder, block, output| {
                        let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
                        let element = builder.add_bag_get(block, bag)?;
                        let _is_found = call_function!(builder, block, [element]);

                        // We may or may not use the element we got from the list in the output struct,
                        // depending on whether we found the element to satisfy the "find" predicate.
                        // If we did find the element, our output "changes" to be a record including that element.
                        let found_branch = builder.add_block();
                        let new_output =
                            builder.add_unknown_with(block, &[element], output_type)?;

                        let not_found_branch = builder.add_block();

                        builder.add_choice(
                            block,
                            &[
                                BlockExpr(found_branch, new_output),
                                BlockExpr(not_found_branch, output),
                            ],
                        )
                    };

                    // Assume the output is initially { found: False, value: \empty }
                    let output_state = builder.add_unknown_with(block, &[], output_type)?;
                    add_loop(builder, block, output_type, output_state, loop_body)
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

    let new_cell = builder.add_new_heap_cell(block)?;
    builder.add_make_tuple(block, &[new_cell, new_bag])
}

fn lowlevel_spec(
    builder: &mut FuncDefBuilder,
    env: &Env,
    block: BlockId,
    layout: &Layout,
    op: &LowLevel,
    update_mode: crate::ir::UpdateModeId,
    arguments: &[Symbol],
) -> Result<ValueId> {
    use LowLevel::*;

    let type_id = layout_spec(builder, layout)?;
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
        NumToFloat => {
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
        ListLen | DictSize => {
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
        ListSet => {
            let list = env.symbols[&arguments[0]];
            let to_insert = env.symbols[&arguments[2]];

            let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

            // decrement the overwritten element
            let overwritten = builder.add_bag_get(block, bag)?;
            let _unit = builder.add_recursive_touch(block, overwritten)?;

            let _unit = builder.add_update(block, update_mode_var, cell)?;

            builder.add_bag_insert(block, bag, to_insert)?;

            let new_cell = builder.add_new_heap_cell(block)?;
            builder.add_make_tuple(block, &[new_cell, bag])
        }
        ListSwap => {
            let list = env.symbols[&arguments[0]];

            let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

            let _unit = builder.add_update(block, update_mode_var, cell)?;

            let new_cell = builder.add_new_heap_cell(block)?;
            builder.add_make_tuple(block, &[new_cell, bag])
        }
        ListReverse => {
            let list = env.symbols[&arguments[0]];

            let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

            let _unit = builder.add_update(block, update_mode_var, cell)?;

            let new_cell = builder.add_new_heap_cell(block)?;
            builder.add_make_tuple(block, &[new_cell, bag])
        }
        ListAppend => {
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
        DictEmpty => {
            match layout {
                Layout::Builtin(Builtin::EmptyDict) => {
                    // just make up an element type
                    let type_id = builder.add_tuple_type(&[])?;
                    new_dict(builder, block, type_id, type_id)
                }
                Layout::Builtin(Builtin::Dict(key_layout, value_layout)) => {
                    let key_id = layout_spec(builder, key_layout)?;
                    let value_id = layout_spec(builder, value_layout)?;
                    new_dict(builder, block, key_id, value_id)
                }
                _ => unreachable!("empty array does not have a list layout"),
            }
        }
        DictGetUnsafe => {
            // NOTE DictGetUnsafe returns a { flag: Bool, value: v }
            // when the flag is True, the value is found and defined;
            // otherwise it is not and `Dict.get` should return `Err ...`

            let dict = env.symbols[&arguments[0]];
            let key = env.symbols[&arguments[1]];

            // indicate that we use the key
            builder.add_recursive_touch(block, key)?;

            let bag = builder.add_get_tuple_field(block, dict, DICT_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, dict, DICT_CELL_INDEX)?;

            let _unit = builder.add_touch(block, cell)?;
            builder.add_bag_get(block, bag)
        }
        DictInsert => {
            let dict = env.symbols[&arguments[0]];
            let key = env.symbols[&arguments[1]];
            let value = env.symbols[&arguments[2]];

            let key_value = builder.add_make_tuple(block, &[key, value])?;

            let bag = builder.add_get_tuple_field(block, dict, DICT_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, dict, DICT_CELL_INDEX)?;

            let _unit = builder.add_update(block, update_mode_var, cell)?;

            builder.add_bag_insert(block, bag, key_value)?;

            let new_cell = builder.add_new_heap_cell(block)?;
            builder.add_make_tuple(block, &[new_cell, bag])
        }
        _other => {
            // println!("missing {:?}", _other);
            // TODO overly pessimstic
            let arguments: Vec<_> = arguments.iter().map(|symbol| env.symbols[symbol]).collect();

            let result_type = layout_spec(builder, layout)?;

            builder.add_unknown_with(block, &arguments, result_type)
        }
    }
}

fn recursive_tag_variant(
    builder: &mut impl TypeContext,
    union_layout: &UnionLayout,
    fields: &[Layout],
) -> Result<TypeId> {
    let when_recursive = WhenRecursive::Loop(*union_layout);

    let data_id = build_recursive_tuple_type(builder, fields, &when_recursive)?;
    let cell_id = builder.add_heap_cell_type();

    builder.add_tuple_type(&[cell_id, data_id])
}

fn build_variant_types(
    builder: &mut impl TypeContext,
    union_layout: &UnionLayout,
) -> Result<Vec<TypeId>> {
    use UnionLayout::*;

    let mut result;

    match union_layout {
        NonRecursive(tags) => {
            result = Vec::with_capacity(tags.len());

            for tag in tags.iter() {
                result.push(build_tuple_type(builder, tag)?);
            }
        }
        Recursive(tags) => {
            result = Vec::with_capacity(tags.len());

            for tag in tags.iter() {
                result.push(recursive_tag_variant(builder, union_layout, tag)?);
            }
        }
        NonNullableUnwrapped(fields) => {
            result = vec![recursive_tag_variant(builder, union_layout, fields)?];
        }
        NullableWrapped {
            nullable_id,
            other_tags: tags,
        } => {
            result = Vec::with_capacity(tags.len() + 1);

            let cutoff = *nullable_id as usize;

            for tag in tags[..cutoff].iter() {
                result.push(recursive_tag_variant(builder, union_layout, tag)?);
            }

            let unit = builder.add_tuple_type(&[])?;
            result.push(unit);

            for tag in tags[cutoff..].iter() {
                result.push(recursive_tag_variant(builder, union_layout, tag)?);
            }
        }
        NullableUnwrapped {
            nullable_id,
            other_fields: fields,
        } => {
            let unit = builder.add_tuple_type(&[])?;
            let other_type = recursive_tag_variant(builder, union_layout, fields)?;

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

#[allow(dead_code)]
fn worst_case_type(context: &mut impl TypeContext) -> Result<TypeId> {
    let cell = context.add_heap_cell_type();
    context.add_bag_type(cell)
}

fn expr_spec<'a>(
    builder: &mut FuncDefBuilder,
    env: &mut Env<'a>,
    block: BlockId,
    layout: &Layout<'a>,
    expr: &Expr<'a>,
) -> Result<ValueId> {
    use Expr::*;

    match expr {
        Literal(literal) => literal_spec(builder, block, literal),
        Call(call) => call_spec(builder, env, block, layout, call),
        Reuse {
            tag_layout,
            tag_name: _,
            tag_id,
            arguments,
            ..
        }
        | Tag {
            tag_layout,
            tag_name: _,
            tag_id,
            arguments,
        } => {
            let variant_types = build_variant_types(builder, tag_layout)?;

            let data_id = build_tuple_value(builder, env, block, arguments)?;
            let cell_id = builder.add_new_heap_cell(block)?;

            let value_id = match tag_layout {
                UnionLayout::NonRecursive(_) => {
                    let value_id = build_tuple_value(builder, env, block, arguments)?;
                    return builder.add_make_union(block, &variant_types, *tag_id as u32, value_id);
                }
                UnionLayout::NonNullableUnwrapped(_) => {
                    let value_id = builder.add_make_tuple(block, &[cell_id, data_id])?;

                    let type_name_bytes = recursive_tag_union_name_bytes(tag_layout).as_bytes();
                    let type_name = TypeName(&type_name_bytes);

                    env.type_names.insert(*tag_layout);

                    return builder.add_make_named(block, MOD_APP, type_name, value_id);
                }
                UnionLayout::Recursive(_) => builder.add_make_tuple(block, &[cell_id, data_id])?,
                UnionLayout::NullableWrapped { nullable_id, .. } => {
                    if *tag_id == *nullable_id as _ {
                        data_id
                    } else {
                        builder.add_make_tuple(block, &[cell_id, data_id])?
                    }
                }
                UnionLayout::NullableUnwrapped { nullable_id, .. } => {
                    if *tag_id == *nullable_id as _ {
                        data_id
                    } else {
                        builder.add_make_tuple(block, &[cell_id, data_id])?
                    }
                }
            };

            let union_id =
                builder.add_make_union(block, &variant_types, *tag_id as u32, value_id)?;

            let type_name_bytes = recursive_tag_union_name_bytes(tag_layout).as_bytes();
            let type_name = TypeName(&type_name_bytes);

            env.type_names.insert(*tag_layout);

            builder.add_make_named(block, MOD_APP, type_name, union_id)
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

                let union_id = builder.add_unwrap_named(block, MOD_APP, type_name, tag_value_id)?;
                let variant_id = builder.add_unwrap_union(block, union_id, *tag_id as u32)?;

                // we're reading from this value, so touch the heap cell
                let heap_cell = builder.add_get_tuple_field(block, variant_id, 0)?;
                builder.add_touch(block, heap_cell)?;

                let tuple_value_id = builder.add_get_tuple_field(block, variant_id, 1)?;

                builder.add_get_tuple_field(block, tuple_value_id, index)
            }
            UnionLayout::NonNullableUnwrapped { .. } => {
                let index = (*index) as u32;
                debug_assert!(*tag_id == 0);

                let tag_value_id = env.symbols[structure];

                let type_name_bytes = recursive_tag_union_name_bytes(union_layout).as_bytes();
                let type_name = TypeName(&type_name_bytes);

                let variant_id =
                    builder.add_unwrap_named(block, MOD_APP, type_name, tag_value_id)?;

                // we're reading from this value, so touch the heap cell
                let heap_cell = builder.add_get_tuple_field(block, variant_id, 0)?;
                builder.add_touch(block, heap_cell)?;

                let tuple_value_id = builder.add_get_tuple_field(block, variant_id, 1)?;

                builder.add_get_tuple_field(block, tuple_value_id, index)
            }
        },
        StructAtIndex {
            index, structure, ..
        } => {
            let value_id = env.symbols[structure];
            builder.add_get_tuple_field(block, value_id, *index as u32)
        }
        Array { elem_layout, elems } => {
            let type_id = layout_spec(builder, elem_layout)?;

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
                let cell = builder.add_new_heap_cell(block)?;

                builder.add_make_tuple(block, &[cell, bag])
            }
        }

        EmptyArray => {
            use ListLayout::*;

            match ListLayout::try_from(layout) {
                Ok(EmptyList) => {
                    // just make up an element type
                    let type_id = builder.add_tuple_type(&[])?;
                    new_list(builder, block, type_id)
                }
                Ok(List(element_layout)) => {
                    let type_id = layout_spec(builder, element_layout)?;
                    new_list(builder, block, type_id)
                }
                Err(()) => unreachable!("empty array does not have a list layout"),
            }
        }
        Reset(symbol) => {
            let type_id = layout_spec(builder, layout)?;
            let value_id = env.symbols[symbol];

            builder.add_unknown_with(block, &[value_id], type_id)
        }
        RuntimeErrorFunction(_) => {
            let type_id = layout_spec(builder, layout)?;

            builder.add_terminate(block, type_id)
        }
        GetTagId { .. } => builder.add_make_tuple(block, &[]),
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
        Int(_) | Float(_) | Decimal(_) | Bool(_) | Byte(_) => builder.add_make_tuple(block, &[]),
    }
}

fn layout_spec(builder: &mut impl TypeContext, layout: &Layout) -> Result<TypeId> {
    layout_spec_help(builder, layout, &WhenRecursive::Unreachable)
}

fn layout_spec_help(
    builder: &mut impl TypeContext,
    layout: &Layout,
    when_recursive: &WhenRecursive,
) -> Result<TypeId> {
    use Layout::*;

    match layout {
        Builtin(builtin) => builtin_spec(builder, builtin, when_recursive),
        Struct(fields) => build_recursive_tuple_type(builder, fields, when_recursive),
        LambdaSet(lambda_set) => layout_spec_help(
            builder,
            &lambda_set.runtime_representation(),
            when_recursive,
        ),
        Union(union_layout) => {
            let variant_types = build_variant_types(builder, union_layout)?;

            match union_layout {
                UnionLayout::NonRecursive(_) => builder.add_union_type(&variant_types),
                UnionLayout::Recursive(_)
                | UnionLayout::NullableUnwrapped { .. }
                | UnionLayout::NullableWrapped { .. }
                | UnionLayout::NonNullableUnwrapped(_) => {
                    let type_name_bytes = recursive_tag_union_name_bytes(union_layout).as_bytes();
                    let type_name = TypeName(&type_name_bytes);

                    Ok(builder.add_named_type(MOD_APP, type_name))
                }
            }
        }
        RecursivePointer => match when_recursive {
            WhenRecursive::Unreachable => {
                unreachable!()
            }
            WhenRecursive::Loop(union_layout) => match union_layout {
                UnionLayout::NonRecursive(_) => unreachable!(),
                UnionLayout::Recursive(_)
                | UnionLayout::NullableUnwrapped { .. }
                | UnionLayout::NullableWrapped { .. }
                | UnionLayout::NonNullableUnwrapped(_) => {
                    let type_name_bytes = recursive_tag_union_name_bytes(union_layout).as_bytes();
                    let type_name = TypeName(&type_name_bytes);

                    Ok(builder.add_named_type(MOD_APP, type_name))
                }
            },
        },
    }
}

fn builtin_spec(
    builder: &mut impl TypeContext,
    builtin: &Builtin,
    when_recursive: &WhenRecursive,
) -> Result<TypeId> {
    use Builtin::*;

    match builtin {
        Int128 | Int64 | Int32 | Int16 | Int8 | Int1 | Usize => builder.add_tuple_type(&[]),
        Decimal | Float128 | Float64 | Float32 => builder.add_tuple_type(&[]),
        Str | EmptyStr => str_type(builder),
        Dict(key_layout, value_layout) => {
            let value_type = layout_spec_help(builder, value_layout, when_recursive)?;
            let key_type = layout_spec_help(builder, key_layout, when_recursive)?;
            let element_type = builder.add_tuple_type(&[key_type, value_type])?;

            let cell = builder.add_heap_cell_type();
            let bag = builder.add_bag_type(element_type)?;
            builder.add_tuple_type(&[cell, bag])
        }
        Set(key_layout) => {
            let value_type = builder.add_tuple_type(&[])?;
            let key_type = layout_spec_help(builder, key_layout, when_recursive)?;
            let element_type = builder.add_tuple_type(&[key_type, value_type])?;

            let cell = builder.add_heap_cell_type();
            let bag = builder.add_bag_type(element_type)?;
            builder.add_tuple_type(&[cell, bag])
        }
        List(element_layout) => {
            let element_type = layout_spec_help(builder, element_layout, when_recursive)?;

            let cell = builder.add_heap_cell_type();
            let bag = builder.add_bag_type(element_type)?;

            builder.add_tuple_type(&[cell, bag])
        }
        EmptyList => {
            // TODO make sure that we consistently treat the EmptyList as a list of unit values
            let element_type = builder.add_tuple_type(&[])?;

            let cell = builder.add_heap_cell_type();
            let bag = builder.add_bag_type(element_type)?;

            builder.add_tuple_type(&[cell, bag])
        }
        EmptyDict | EmptySet => {
            // TODO make sure that we consistently treat the these as a dict of unit values
            let unit = builder.add_tuple_type(&[])?;
            let element_type = builder.add_tuple_type(&[unit, unit])?;

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

const OK_TAG_ID: u32 = 1;
const ERR_TAG_ID: u32 = 0;

const LIST_CELL_INDEX: u32 = 0;
const LIST_BAG_INDEX: u32 = 1;

const DICT_CELL_INDEX: u32 = LIST_CELL_INDEX;
const DICT_BAG_INDEX: u32 = LIST_BAG_INDEX;

fn new_list(builder: &mut FuncDefBuilder, block: BlockId, element_type: TypeId) -> Result<ValueId> {
    let cell = builder.add_new_heap_cell(block)?;
    let bag = builder.add_empty_bag(block, element_type)?;
    builder.add_make_tuple(block, &[cell, bag])
}

fn new_dict(
    builder: &mut FuncDefBuilder,
    block: BlockId,
    key_type: TypeId,
    value_type: TypeId,
) -> Result<ValueId> {
    let cell = builder.add_new_heap_cell(block)?;
    let element_type = builder.add_tuple_type(&[key_type, value_type])?;
    let bag = builder.add_empty_bag(block, element_type)?;
    builder.add_make_tuple(block, &[cell, bag])
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
