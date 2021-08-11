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

use crate::ir::{Call, CallType, Expr, Literal, ModifyRc, Proc, Stmt};
use crate::layout::{Builtin, Layout, ListLayout, UnionLayout};

// just using one module for now
pub const MOD_APP: ModName = ModName(b"UserApp");

pub const STATIC_STR_NAME: ConstName = ConstName(&Symbol::STR_ALIAS_ANALYSIS_STATIC.to_ne_bytes());

const ENTRY_POINT_NAME: &[u8] = b"mainForHost";

pub fn func_name_bytes(proc: &Proc) -> [u8; SIZE] {
    func_name_bytes_help(proc.name, proc.args.iter().map(|x| x.0), proc.ret_layout)
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
    return_layout: Layout<'a>,
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
            match layout {
                Layout::Closure(_, lambda_set, _) => {
                    lambda_set.runtime_representation().hash(&mut hasher);
                }
                _ => {
                    layout.hash(&mut hasher);
                }
            }
        }

        match return_layout {
            Layout::Closure(_, lambda_set, _) => {
                lambda_set.runtime_representation().hash(&mut hasher);
            }
            _ => {
                return_layout.hash(&mut hasher);
            }
        }

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

        // the entry point wrapper
        let roc_main_bytes = func_name_bytes_help(
            entry_point.symbol,
            entry_point.layout.arguments.iter().copied(),
            entry_point.layout.result,
        );
        let roc_main = FuncName(&roc_main_bytes);

        let entry_point_function = build_entry_point(entry_point.layout, roc_main)?;
        let entry_point_name = FuncName(ENTRY_POINT_NAME);
        m.add_func(entry_point_name, entry_point_function)?;

        let mut type_definitions = MutSet::default();

        // all other functions
        for proc in procs {
            let bytes = func_name_bytes(proc);
            let func_name = FuncName(&bytes);

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

    morphic_lib::solve(program)
}

fn build_entry_point(layout: crate::ir::ProcLayout, func_name: FuncName) -> Result<FuncDef> {
    let mut builder = FuncDefBuilder::new();
    let block = builder.add_block();

    // to the modelling language, the arguments appear out of thin air
    let argument_type = build_tuple_type(&mut builder, layout.arguments)?;
    let argument = builder.add_unknown_with(block, &[], argument_type)?;

    let name_bytes = [0; 16];
    let spec_var = CalleeSpecVar(&name_bytes);
    let result = builder.add_call(block, spec_var, MOD_APP, func_name, argument)?;

    // to the modelling language, the result disappears into the void
    let unit_type = builder.add_tuple_type(&[])?;
    let unit_value = builder.add_unknown_with(block, &[result], unit_type)?;

    let root = BlockExpr(block, unit_value);
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
        Invoke {
            symbol,
            call,
            layout: call_layout,
            pass,
            fail,
            exception_id: _,
        } => {
            // a call that might throw an exception

            let value_id = call_spec(builder, env, block, call_layout, call)?;

            let pass_block = builder.add_block();
            env.symbols.insert(*symbol, value_id);
            let pass_value_id = stmt_spec(builder, env, pass_block, layout, pass)?;
            env.symbols.remove(symbol);
            let pass_block_expr = BlockExpr(pass_block, pass_value_id);

            let fail_block = builder.add_block();
            let fail_value_id = stmt_spec(builder, env, fail_block, layout, fail)?;
            let fail_block_expr = BlockExpr(fail_block, fail_value_id);

            builder.add_choice(block, &[pass_block_expr, fail_block_expr])
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
        Resume(_) | RuntimeError(_) => {
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
            let bytes = func_name_bytes_help(*symbol, it, *ret_layout);
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
        HigherOrderLowLevel {
            specialization_id,
            closure_env_layout,
            op,
            arg_layouts,
            ret_layout,
            ..
        } => {
            let array = specialization_id.to_bytes();
            let spec_var = CalleeSpecVar(&array);

            let symbol = {
                use roc_module::low_level::LowLevel::*;

                match op {
                    ListMap | ListMapWithIndex => call.arguments[1],
                    ListMap2 => call.arguments[2],
                    ListMap3 => call.arguments[3],
                    ListWalk | ListWalkUntil | ListWalkBackwards | DictWalk => call.arguments[2],
                    ListKeepIf | ListKeepOks | ListKeepErrs => call.arguments[1],
                    ListSortWith => call.arguments[1],
                    _ => unreachable!(),
                }
            };

            let it = arg_layouts.iter().copied();
            let bytes = func_name_bytes_help(symbol, it, *ret_layout);
            let name = FuncName(&bytes);
            let module = MOD_APP;

            {
                use roc_module::low_level::LowLevel::*;

                match op {
                    DictWalk => {
                        let dict = env.symbols[&call.arguments[0]];
                        let default = env.symbols[&call.arguments[1]];
                        let closure_env = env.symbols[&call.arguments[3]];

                        let bag = builder.add_get_tuple_field(block, dict, DICT_BAG_INDEX)?;
                        let _cell = builder.add_get_tuple_field(block, dict, DICT_CELL_INDEX)?;

                        let first = builder.add_bag_get(block, bag)?;

                        let key = builder.add_get_tuple_field(block, first, 0)?;
                        let val = builder.add_get_tuple_field(block, first, 1)?;

                        let argument = if closure_env_layout.is_none() {
                            builder.add_make_tuple(block, &[key, val, default])?
                        } else {
                            builder.add_make_tuple(block, &[key, val, default, closure_env])?
                        };
                        builder.add_call(block, spec_var, module, name, argument)?;
                    }

                    ListWalk | ListWalkBackwards | ListWalkUntil => {
                        let list = env.symbols[&call.arguments[0]];
                        let default = env.symbols[&call.arguments[1]];
                        let closure_env = env.symbols[&call.arguments[3]];

                        let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
                        let _cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

                        let first = builder.add_bag_get(block, bag)?;

                        let argument = if closure_env_layout.is_none() {
                            builder.add_make_tuple(block, &[first, default])?
                        } else {
                            builder.add_make_tuple(block, &[first, default, closure_env])?
                        };
                        builder.add_call(block, spec_var, module, name, argument)?;
                    }

                    ListMapWithIndex => {
                        let list = env.symbols[&call.arguments[0]];
                        let closure_env = env.symbols[&call.arguments[2]];

                        let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
                        let _cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

                        let first = builder.add_bag_get(block, bag)?;
                        let index = builder.add_make_tuple(block, &[])?;

                        let argument = if closure_env_layout.is_none() {
                            builder.add_make_tuple(block, &[first, index])?
                        } else {
                            builder.add_make_tuple(block, &[first, index, closure_env])?
                        };
                        builder.add_call(block, spec_var, module, name, argument)?;
                    }

                    ListMap => {
                        let list1 = env.symbols[&call.arguments[0]];
                        let closure_env = env.symbols[&call.arguments[2]];

                        let bag1 = builder.add_get_tuple_field(block, list1, LIST_BAG_INDEX)?;
                        let _cell1 = builder.add_get_tuple_field(block, list1, LIST_CELL_INDEX)?;

                        let elem1 = builder.add_bag_get(block, bag1)?;

                        let argument = if closure_env_layout.is_none() {
                            builder.add_make_tuple(block, &[elem1])?
                        } else {
                            builder.add_make_tuple(block, &[elem1, closure_env])?
                        };
                        builder.add_call(block, spec_var, module, name, argument)?;
                    }

                    ListSortWith => {
                        let list1 = env.symbols[&call.arguments[0]];
                        let closure_env = env.symbols[&call.arguments[2]];

                        let bag1 = builder.add_get_tuple_field(block, list1, LIST_BAG_INDEX)?;
                        let _cell1 = builder.add_get_tuple_field(block, list1, LIST_CELL_INDEX)?;

                        let elem1 = builder.add_bag_get(block, bag1)?;

                        let argument = if closure_env_layout.is_none() {
                            builder.add_make_tuple(block, &[elem1, elem1])?
                        } else {
                            builder.add_make_tuple(block, &[elem1, elem1, closure_env])?
                        };
                        builder.add_call(block, spec_var, module, name, argument)?;
                    }

                    ListMap2 => {
                        let list1 = env.symbols[&call.arguments[0]];
                        let list2 = env.symbols[&call.arguments[1]];
                        let closure_env = env.symbols[&call.arguments[3]];

                        let bag1 = builder.add_get_tuple_field(block, list1, LIST_BAG_INDEX)?;
                        let _cell1 = builder.add_get_tuple_field(block, list1, LIST_CELL_INDEX)?;
                        let elem1 = builder.add_bag_get(block, bag1)?;

                        let bag2 = builder.add_get_tuple_field(block, list2, LIST_BAG_INDEX)?;
                        let _cell2 = builder.add_get_tuple_field(block, list2, LIST_CELL_INDEX)?;
                        let elem2 = builder.add_bag_get(block, bag2)?;

                        let argument = if closure_env_layout.is_none() {
                            builder.add_make_tuple(block, &[elem1, elem2])?
                        } else {
                            builder.add_make_tuple(block, &[elem1, elem2, closure_env])?
                        };
                        builder.add_call(block, spec_var, module, name, argument)?;
                    }

                    ListMap3 => {
                        let list1 = env.symbols[&call.arguments[0]];
                        let list2 = env.symbols[&call.arguments[1]];
                        let list3 = env.symbols[&call.arguments[2]];
                        let closure_env = env.symbols[&call.arguments[4]];

                        let bag1 = builder.add_get_tuple_field(block, list1, LIST_BAG_INDEX)?;
                        let _cell1 = builder.add_get_tuple_field(block, list1, LIST_CELL_INDEX)?;
                        let elem1 = builder.add_bag_get(block, bag1)?;

                        let bag2 = builder.add_get_tuple_field(block, list2, LIST_BAG_INDEX)?;
                        let _cell2 = builder.add_get_tuple_field(block, list2, LIST_CELL_INDEX)?;
                        let elem2 = builder.add_bag_get(block, bag2)?;

                        let bag3 = builder.add_get_tuple_field(block, list3, LIST_BAG_INDEX)?;
                        let _cell3 = builder.add_get_tuple_field(block, list3, LIST_CELL_INDEX)?;
                        let elem3 = builder.add_bag_get(block, bag3)?;

                        let argument = if closure_env_layout.is_none() {
                            builder.add_make_tuple(block, &[elem1, elem2, elem3])?
                        } else {
                            builder.add_make_tuple(block, &[elem1, elem2, elem3, closure_env])?
                        };
                        builder.add_call(block, spec_var, module, name, argument)?;
                    }

                    ListKeepIf | ListKeepOks | ListKeepErrs => {
                        let list = env.symbols[&call.arguments[0]];
                        let closure_env = env.symbols[&call.arguments[2]];

                        let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
                        // let _cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

                        let first = builder.add_bag_get(block, bag)?;

                        let argument = if closure_env_layout.is_none() {
                            builder.add_make_tuple(block, &[first])?
                        } else {
                            builder.add_make_tuple(block, &[first, closure_env])?
                        };
                        let result = builder.add_call(block, spec_var, module, name, argument)?;
                        let unit = builder.add_tuple_type(&[])?;
                        builder.add_unknown_with(block, &[result], unit)?;
                    }

                    _ => {
                        // fake a call to the function argument
                        // to make sure the function is specialized

                        // very invalid
                        let arg_value_id = build_tuple_value(builder, env, block, &[])?;

                        builder.add_call(block, spec_var, module, name, arg_value_id)?;
                    }
                }
            }

            // TODO overly pessimstic
            // filter_map because one of the arguments is a function name, which
            // is not defined in the env
            let arguments: Vec<_> = call
                .arguments
                .iter()
                .filter_map(|symbol| env.symbols.get(symbol))
                .copied()
                .collect();

            let result_type = layout_spec(builder, layout)?;

            builder.add_unknown_with(block, &arguments, result_type)
        }
    }
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
        ListAppend => {
            let list = env.symbols[&arguments[0]];
            let to_insert = env.symbols[&arguments[1]];

            let bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;
            let cell = builder.add_get_tuple_field(block, list, LIST_CELL_INDEX)?;

            let _unit = builder.add_update(block, update_mode_var, cell)?;

            // TODO new heap cell
            builder.add_bag_insert(block, bag, to_insert)?;

            let new_cell = builder.add_new_heap_cell(block)?;
            builder.add_make_tuple(block, &[new_cell, bag])
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
                    if *tag_id == *nullable_id as u8 {
                        data_id
                    } else {
                        builder.add_make_tuple(block, &[cell_id, data_id])?
                    }
                }
                UnionLayout::NullableUnwrapped { nullable_id, .. } => {
                    if *tag_id == *nullable_id as u8 {
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

            for symbol in elems.iter() {
                let value_id = env.symbols[symbol];

                bag = builder.add_bag_insert(block, bag, value_id)?;
            }

            let cell = builder.add_new_heap_cell(block)?;

            builder.add_make_tuple(block, &[cell, bag])
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
        Int(_) | Float(_) | Bool(_) | Byte(_) => builder.add_make_tuple(block, &[]),
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
        Closure(_, lambda_set, _) => layout_spec_help(
            builder,
            &lambda_set.runtime_representation(),
            when_recursive,
        ),
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
        Decimal | Float128 | Float64 | Float32 | Float16 => builder.add_tuple_type(&[]),
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

// const OK_TAG_ID: u8 = 1u8;
// const ERR_TAG_ID: u8 = 0u8;

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

fn new_num(builder: &mut FuncDefBuilder, block: BlockId) -> Result<ValueId> {
    // we model all our numbers as unit values
    builder.add_make_tuple(block, &[])
}
