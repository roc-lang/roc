use morphic_lib::TypeContext;
use morphic_lib::{
    BlockExpr, BlockId, CalleeSpecVar, ConstDefBuilder, ConstName, EntryPointName, ExprContext,
    FuncDef, FuncDefBuilder, FuncName, ModDefBuilder, ModName, ProgramBuilder, Result, TypeId,
    UpdateModeVar, ValueId,
};
use roc_collections::all::MutMap;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use std::convert::TryFrom;

use crate::ir::{Call, CallType, Expr, Literal, ModifyRc, Proc, Stmt};
use crate::layout::{Builtin, Layout, ListLayout, UnionLayout};

// just using one module for now
pub const MOD_APP: ModName = ModName(b"UserApp");

pub const STATIC_STR_NAME: ConstName = ConstName(&Symbol::STR_ALIAS_ANALYSIS_STATIC.to_ne_bytes());

pub fn func_name_bytes(proc: &Proc) -> [u8; 16] {
    func_name_bytes_help(proc.name, proc.args.iter().map(|x| x.0), proc.ret_layout)
}

pub fn func_name_bytes_help<'a, I>(
    symbol: Symbol,
    argument_layouts: I,
    return_layout: Layout<'a>,
) -> [u8; 16]
where
    I: Iterator<Item = Layout<'a>>,
{
    let mut name_bytes = [0u8; 16];

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

    return name_bytes;
}

pub fn spec_program<'a, I>(procs: I) -> Result<morphic_lib::Solutions>
where
    I: Iterator<Item = &'a Proc<'a>>,
{
    let mut main_function = None;
    let main_module = {
        let mut m = ModDefBuilder::new();

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

        for proc in procs {
            let spec = proc_spec(proc)?;

            m.add_func(FuncName(&func_name_bytes(proc)), spec)?;

            if format!("{:?}", proc.name).contains("mainForHost") {
                main_function = Some(proc.name);
            }

            if format!("{:?}", proc.name).contains("replOutput") {
                main_function = Some(proc.name);
            }
        }

        m.build()?
    };

    match main_function {
        None => {
            let program = {
                let mut p = ProgramBuilder::new();
                p.add_mod(MOD_APP, main_module)?;
                p.add_entry_point(
                    EntryPointName(b"not defined! probably a function in the repl"),
                    MOD_APP,
                    FuncName(&[]),
                )?;

                p.build()?
            };

            morphic_lib::solve(program)
        }
        Some(main_function) => {
            let program = {
                let mut p = ProgramBuilder::new();
                p.add_mod(MOD_APP, main_module)?;
                p.add_entry_point(
                    EntryPointName(b"mainForHost"),
                    MOD_APP,
                    FuncName(&main_function.to_ne_bytes()),
                )?;

                p.build()?
            };

            // eprintln!("{}", program.to_source_string());

            morphic_lib::solve(program)
        }
    }
}

fn proc_spec(proc: &Proc) -> Result<FuncDef> {
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

    Ok(spec)
}

#[derive(Default)]
struct Env {
    symbols: MutMap<Symbol, ValueId>,
    join_points: MutMap<crate::ir::JoinPointId, morphic_lib::ContinuationId>,
}

fn stmt_spec(
    builder: &mut FuncDefBuilder,
    env: &mut Env,
    block: BlockId,
    layout: &Layout,
    stmt: &Stmt,
) -> Result<ValueId> {
    use Stmt::*;

    match stmt {
        Let(symbol, expr, layout, continuation) => {
            let value_id = expr_spec(builder, env, block, layout, expr)?;
            env.symbols.insert(*symbol, value_id);
            let result = stmt_spec(builder, env, block, layout, continuation)?;
            env.symbols.remove(symbol);

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
            ret_layout,
        } => {
            let mut cases = Vec::with_capacity(branches.len() + 1);

            let it = branches
                .iter()
                .map(|(_, _, body)| body)
                .chain(std::iter::once(default_branch.1));

            for branch in it {
                let block = builder.add_block();
                let value_id = stmt_spec(builder, env, block, ret_layout, branch)?;
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
            continuation,
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

            let join_body_sub_block = {
                env.join_points.insert(*id, jpid);
                let jp_body_block = builder.add_block();

                // unpack the argument
                for (i, p) in parameters.iter().enumerate() {
                    let value_id =
                        builder.add_get_tuple_field(jp_body_block, jp_argument, i as u32)?;
                    env.symbols.insert(p.symbol, value_id);
                }

                let jp_body_value_id = stmt_spec(builder, env, jp_body_block, layout, remainder)?;
                BlockExpr(jp_body_block, jp_body_value_id)
            };

            // NOTE the symbols bound by the join point can shadow the argument symbols of the
            // surrounding function, so we don't remove them from the env here

            let cont_block = builder.add_block();
            let cont_value_id = stmt_spec(builder, env, cont_block, layout, continuation)?;

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

fn build_tuple_type(builder: &mut FuncDefBuilder, layouts: &[Layout]) -> Result<TypeId> {
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
            full_layout: _,
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
        HigherOrderLowLevel { .. } => {
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
        Eq | NotEq => {
            // just dream up a unit value
            builder.add_make_tuple(block, &[])
        }
        NumLte | NumLt | NumGt | NumGte => {
            // just dream up a unit value
            builder.add_make_tuple(block, &[])
        }
        ListLen => {
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

            Ok(list)
        }
        _other => {
            // TODO overly pessimstic
            let arguments: Vec<_> = arguments.iter().map(|symbol| env.symbols[symbol]).collect();

            let result_type = layout_spec(builder, layout)?;

            builder.add_unknown_with(block, &arguments, result_type)
        }
    }
}

fn build_variant_types(
    builder: &mut FuncDefBuilder,
    union_layout: &UnionLayout,
) -> Result<Vec<TypeId>> {
    use UnionLayout::*;

    let mut result = Vec::new();

    match union_layout {
        NonRecursive(tags) => {
            for tag in tags.iter() {
                result.push(build_tuple_type(builder, tag)?);
            }
        }
        Recursive(_) => unreachable!(),
        NonNullableUnwrapped(_) => unreachable!(),
        NullableWrapped {
            nullable_id: _,
            other_tags: _,
        } => unreachable!(),
        NullableUnwrapped {
            nullable_id: _,
            other_fields: _,
        } => unreachable!(),
    }

    Ok(result)
}

fn worst_case_type(context: &mut impl TypeContext) -> Result<TypeId> {
    let cell = context.add_heap_cell_type();
    context.add_bag_type(cell)
}

fn expr_spec(
    builder: &mut FuncDefBuilder,
    env: &mut Env,
    block: BlockId,
    layout: &Layout,
    expr: &Expr,
) -> Result<ValueId> {
    use Expr::*;

    match expr {
        Literal(literal) => literal_spec(builder, block, literal),
        Call(call) => call_spec(builder, env, block, layout, call),
        Tag {
            tag_layout,
            tag_name: _,
            tag_id,
            union_size: _,
            arguments,
        } => match tag_layout {
            UnionLayout::NonRecursive(_) => {
                let value_id = build_tuple_value(builder, env, block, arguments)?;
                let variant_types = build_variant_types(builder, tag_layout)?;
                builder.add_make_union(block, &variant_types, *tag_id as u32, value_id)
            }
            UnionLayout::Recursive(_)
            | UnionLayout::NonNullableUnwrapped(_)
            | UnionLayout::NullableWrapped { .. }
            | UnionLayout::NullableUnwrapped { .. } => {
                let result_type = worst_case_type(builder)?;
                let value_id = build_tuple_value(builder, env, block, arguments)?;
                builder.add_unknown_with(block, &[value_id], result_type)
            }
        },
        Struct(fields) => build_tuple_value(builder, env, block, fields),
        AccessAtIndex {
            index,
            field_layouts,
            structure,
            wrapped,
        } => {
            use crate::ir::Wrapped;

            let value_id = env.symbols[structure];

            match wrapped {
                Wrapped::EmptyRecord => {
                    // this is a unit value
                    builder.add_make_tuple(block, &[])
                }
                Wrapped::SingleElementRecord => {
                    // builder.add_get_tuple_field(block, value_id, *index as u32)
                    Ok(env.symbols[structure])
                }
                Wrapped::RecordOrSingleTagUnion => {
                    builder.add_get_tuple_field(block, value_id, *index as u32)
                }
                Wrapped::MultiTagUnion => {
                    // Clearly this is not generally correct, but it should be for our examples
                    let hacky_is_recursive =
                        field_layouts.iter().any(|l| l == &Layout::RecursivePointer);

                    if hacky_is_recursive {
                        let result_type = layout_spec(builder, layout)?;
                        builder.add_unknown_with(block, &[value_id], result_type)
                    } else {
                        builder.add_get_tuple_field(block, value_id, *index as u32)
                    }
                }
            }
        }
        Array { elem_layout, elems } => {
            let type_id = layout_spec(builder, elem_layout)?;

            let list = new_list(builder, block, type_id)?;

            let mut bag = builder.add_get_tuple_field(block, list, LIST_BAG_INDEX)?;

            for symbol in elems.iter() {
                let value_id = env.symbols[symbol];

                bag = builder.add_bag_insert(block, bag, value_id)?;
            }

            Ok(bag)
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
        Reuse { .. } => todo!("currently unused"),
        Reset(_) => todo!("currently unused"),
        RuntimeErrorFunction(_) => {
            let type_id = layout_spec(builder, layout)?;

            builder.add_terminate(block, type_id)
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
        Int(_) | Float(_) | Bool(_) | Byte(_) => builder.add_make_tuple(block, &[]),
    }
}

fn layout_spec(builder: &mut FuncDefBuilder, layout: &Layout) -> Result<TypeId> {
    use Layout::*;

    match layout {
        Builtin(builtin) => builtin_spec(builder, builtin),
        Struct(fields) => build_tuple_type(builder, fields),
        Union(union_layout) => match union_layout {
            UnionLayout::NonRecursive(_) => {
                let variant_types = build_variant_types(builder, union_layout)?;
                builder.add_union_type(&variant_types)
            }
            UnionLayout::Recursive(_) => worst_case_type(builder),
            UnionLayout::NonNullableUnwrapped(_) => worst_case_type(builder),
            UnionLayout::NullableWrapped {
                nullable_id: _,
                other_tags: _,
            } => worst_case_type(builder),
            UnionLayout::NullableUnwrapped {
                nullable_id: _,
                other_fields: _,
            } => worst_case_type(builder),
        },
        RecursivePointer => worst_case_type(builder),
        FunctionPointer(_, _) => todo!(),
        Closure(_, lambda_set, _) => layout_spec(builder, &lambda_set.runtime_representation()),
    }
}

fn builtin_spec(builder: &mut FuncDefBuilder, builtin: &Builtin) -> Result<TypeId> {
    use Builtin::*;

    match builtin {
        Int128 | Int64 | Int32 | Int16 | Int8 | Int1 | Usize => builder.add_tuple_type(&[]),
        Float128 | Float64 | Float32 | Float16 => builder.add_tuple_type(&[]),
        Str | EmptyStr => str_type(builder),
        Dict(key_layout, value_layout) => {
            let value_type = layout_spec(builder, value_layout)?;
            let key_type = layout_spec(builder, key_layout)?;
            let element_type = builder.add_tuple_type(&[key_type, value_type])?;

            let cell = builder.add_heap_cell_type();
            let bag = builder.add_bag_type(element_type)?;
            builder.add_tuple_type(&[cell, bag])
        }
        Set(key_layout) => {
            let value_type = builder.add_tuple_type(&[])?;
            let key_type = layout_spec(builder, key_layout)?;
            let element_type = builder.add_tuple_type(&[key_type, value_type])?;

            let cell = builder.add_heap_cell_type();
            let bag = builder.add_bag_type(element_type)?;
            builder.add_tuple_type(&[cell, bag])
        }
        List(element_layout) => {
            let element_type = layout_spec(builder, element_layout)?;

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
    let len_id = builder.add_tuple_type(&[])?;
    builder.add_tuple_type(&[cell_id, len_id])
}

// const OK_TAG_ID: u8 = 1u8;
// const ERR_TAG_ID: u8 = 0u8;

const LIST_CELL_INDEX: u32 = 0;
const LIST_BAG_INDEX: u32 = 1;

fn new_list(builder: &mut FuncDefBuilder, block: BlockId, element_type: TypeId) -> Result<ValueId> {
    let cell = builder.add_new_heap_cell(block)?;
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
