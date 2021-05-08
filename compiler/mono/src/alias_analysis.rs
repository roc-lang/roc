use morphic_lib::TypeContext;
use morphic_lib::{
    BlockExpr, BlockId, CalleeSpecVar, FuncDef, FuncDefBuilder, FuncName, ModName, Result, TypeId,
    TypeName, ValueId,
};
use roc_collections::all::MutMap;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;

use crate::ir::{Call, CallType, Expr, Literal, Proc, Stmt};
use crate::layout::{Builtin, Layout};

// just using one module for now
const MOD_NUM: ModName = ModName(b"UserApp");
const MOD_APP: ModName = ModName(b"UserApp");

pub fn proc_spec(proc: &Proc) -> Result<FuncDef> {
    let mut builder = FuncDefBuilder::new();
    let mut env = Env::default();

    let block = builder.add_block();

    let mut argument_layouts = Vec::new();

    for (layout, _symbol) in proc.args.iter() {
        // NOTE how do I access the passed-in argument?
        // let value_id = builder.add_get_tuple_field(block, proc_argument_value_id, i as u32)?;
        // env.symbols.insert(symbol, value_id);

        argument_layouts.push(*layout);
    }

    let value_id = stmt_spec(&mut builder, &mut env, block, &proc.ret_layout, &proc.body)?;

    let root = BlockExpr(block, value_id);
    let arg_type_id = layout_spec(&mut builder, &Layout::Struct(&argument_layouts))?;
    let ret_type_id = layout_spec(&mut builder, &proc.ret_layout)?;
    builder.build(arg_type_id, ret_type_id, root)
}

#[derive(Default)]
struct Env {
    symbols: MutMap<Symbol, ValueId>,
    join_points: MutMap<crate::ir::JoinPointId, morphic_lib::JoinPointId>,
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
            // NOTE should we touch the cond_symbol here? it is always an integer, but the
            // cond_symbol might be unused if we don't somehow "use" it here

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
        Refcounting(_, _) => unreachable!("not yet introduced"),
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
            // NOTE just one value_id is returned, but we need the ids of the individual arguments!
            // symbols bound by the join point won't be defined right now!
            let (jpid, jp_argument) =
                builder.declare_join_point(block, jp_arg_type_id, ret_type_id)?;

            env.join_points.insert(*id, jpid);
            let jp_body_block = builder.add_block();

            // unpack the argument
            for (i, p) in parameters.iter().enumerate() {
                let value_id = builder.add_get_tuple_field(jp_body_block, jp_argument, i as u32)?;
                env.symbols.insert(p.symbol, value_id);
            }

            let jp_body_value_id = stmt_spec(builder, env, jp_body_block, layout, remainder)?;

            for p in parameters.iter() {
                env.symbols.remove(&p.symbol);
            }

            // NOTE I think we need to use add_sub_block here, but not sure how

            let cont_block = builder.add_block();
            let cont_value_id = stmt_spec(builder, env, cont_block, layout, continuation)?;

            env.join_points.remove(id);
            builder.define_join_point(jpid, BlockExpr(jp_body_block, jp_body_value_id))?;

            Ok(cont_value_id)
        }
        Jump(id, symbols) => {
            let ret_type_id = layout_spec(builder, layout)?;
            let argument = build_tuple_value(builder, env, block, symbols)?;

            let jpid = env.join_points[id];
            builder.add_jump(block, jpid, argument, ret_type_id)
        }
        Rethrow | RuntimeError(_) => {
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
        let value_id = env.symbols[field];
        value_ids.push(value_id);
    }

    builder.add_make_tuple(block, &value_ids)
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
            ret_layout: _,
            arg_layouts: _,
        } => {
            // TODO annotate each call with a unique identifier
            let spec_var = CalleeSpecVar(&[]);

            let arg_value_id = build_tuple_value(builder, env, block, call.arguments)?;
            let slice = &symbol.to_ne_bytes();
            let name = FuncName(slice);
            let module = MOD_APP;
            builder.add_call(block, spec_var, module, name, arg_value_id)
        }
        ByPointer {
            name: _,
            full_layout: _,
            ret_layout: _,
            arg_layouts: _,
        } => todo!(),
        Foreign {
            foreign_symbol: _,
            ret_layout: _,
        } => {
            // NOTE foreign functions are those exposed by the platform
            todo!()
        }
        LowLevel { op } => lowlevel_spec(builder, env, block, layout, op, call.arguments),
    }
}

fn lowlevel_spec(
    _builder: &mut FuncDefBuilder,
    _env: &Env,
    _block: BlockId,
    _layout: &Layout,
    _op: &LowLevel,
    _arguments: &[Symbol],
) -> Result<ValueId> {
    todo!()
}

fn expr_spec(
    builder: &mut FuncDefBuilder,
    env: &Env,
    block: BlockId,
    layout: &Layout,
    expr: &Expr,
) -> Result<ValueId> {
    use Expr::*;

    match expr {
        Literal(literal) => literal_spec(builder, block, literal),
        FunctionPointer(_, _) => todo!(),
        Call(call) => call_spec(builder, env, block, layout, call),
        Tag {
            tag_layout: _,
            tag_name: _,
            tag_id: _,
            union_size: _,
            arguments: _,
        } => todo!(),
        Struct(fields) => build_tuple_value(builder, env, block, fields),
        AccessAtIndex {
            index,
            field_layouts: _,
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
                    todo!("do we unwrap single-element records still?")
                }
                Wrapped::RecordOrSingleTagUnion => {
                    builder.add_get_tuple_field(block, value_id, *index as u32)
                }
                Wrapped::MultiTagUnion => {
                    todo!("the complicated case")
                }
            }
        }
        Array { elem_layout, elems } => {
            let type_id = layout_spec(builder, elem_layout)?;

            let mut bag = builder.add_empty_bag(block, type_id)?;

            for symbol in elems.iter() {
                let value_id = env.symbols[symbol];

                bag = builder.add_bag_insert(block, bag, value_id)?;
            }

            Ok(bag)
        }

        EmptyArray => {
            let type_id = layout_spec(builder, layout)?;

            builder.add_empty_bag(block, type_id)
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
        Str(_) => todo!("string literals are statically allocated, so what do we do?"),
        Int(_) | Float(_) | Bool(_) | Byte(_) => builder.add_make_tuple(block, &[]),
    }
}

fn layout_spec(builder: &mut FuncDefBuilder, layout: &Layout) -> Result<TypeId> {
    use Layout::*;

    match layout {
        Builtin(builtin) => Ok(builtin_spec(builder, builtin)),
        PhantomEmptyStruct => todo!(),
        Struct(_) => todo!(),
        Union(_) => todo!(),
        RecursivePointer => todo!(),
        FunctionPointer(_, _) => todo!(),
        Closure(_, _, _) => todo!(),
        Pointer(_) => todo!(),
    }
}

fn builtin_spec(builder: &mut FuncDefBuilder, builtin: &Builtin) -> TypeId {
    use Builtin::*;

    match builtin {
        Int128 => todo!(),
        Int64 => builder.add_named_type(MOD_NUM, TypeName(b"I64")),
        Int32 => todo!(),
        Int16 => todo!(),
        Int8 => todo!(),
        Int1 => todo!(),
        Usize => todo!(),
        Float128 => todo!(),
        Float64 => todo!(),
        Float32 => todo!(),
        Float16 => todo!(),
        Str => todo!(),
        Dict(_, _) => todo!(),
        Set(_) => todo!(),
        List(_, _) => todo!(),
        EmptyStr => todo!(),
        EmptyList => todo!(),
        EmptyDict => todo!(),
        EmptySet => todo!(),
    }
}
