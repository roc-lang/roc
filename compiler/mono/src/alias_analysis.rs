use morphic_lib::TypeContext;
use morphic_lib::{
    BlockExpr, BlockId, FuncDef, FuncDefBuilder, ModName, Result, TypeId, TypeName, ValueId,
};
use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;

use crate::ir::{Call, CallType, Expr, Literal, Proc, Stmt};
use crate::layout::{Builtin, Layout};

fn proc_spec(proc: &Proc) -> Result<FuncDef> {
    todo!()
}

fn stmt_spec(
    builder: &mut FuncDefBuilder,
    env: &mut MutMap<Symbol, ValueId>,
    block: BlockId,
    layout: &Layout,
    stmt: &Stmt,
) -> Result<ValueId> {
    use Stmt::*;

    match stmt {
        Let(symbol, expr, layout, continuation) => {
            let value_id = expr_spec(builder, env, block, layout, expr)?;
            env.insert(*symbol, value_id);
            let result = stmt_spec(builder, env, block, layout, continuation)?;
            env.remove(symbol);

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
            env.insert(*symbol, value_id);
            let pass_value_id = stmt_spec(builder, env, pass_block, layout, pass)?;
            env.remove(symbol);
            let pass_block_expr = BlockExpr(pass_block, pass_value_id);

            let fail_block = builder.add_block();
            let fail_value_id = stmt_spec(builder, env, fail_block, layout, fail)?;
            let fail_block_expr = BlockExpr(fail_block, fail_value_id);

            let cases = &[pass_block, fail_block];
            builder.add_choice(block, &[pass_block_expr, fail_block_expr])
        }
        Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => todo!(),
        Ret(symbol) => Ok(env[symbol]),
        Refcounting(_, _) => unreachable!("not yet introduced"),
        Join {
            id,
            parameters,
            continuation,
            remainder,
        } => todo!(),
        Jump(_, _) => todo!(),
        Rethrow | RuntimeError(_) => {
            let type_id = layout_spec(builder, layout)?;

            builder.add_terminate(block, type_id)
        }
    }
}

fn call_spec(
    builder: &mut FuncDefBuilder,
    env: &MutMap<Symbol, ValueId>,
    block: BlockId,
    layout: &Layout,
    call: &Call,
) -> Result<ValueId> {
    use CallType::*;
    //pub call_type: CallType<'a>,
    // pub arguments: &'a [Symbol],
    match &call.call_type {
        ByName {
            name,
            full_layout,
            ret_layout,
            arg_layouts,
        } => todo!(),
        ByPointer {
            name,
            full_layout,
            ret_layout,
            arg_layouts,
        } => todo!(),
        Foreign {
            foreign_symbol,
            ret_layout,
        } => todo!(),
        LowLevel { op } => todo!(),
    }
}

fn expr_spec(
    builder: &mut FuncDefBuilder,
    env: &MutMap<Symbol, ValueId>,
    block: BlockId,
    layout: &Layout,
    expr: &Expr,
) -> Result<ValueId> {
    use Expr::*;

    match expr {
        Literal(literal) => literal_spec(builder, block, literal),
        FunctionPointer(_, _) => todo!(),
        Call(_) => todo!(),
        Tag {
            tag_layout,
            tag_name,
            tag_id,
            union_size,
            arguments,
        } => todo!(),
        Struct(fields) => {
            let mut value_ids = Vec::new();

            for field in fields.iter() {
                let value_id = env[field];
                value_ids.push(value_id);
            }

            builder.add_make_tuple(block, &value_ids)
        }
        AccessAtIndex {
            index,
            field_layouts: _,
            structure,
            wrapped,
        } => {
            use crate::ir::Wrapped;

            let value_id = env[structure];

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
                let value_id = env[symbol];

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

const MOD_NUM: ModName = ModName(b"Num");

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
