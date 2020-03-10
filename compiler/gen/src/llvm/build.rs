use bumpalo::collections::Vec;
use bumpalo::Bump;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum::{self, *};
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use crate::llvm::convert::{
    basic_type_from_layout, collection_wrapper, get_array_type, get_fn_type,
};
use roc_collections::all::ImMap;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::expr::{Expr, Proc, Procs};
use roc_mono::layout::{Builtin, Layout};

/// This is for Inkwell's FunctionValue::verify - we want to know the verification
/// output in debug builds, but we don't want it to print to stdout in release builds!
#[cfg(debug_assertions)]
const PRINT_FN_VERIFICATION_OUTPUT: bool = true;

#[cfg(not(debug_assertions))]
const PRINT_FN_VERIFICATION_OUTPUT: bool = false;

type Scope<'a, 'ctx> = ImMap<Symbol, (Layout<'a>, PointerValue<'ctx>)>;

pub struct Env<'a, 'ctx, 'env> {
    pub arena: &'a Bump,
    pub context: &'ctx Context,
    pub builder: &'env Builder<'ctx>,
    pub module: &'ctx Module<'ctx>,
    pub interns: Interns,
    pub pointer_bytes: u32,
}

pub fn build_expr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    expr: &Expr<'a>,
    procs: &Procs<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::expr::Expr::*;

    match expr {
        Int(num) => env.context.i64_type().const_int(*num as u64, true).into(),
        Float(num) => env.context.f64_type().const_float(*num).into(),
        Cond {
            cond_lhs,
            cond_rhs,
            pass,
            fail,
            ret_layout,
            ..
        } => {
            let cond = Branch2 {
                cond_lhs,
                cond_rhs,
                pass,
                fail,
                ret_layout: ret_layout.clone(),
            };

            build_branch2(env, scope, parent, cond, procs)
        }
        Branches { .. } => {
            panic!("TODO build_branches(env, scope, parent, cond_lhs, branches, procs)");
        }
        Switch {
            cond,
            branches,
            default_branch,
            ret_layout,
            cond_layout,
        } => {
            let ret_type = basic_type_from_layout(env.context, &ret_layout);
            let switch_args = SwitchArgs {
                cond_layout: cond_layout.clone(),
                cond_expr: cond,
                branches,
                default_branch,
                ret_type,
            };

            build_switch(env, scope, parent, switch_args, procs)
        }
        Store(stores, ret) => {
            let mut scope = im_rc::HashMap::clone(scope);
            let context = &env.context;

            for (symbol, layout, expr) in stores.iter() {
                let val = build_expr(env, &scope, parent, &expr, procs);
                let expr_bt = basic_type_from_layout(context, &layout);
                let alloca = create_entry_block_alloca(
                    env,
                    parent,
                    expr_bt,
                    symbol.ident_string(&env.interns),
                );

                env.builder.build_store(alloca, val);

                // Make a new scope which includes the binding we just encountered.
                // This should be done *after* compiling the bound expr, since any
                // recursive (in the LetRec sense) bindings should already have
                // been extracted as procedures. Nothing in here should need to
                // access itself!
                scope = im_rc::HashMap::clone(&scope);

                scope.insert(*symbol, (layout.clone(), alloca));
            }

            build_expr(env, &scope, parent, ret, procs)
        }
        CallByName(symbol, args) => match *symbol {
            Symbol::BOOL_OR => {
                panic!("TODO create a phi node for ||");
            }
            Symbol::BOOL_AND => {
                panic!("TODO create a phi node for &&");
            }
            _ => {
                let mut arg_vals: Vec<BasicValueEnum> =
                    Vec::with_capacity_in(args.len(), env.arena);

                for (arg, _layout) in args.iter() {
                    arg_vals.push(build_expr(env, scope, parent, arg, procs));
                }

                call_with_args(*symbol, arg_vals.into_bump_slice(), env)
            }
        },
        FunctionPointer(symbol) => {
            let ptr = env
                .module
                .get_function(symbol.ident_string(&env.interns))
                .unwrap_or_else(|| panic!("Could not get pointer to unknown function {:?}", symbol))
                .as_global_value()
                .as_pointer_value();

            BasicValueEnum::PointerValue(ptr)
        }
        CallByPointer(sub_expr, args, _var) => {
            let mut arg_vals: Vec<BasicValueEnum> = Vec::with_capacity_in(args.len(), env.arena);

            for arg in args.iter() {
                arg_vals.push(build_expr(env, scope, parent, arg, procs));
            }

            let call = match build_expr(env, scope, parent, sub_expr, procs) {
                BasicValueEnum::PointerValue(ptr) => {
                    env.builder.build_call(ptr, arg_vals.as_slice(), "tmp")
                }
                non_ptr => {
                    panic!(
                        "Tried to call by pointer, but encountered a non-pointer: {:?}",
                        non_ptr
                    );
                }
            };

            call.try_as_basic_value()
                .left()
                .unwrap_or_else(|| panic!("LLVM error: Invalid call by pointer."))
        }

        Load(symbol) => match scope.get(symbol) {
            Some((_, ptr)) => env
                .builder
                .build_load(*ptr, symbol.ident_string(&env.interns)),
            None => panic!("Could not find a var for {:?} in scope {:?}", symbol, scope),
        },
        Str(str_literal) => {
            if str_literal.is_empty() {
                panic!("TODO build an empty string in LLVM");
            } else {
                let ctx = env.context;
                let builder = env.builder;
                let bytes_len = str_literal.len() + 1/* TODO drop the +1 when we have structs and this is no longer a NUL-terminated CString.*/;

                let byte_type = ctx.i8_type();
                let nul_terminator = byte_type.const_zero();
                let len = ctx.i32_type().const_int(bytes_len as u64, false);
                let ptr = env
                    .builder
                    .build_array_malloc(ctx.i8_type(), len, "str_ptr")
                    .unwrap();

                // Copy the bytes from the string literal into the array
                for (index, byte) in str_literal.bytes().enumerate() {
                    let index = ctx.i32_type().const_int(index as u64, false);
                    let elem_ptr = unsafe { builder.build_gep(ptr, &[index], "byte") };

                    builder.build_store(elem_ptr, byte_type.const_int(byte as u64, false));
                }

                // Add a NUL terminator at the end.
                // TODO: Instead of NUL-terminating, return a struct
                // with the pointer and also the length and capacity.
                let index = ctx.i32_type().const_int(bytes_len as u64 - 1, false);
                let elem_ptr = unsafe { builder.build_gep(ptr, &[index], "nul_terminator") };

                builder.build_store(elem_ptr, nul_terminator);

                BasicValueEnum::PointerValue(ptr)
            }
        }
        Array { elem_layout, elems } => {
            let ctx = env.context;
            let elem_type = basic_type_from_layout(ctx, elem_layout);
            let builder = env.builder;

            if elems.is_empty() {
                let array_type = get_array_type(&elem_type, 0);
                let ptr_type = array_type.ptr_type(AddressSpace::Generic);
                let struct_type = collection_wrapper(ctx, ptr_type);

                // The first field in the struct should be the pointer.
                let struct_val = builder
                    .build_insert_value(
                        struct_type.const_zero(),
                        BasicValueEnum::PointerValue(ptr_type.const_null()),
                        Builtin::WRAPPER_PTR,
                        "insert_ptr",
                    )
                    .unwrap();

                BasicValueEnum::StructValue(struct_val.into_struct_value())
            } else {
                let len_u64 = elems.len() as u64;
                let elem_bytes = elem_layout.stack_size(env.pointer_bytes) as u64;

                let ptr = {
                    let bytes_len = elem_bytes * len_u64;
                    let len = ctx.i32_type().const_int(bytes_len, false);

                    env.builder
                        .build_array_malloc(elem_type, len, "create_list_ptr")
                        .unwrap()
                };

                // Copy the elements from the list literal into the array
                for (index, elem) in elems.iter().enumerate() {
                    let offset = ctx.i32_type().const_int(elem_bytes * index as u64, false);
                    let elem_ptr = unsafe { builder.build_gep(ptr, &[offset], "elem") };

                    let val = build_expr(env, &scope, parent, &elem, procs);

                    builder.build_store(elem_ptr, val);
                }

                let ptr_val = BasicValueEnum::PointerValue(ptr);
                let struct_type = collection_wrapper(ctx, ptr.get_type());
                let len = BasicValueEnum::IntValue(ctx.i32_type().const_int(len_u64, false));
                let mut struct_val;

                // Field 0: pointer
                struct_val = builder
                    .build_insert_value(
                        struct_type.const_zero(),
                        ptr_val,
                        Builtin::WRAPPER_PTR,
                        "insert_ptr",
                    )
                    .unwrap();

                // Field 1: length
                struct_val = builder
                    .build_insert_value(struct_val, len, Builtin::WRAPPER_LEN, "insert_len")
                    .unwrap();

                // Field 2: capacity (initially set to length)
                struct_val = builder
                    .build_insert_value(
                        struct_val,
                        len,
                        Builtin::WRAPPER_CAPACITY,
                        "insert_capacity",
                    )
                    .unwrap();

                BasicValueEnum::StructValue(struct_val.into_struct_value())
            }
        }
        _ => {
            panic!("I don't yet know how to LLVM build {:?}", expr);
        }
    }
}

struct Branch2<'a> {
    cond_lhs: &'a Expr<'a>,
    cond_rhs: &'a Expr<'a>,
    pass: &'a Expr<'a>,
    fail: &'a Expr<'a>,
    ret_layout: Layout<'a>,
}

fn build_branch2<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    cond: Branch2<'a>,
    procs: &Procs<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ret_layout = cond.ret_layout;
    let ret_type = basic_type_from_layout(env.context, &ret_layout);

    let lhs = build_expr(env, scope, parent, cond.cond_lhs, procs);
    let rhs = build_expr(env, scope, parent, cond.cond_rhs, procs);

    match (lhs, rhs) {
        (FloatValue(lhs_float), FloatValue(rhs_float)) => {
            let comparison =
                builder.build_float_compare(FloatPredicate::OEQ, lhs_float, rhs_float, "cond");

            build_phi2(
                env, scope, parent, comparison, cond.pass, cond.fail, ret_type, procs,
            )
        }

        (IntValue(lhs_int), IntValue(rhs_int)) => {
            let comparison = builder.build_int_compare(IntPredicate::EQ, lhs_int, rhs_int, "cond");

            build_phi2(
                env, scope, parent, comparison, cond.pass, cond.fail, ret_type, procs,
            )
        }
        _ => panic!(
            "Tried to make a branch out of incompatible conditions: lhs = {:?} and rhs = {:?}",
            cond.cond_lhs, cond.cond_rhs
        ),
    }
}

struct SwitchArgs<'a, 'ctx> {
    pub cond_expr: &'a Expr<'a>,
    pub cond_layout: Layout<'a>,
    pub branches: &'a [(u64, Expr<'a>)],
    pub default_branch: &'a Expr<'a>,
    pub ret_type: BasicTypeEnum<'ctx>,
}

fn build_switch<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    switch_args: SwitchArgs<'a, 'ctx>,
    procs: &Procs<'a>,
) -> BasicValueEnum<'ctx> {
    let arena = env.arena;
    let builder = env.builder;
    let context = env.context;
    let SwitchArgs {
        branches,
        cond_expr,
        default_branch,
        ret_type,
        ..
    } = switch_args;

    let cont_block = context.append_basic_block(parent, "cont");

    // Build the condition
    let cond = build_expr(env, scope, parent, cond_expr, procs).into_int_value();

    // Build the cases
    let mut incoming = Vec::with_capacity_in(branches.len(), arena);
    let mut cases = Vec::with_capacity_in(branches.len(), arena);

    for (int, _) in branches.iter() {
        let int_val = context.i64_type().const_int(*int as u64, false);
        let block = context.append_basic_block(parent, format!("branch{}", int).as_str());

        cases.push((int_val, block));
    }

    let default_block = context.append_basic_block(parent, "default");

    builder.build_switch(cond, default_block, &cases);

    for ((_, branch_expr), (_, block)) in branches.iter().zip(cases) {
        builder.position_at_end(block);

        let branch_val = build_expr(env, scope, parent, branch_expr, procs);

        builder.build_unconditional_branch(cont_block);

        incoming.push((branch_val, block));
    }

    // The block for the conditional's default branch.
    builder.position_at_end(default_block);

    let default_val = build_expr(env, scope, parent, default_branch, procs);

    builder.build_unconditional_branch(cont_block);

    incoming.push((default_val, default_block));

    // emit merge block
    builder.position_at_end(cont_block);

    let phi = builder.build_phi(ret_type, "branch");

    for (branch_val, block) in incoming {
        phi.add_incoming(&[(&Into::<BasicValueEnum>::into(branch_val), block)]);
    }

    phi.as_basic_value()
}

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
fn build_phi2<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    comparison: IntValue<'ctx>,
    pass: &'a Expr<'a>,
    fail: &'a Expr<'a>,
    ret_type: BasicTypeEnum<'ctx>,
    procs: &Procs<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let context = env.context;

    // build blocks
    let then_block = context.append_basic_block(parent, "then");
    let else_block = context.append_basic_block(parent, "else");
    let cont_block = context.append_basic_block(parent, "branchcont");

    builder.build_conditional_branch(comparison, then_block, else_block);

    // build then block
    builder.position_at_end(then_block);
    let then_val = build_expr(env, scope, parent, pass, procs);
    builder.build_unconditional_branch(cont_block);

    let then_block = builder.get_insert_block().unwrap();

    // build else block
    builder.position_at_end(else_block);
    let else_val = build_expr(env, scope, parent, fail, procs);
    builder.build_unconditional_branch(cont_block);

    let else_block = builder.get_insert_block().unwrap();

    // emit merge block
    builder.position_at_end(cont_block);

    let phi = builder.build_phi(ret_type, "branch");

    phi.add_incoming(&[
        (&Into::<BasicValueEnum>::into(then_val), then_block),
        (&Into::<BasicValueEnum>::into(else_val), else_block),
    ]);

    phi.as_basic_value()
}

/// TODO could this be added to Inkwell itself as a method on BasicValueEnum?
fn set_name(bv_enum: BasicValueEnum<'_>, name: &str) {
    match bv_enum {
        ArrayValue(val) => val.set_name(name),
        IntValue(val) => val.set_name(name),
        FloatValue(val) => val.set_name(name),
        PointerValue(val) => val.set_name(name),
        StructValue(val) => val.set_name(name),
        VectorValue(val) => val.set_name(name),
    }
}

/// Creates a new stack allocation instruction in the entry block of the function.
pub fn create_entry_block_alloca<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    parent: FunctionValue<'_>,
    basic_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let builder = env.context.create_builder();
    let entry = parent.get_first_basic_block().unwrap();

    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(entry),
    }

    builder.build_alloca(basic_type, name)
}

pub fn build_proc_header<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    symbol: Symbol,
    proc: &Proc<'a>,
) -> (FunctionValue<'ctx>, Vec<'a, BasicTypeEnum<'ctx>>) {
    let args = proc.args;
    let arena = env.arena;
    let context = &env.context;
    let ret_type = basic_type_from_layout(context, &proc.ret_layout);
    let mut arg_basic_types = Vec::with_capacity_in(args.len(), arena);
    let mut arg_symbols = Vec::new_in(arena);

    for (layout, arg_symbol) in args.iter() {
        let arg_type = basic_type_from_layout(env.context, &layout);

        arg_basic_types.push(arg_type);
        arg_symbols.push(arg_symbol);
    }

    let fn_type = get_fn_type(&ret_type, &arg_basic_types);

    let fn_val = env.module.add_function(
        symbol.ident_string(&env.interns),
        fn_type,
        Some(Linkage::Private),
    );

    (fn_val, arg_basic_types)
}

pub fn build_proc<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    proc: Proc<'a>,
    procs: &Procs<'a>,
    fn_val: FunctionValue<'ctx>,
    arg_basic_types: Vec<'a, BasicTypeEnum<'ctx>>,
) {
    let args = proc.args;
    let context = &env.context;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");
    let builder = env.builder;

    builder.position_at_end(entry);

    let mut scope = ImMap::default();

    // Add args to scope
    for ((arg_val, arg_type), (layout, arg_symbol)) in
        fn_val.get_param_iter().zip(arg_basic_types).zip(args)
    {
        set_name(arg_val, arg_symbol.ident_string(&env.interns));

        let alloca =
            create_entry_block_alloca(env, fn_val, arg_type, arg_symbol.ident_string(&env.interns));

        builder.build_store(alloca, arg_val);

        scope.insert(*arg_symbol, (layout.clone(), alloca));
    }

    let body = build_expr(env, &scope, fn_val, &proc.body, procs);

    builder.build_return(Some(&body));
}

pub fn verify_fn(fn_val: FunctionValue<'_>) {
    if !fn_val.verify(PRINT_FN_VERIFICATION_OUTPUT) {
        unsafe {
            fn_val.delete();
        }

        panic!("Invalid generated fn_val.")
    }
}

#[inline(always)]
fn call_with_args<'a, 'ctx, 'env>(
    symbol: Symbol,
    args: &[BasicValueEnum<'ctx>],
    env: &Env<'a, 'ctx, 'env>,
) -> BasicValueEnum<'ctx> {
    match symbol {
        Symbol::NUM_ADD => {
            debug_assert!(args.len() == 2);

            let int_val = env.builder.build_int_add(
                args[0].into_int_value(),
                args[1].into_int_value(),
                "ADD_I64",
            );

            BasicValueEnum::IntValue(int_val)
        }
        Symbol::NUM_SUB => {
            debug_assert!(args.len() == 2);

            let int_val = env.builder.build_int_sub(
                args[0].into_int_value(),
                args[1].into_int_value(),
                "SUB_I64",
            );

            BasicValueEnum::IntValue(int_val)
        }
        Symbol::NUM_MUL => {
            debug_assert!(args.len() == 2);

            let int_val = env.builder.build_int_mul(
                args[0].into_int_value(),
                args[1].into_int_value(),
                "MUL_I64",
            );

            BasicValueEnum::IntValue(int_val)
        }
        Symbol::NUM_NEG => {
            debug_assert!(args.len() == 1);

            let int_val = env
                .builder
                .build_int_neg(args[0].into_int_value(), "NEGATE_I64");

            BasicValueEnum::IntValue(int_val)
        }
        Symbol::LIST_LEN => {
            debug_assert!(args.len() == 1);

            let wrapper_struct = args[0].into_struct_value();
            let builder = env.builder;

            // Get the 32-bit int length
            let i32_val = builder.build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "unwrapped_list_len").unwrap().into_int_value();

            // cast the 32-bit length to a 64-bit int
            BasicValueEnum::IntValue(builder.build_int_cast(i32_val, env.context.i64_type(), "i32_to_i64"))
        }
        Symbol::LIST_IS_EMPTY => {
            debug_assert!(args.len() == 1);

            let list_struct = args[0].into_struct_value();
            let builder = env.builder;
            let list_len = builder.build_extract_value(list_struct, 1, "unwrapped_list_len").unwrap().into_int_value();
            let zero = env.context.i32_type().const_zero();
            let answer = builder.build_int_compare(IntPredicate::EQ, list_len, zero, "is_zero");

            BasicValueEnum::IntValue(answer)
        }
        Symbol::LIST_GET_UNSAFE => {
            let builder = env.builder;

            // List.get : List elem, Int -> Result elem [ OutOfBounds ]*
            debug_assert!(args.len() == 2);

            let wrapper_struct = args[0].into_struct_value();
            let elem_index = args[1].into_int_value();

            // Slot 1 in the wrapper struct is the length
            let _list_len = builder.build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "unwrapped_list_len").unwrap().into_int_value();

            // TODO here, check to see if the requested index exceeds the length of the array.

            // Slot 0 in the wrapper struct is the pointer to the array data
            let array_data_ptr = builder.build_extract_value(wrapper_struct, Builtin::WRAPPER_PTR, "unwrapped_list_ptr").unwrap().into_pointer_value();

            let elem_bytes = 8; // TODO Look this size up instead of hardcoding it!
            let elem_size = env.context.i64_type().const_int(elem_bytes, false);

            // Calculate the offset at runtime by multiplying the index by the size of an element.
            let offset_bytes = builder.build_int_mul(elem_index, elem_size, "mul_offset");

            // We already checked the bounds earlier.
            let elem_ptr = unsafe { builder.build_in_bounds_gep(array_data_ptr, &[offset_bytes], "elem") };

            builder.build_load(elem_ptr, "List.get")
        }
        Symbol::LIST_SET /* TODO clone first for LIST_SET! */ | Symbol::LIST_SET_IN_PLACE => {
            let builder = env.builder;

            debug_assert!(args.len() == 3);

            let wrapper_struct = args[0].into_struct_value();
            let elem_index = args[1].into_int_value();
            let elem = args[2];

            // Slot 1 in the wrapper struct is the length
            let _list_len = builder.build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "unwrapped_list_len").unwrap().into_int_value();

            // TODO here, check to see if the requested index exceeds the length of the array.
            // If so, bail out and return the list unaltered.

            // Slot 0 in the wrapper struct is the pointer to the array data
            let array_data_ptr = builder.build_extract_value(wrapper_struct, Builtin::WRAPPER_PTR, "unwrapped_list_ptr").unwrap().into_pointer_value();

            let elem_bytes = 8; // TODO Look this size up instead of hardcoding it!
            let elem_size = env.context.i64_type().const_int(elem_bytes, false);

            // Calculate the offset at runtime by multiplying the index by the size of an element.
            let offset_bytes = builder.build_int_mul(elem_index, elem_size, "mul_offset");

            // We already checked the bounds earlier.
            let elem_ptr = unsafe { builder.build_in_bounds_gep(array_data_ptr, &[offset_bytes], "elem") };

            // Mutate the array in-place.
            builder.build_store(elem_ptr, elem);

            // Return the wrapper unchanged, since pointer, length and capacity are all unchanged
            wrapper_struct.into()
        }
        _ => {
            let fn_val = env
                .module
                .get_function(symbol.ident_string(&env.interns))
                .unwrap_or_else(|| panic!("Unrecognized function: {:?}", symbol));

            let call = env.builder.build_call(fn_val, args, "tmp");

            call.try_as_basic_value()
                .left()
                .unwrap_or_else(|| panic!("LLVM error: Invalid call by name for name {:?}", symbol))
        }
    }
}
