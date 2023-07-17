use bumpalo::{collections::Vec as AVec, Bump};
use roc_module::{low_level::LowLevel, symbol::Symbol};
use roc_types::subs::Variable;

use crate::{
    borrow::Ownership,
    layout::{FunctionPointer, InLayout, LambdaName, Layout, LayoutCache, LayoutRepr},
};

use super::{
    boxed, with_hole, BranchInfo, Call, CallType, CapturedSymbols, Env, ErasedField, Expr,
    JoinPointId, Param, Procs, Stmt, UpdateModeId,
};

fn index_erased_function<'a>(
    arena: &'a Bump,
    assign_to: Symbol,
    erased_function: Symbol,
    field: ErasedField,
    layout: InLayout<'a>,
) -> impl FnOnce(Stmt<'a>) -> Stmt<'a> {
    move |rest| {
        Stmt::Let(
            assign_to,
            Expr::ErasedLoad {
                symbol: erased_function,
                field,
            },
            layout,
            arena.alloc(rest),
        )
    }
}

fn call_callee<'a>(
    arena: &'a Bump,
    result_symbol: Symbol,
    result: InLayout<'a>,
    fn_ptr_symbol: Symbol,
    fn_arg_layouts: &'a [InLayout<'a>],
    fn_arguments: &'a [Symbol],
) -> impl FnOnce(Stmt<'a>) -> Stmt<'a> {
    move |rest| {
        Stmt::Let(
            result_symbol,
            Expr::Call(Call {
                call_type: CallType::ByPointer {
                    pointer: fn_ptr_symbol,
                    ret_layout: result,
                    arg_layouts: fn_arg_layouts,
                },
                arguments: fn_arguments,
            }),
            result,
            arena.alloc(rest),
        )
    }
}

fn is_null<'a>(
    env: &mut Env<'a, '_>,
    arena: &'a Bump,
    assign_to: Symbol,
    ptr_symbol: Symbol,
    layout: InLayout<'a>,
) -> impl FnOnce(Stmt<'a>) -> Stmt<'a> {
    let null_symbol = env.unique_symbol();
    move |rest| {
        Stmt::Let(
            null_symbol,
            Expr::NullPointer,
            layout,
            arena.alloc(Stmt::Let(
                assign_to,
                Expr::Call(Call {
                    call_type: CallType::LowLevel {
                        op: LowLevel::Eq,
                        update_mode: UpdateModeId::BACKEND_DUMMY,
                    },
                    arguments: arena.alloc([ptr_symbol, null_symbol]),
                }),
                Layout::BOOL,
                arena.alloc(rest),
            )),
        )
    }
}

struct BuiltFunctionPointer<'a> {
    function_pointer: InLayout<'a>,
    reified_arguments: &'a [InLayout<'a>],
}

fn build_function_pointer<'a>(
    arena: &'a Bump,
    layout_cache: &mut LayoutCache<'a>,
    argument_layouts: &'a [InLayout<'a>],
    return_layout: InLayout<'a>,
    pass_closure: bool,
) -> BuiltFunctionPointer<'a> {
    let reified_arguments = if pass_closure {
        let mut args = AVec::with_capacity_in(argument_layouts.len() + 1, arena);
        args.extend(argument_layouts.iter().chain(&[Layout::ERASED]).copied());
        args.into_bump_slice()
    } else {
        argument_layouts
    };

    let fn_ptr_layout = LayoutRepr::FunctionPointer(FunctionPointer {
        args: reified_arguments,
        ret: return_layout,
    });

    let function_pointer = layout_cache.put_in_direct_no_semantic(fn_ptr_layout);

    BuiltFunctionPointer {
        function_pointer,
        reified_arguments,
    }
}

/// Given
///
/// ```text
/// Call(f, args)
/// ```
///
/// We generate
///
/// ```text
/// f = compile(f)
/// joinpoint join result:
///    <hole>
/// f_value: Ptr<[]> = ErasedLoad(f, .value_ptr)
/// f_callee: Ptr<[]> = ErasedLoad(f, .callee)
/// if (f_value != nullptr) {
///    f_callee = Cast(f_callee, (..params, Erased) -> ret);
///    result = f_callee ..args f
///    jump join result
/// } else {
///    f_callee = cast(f_callee, (..params) -> ret);
///    result = f_callee ..args
///    jump join result
/// }
/// ```
pub fn call_erased_function<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut Procs<'a>,
    function_expr: roc_can::expr::Expr,
    function_var: Variable,
    function_signature: (&'a [InLayout<'a>], InLayout<'a>),
    function_argument_symbols: &'a [Symbol],
    call_result_symbol: Symbol,
    hole: &'a Stmt<'a>,
    hole_layout: InLayout<'a>,
) -> Stmt<'a> {
    let arena = env.arena;
    let (f_args, f_ret) = function_signature;

    let f = env.unique_symbol();

    let join_point_id = JoinPointId(env.unique_symbol());

    // f_value = ErasedLoad(f, .value)
    let f_value = env.unique_symbol();
    let let_f_value =
        index_erased_function(arena, f_value, f, ErasedField::ValuePtr, Layout::OPAQUE_PTR);

    let mut build_closure_data_branch = |env: &mut Env, pass_closure| {
        // f_callee = Cast(f_callee, (..params) -> ret);
        // result = f_callee ..args
        // jump join result

        let BuiltFunctionPointer {
            function_pointer,
            reified_arguments: f_args,
        } = build_function_pointer(arena, layout_cache, f_args, f_ret, pass_closure);

        // f_callee = ErasedLoad(f, .callee)
        let f_callee = env.unique_symbol();
        let let_f_callee =
            index_erased_function(arena, f_callee, f, ErasedField::Callee, function_pointer);

        let function_argument_symbols = if pass_closure {
            // function_argument_symbols = ...args, f.value
            let mut args = AVec::with_capacity_in(function_argument_symbols.len() + 1, arena);
            args.extend(function_argument_symbols.iter().chain(&[f]));
            args.into_bump_slice()
        } else {
            function_argument_symbols
        };

        let result = env.unique_symbol();
        let let_result = call_callee(
            arena,
            result,
            f_ret,
            f_callee,
            f_args,
            function_argument_symbols,
        );

        let_f_callee(
            //
            let_result(
                //
                Stmt::Jump(join_point_id, arena.alloc([result])),
            ),
        )
    };

    let value_is_null = env.unique_symbol();
    let let_value_is_null = is_null(env, arena, value_is_null, f_value, Layout::OPAQUE_PTR);

    let call_and_jump_on_value = let_value_is_null(
        //
        Stmt::Switch {
            cond_symbol: value_is_null,
            cond_layout: Layout::BOOL,
            // value == null
            branches: arena.alloc([(1, BranchInfo::None, build_closure_data_branch(env, false))]),
            // value != null
            default_branch: (
                BranchInfo::None,
                arena.alloc(build_closure_data_branch(env, true)),
            ),
            ret_layout: hole_layout,
        },
    );

    let joinpoint = {
        let param = Param {
            symbol: call_result_symbol,
            layout: f_ret,
            ownership: Ownership::Owned,
        };

        let remainder = let_f_value(
            // f_value = ErasedLoad(f, .value)
            // <rest>
            call_and_jump_on_value,
        );

        Stmt::Join {
            id: join_point_id,
            parameters: env.arena.alloc([param]),
            body: hole,
            remainder: arena.alloc(remainder),
        }
    };

    // Compile the function expression into f_val
    with_hole(
        env,
        function_expr,
        function_var,
        procs,
        layout_cache,
        f,
        env.arena.alloc(joinpoint),
    )
}

/// Given
///
/// ```text
/// f = \{} -> s
/// ```
///
/// We generate
///
/// ```text
/// value = Expr::Box({s})
/// callee = Expr::FunctionPointer(f)
/// f = Expr::ErasedMake({ value, callee })
/// ```
///
/// Given
///
/// ```text
/// f = \{} -> {}
/// ```
///
/// We generate
///
/// ```text
/// callee = Expr::FunctionPointer(f)
/// f = Expr::ErasedMake({ value: nullptr, callee })
/// ```
pub fn build_erased_function<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    resolved_lambda: ResolvedErasedLambda<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let ResolvedErasedLambda {
        captures,
        lambda_name,
        arguments,
        ret,
    } = resolved_lambda;

    let value = match captures {
        None => None,
        Some(_) => Some(env.unique_symbol()),
    };

    let callee = env.unique_symbol();

    // assigned = Expr::ErasedMake({ value, callee })
    // hole <assigned>
    let result = Stmt::Let(
        assigned,
        Expr::ErasedMake { value, callee },
        Layout::ERASED,
        hole,
    );

    let BuiltFunctionPointer {
        function_pointer,
        reified_arguments: _,
    } = build_function_pointer(env.arena, layout_cache, arguments, ret, captures.is_some());

    // callee = Expr::FunctionPointer(f)
    let result = Stmt::Let(
        callee,
        Expr::FunctionPointer { lambda_name },
        function_pointer,
        env.arena.alloc(result),
    );

    // value = Expr::Box({s})
    match captures {
        None => result,
        Some(ResolvedErasedCaptures { layouts, symbols }) => {
            // captures = {...captures}
            // captures = Box(captures)
            // value = Cast(captures, void*)
            // <hole>

            let stack_captures = env.unique_symbol();
            let stack_captures_layout =
                layout_cache.put_in_direct_no_semantic(LayoutRepr::Struct(layouts));
            let stack_captures_layout = env.arena.alloc(stack_captures_layout);

            let boxed_captures_layout = layout_cache
                .put_in_direct_no_semantic(LayoutRepr::boxed_erased_value(stack_captures_layout));

            let result = Stmt::Let(
                value.unwrap(),
                boxed::box_nullable(env.arena.alloc(stack_captures), stack_captures_layout),
                boxed_captures_layout,
                env.arena.alloc(result),
            );

            let result = Stmt::Let(
                stack_captures,
                Expr::Struct(symbols),
                *stack_captures_layout,
                env.arena.alloc(result),
            );

            result
        }
    }
}

#[derive(Debug)]
struct ResolvedErasedCaptures<'a> {
    layouts: &'a [InLayout<'a>],
    symbols: &'a [Symbol],
}

#[derive(Debug)]
pub struct ResolvedErasedLambda<'a> {
    captures: Option<ResolvedErasedCaptures<'a>>,
    lambda_name: LambdaName<'a>,
    arguments: &'a [InLayout<'a>],
    ret: InLayout<'a>,
}

impl<'a> ResolvedErasedLambda<'a> {
    pub fn new(
        env: &Env<'a, '_>,
        layout_cache: &mut LayoutCache<'a>,
        lambda_symbol: Symbol,
        captures: CapturedSymbols<'a>,
        arguments: &'a [InLayout<'a>],
        ret: InLayout<'a>,
    ) -> Self {
        let resolved_captures;
        let lambda_name;
        match captures {
            CapturedSymbols::None => {
                resolved_captures = None;
                lambda_name = LambdaName::from_captures(lambda_symbol, &[]);
            }
            CapturedSymbols::Captured(captures) => {
                let layouts = {
                    let layouts = captures
                        .iter()
                        .map(|(_, var)| layout_cache.from_var(env.arena, *var, env.subs).unwrap());
                    env.arena.alloc_slice_fill_iter(layouts)
                };
                let symbols = {
                    let symbols = captures.iter().map(|(sym, _)| *sym);
                    env.arena.alloc_slice_fill_iter(symbols)
                };

                resolved_captures = Some(ResolvedErasedCaptures { layouts, symbols });
                lambda_name = LambdaName::from_captures(lambda_symbol, layouts);
            }
        };

        Self {
            captures: resolved_captures,
            lambda_name,
            arguments,
            ret,
        }
    }

    pub fn lambda_name(&self) -> LambdaName<'a> {
        self.lambda_name
    }
}

/// Given
///
/// ```text
/// proc f(...args, captures_symbol: Erased):
///     # captures = { a: A, b: B }
/// ```
///
/// We generate
///
/// ```text
/// loaded_captures: Ptr<[]> = ErasedLoad(captures_symbol, .value)
/// heap_captures: Box<{ A, B }> = Expr::Call(Lowlevel { Cast, captures_symbol })
/// stack_captures = Expr::Unbox(heap_captures)
/// a = Expr::StructAtIndex(stack_captures, 0)
/// b = Expr::StructAtIndex(stack_captures, 1)
/// <hole>
/// ```
pub fn unpack_closure_data<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    captures_symbol: Symbol,
    captures: &[(Symbol, Variable)],
    mut hole: Stmt<'a>,
) -> Stmt<'a> {
    let heap_captures = env.unique_symbol();
    let stack_captures = env.unique_symbol();

    let captures_layouts = {
        let layouts = captures
            .iter()
            .map(|(_, var)| layout_cache.from_var(env.arena, *var, env.subs).unwrap());
        &*env.arena.alloc_slice_fill_iter(layouts)
    };

    let stack_captures_layout =
        layout_cache.put_in_direct_no_semantic(LayoutRepr::Struct(captures_layouts));
    let stack_captures_layout = env.arena.alloc(stack_captures_layout);
    let heap_captures_layout = layout_cache
        .put_in_direct_no_semantic(LayoutRepr::boxed_erased_value(stack_captures_layout));

    for (i, ((capture, _capture_var), &capture_layout)) in
        captures.iter().zip(captures_layouts).enumerate().rev()
    {
        hole = Stmt::Let(
            *capture,
            Expr::StructAtIndex {
                index: i as _,
                field_layouts: captures_layouts,
                structure: stack_captures,
            },
            capture_layout,
            env.arena.alloc(hole),
        );
    }

    hole = Stmt::Let(
        stack_captures,
        boxed::unbox_nullable(heap_captures, stack_captures_layout),
        *stack_captures_layout,
        env.arena.alloc(hole),
    );

    let let_loaded_captures = index_erased_function(
        env.arena,
        heap_captures,
        captures_symbol,
        ErasedField::Value,
        heap_captures_layout,
    );

    let_loaded_captures(hole)
}
