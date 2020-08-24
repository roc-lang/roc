use bumpalo::collections::Vec;
use bumpalo::Bump;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use roc_collections::all::MutMap;
use roc_module::ident::{Lowercase, TagName};
use roc_module::operator::CalledVia;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::layout::{Builtin, Layout};
use roc_parse::ast::{AssignedField, Expr};
use roc_region::all::{Located, Region};
use roc_types::subs::{Content, FlatType, Subs, Variable};
use roc_types::types::RecordField;

struct Env<'a, 'env> {
    arena: &'a Bump,
    subs: &'env Subs,
    ptr_bytes: u32,
    interns: &'env Interns,
    home: ModuleId,
}

/// JIT execute the given main function, and then wrap its results in an Expr
/// so we can display them to the user using the formatter.
///
/// We need the original types in order to properly render records and tag unions,
/// because at runtime those are structs - that is, unlabeled memory offsets.
/// By traversing the type signature while we're traversing the layout, once
/// we get to a struct or tag, we know what the labels are and can turn them
/// back into the appropriate user-facing literals.
pub unsafe fn jit_to_ast<'a>(
    arena: &'a Bump,
    execution_engine: ExecutionEngine,
    main_fn_name: &str,
    layout: &Layout<'a>,
    content: &Content,
    interns: &Interns,
    home: ModuleId,
    subs: &Subs,
    ptr_bytes: u32,
) -> Expr<'a> {
    let env = Env {
        arena,
        subs,
        ptr_bytes,
        home,
        interns,
    };

    jit_to_ast_help(&env, execution_engine, main_fn_name, layout, content)
}

macro_rules! jit_map {
    ($execution_engine: expr, $main_fn_name: expr, $ty: ty, $transform: expr) => {{
        unsafe {
            let main: JitFunction<unsafe extern "C" fn() -> $ty> = $execution_engine
                .get_function($main_fn_name)
                .ok()
                .ok_or(format!("Unable to JIT compile `{}`", $main_fn_name))
                .expect("errored");

            $transform(main.call())
        }
    }};
}

fn jit_to_ast_help<'a>(
    env: &Env<'a, '_>,
    execution_engine: ExecutionEngine,
    main_fn_name: &str,
    layout: &Layout<'a>,
    content: &Content,
) -> Expr<'a> {
    match layout {
        Layout::Builtin(Builtin::Int64) => {
            jit_map!(execution_engine, main_fn_name, i64, |num| i64_to_ast(
                env, num, content
            ))
        }
        Layout::Builtin(Builtin::Float64) => {
            jit_map!(execution_engine, main_fn_name, f64, |num| Expr::Num(
                env.arena.alloc(format!("{}", num))
            ))
        }
        Layout::Builtin(Builtin::Str) | Layout::Builtin(Builtin::EmptyStr) => jit_map!(
            execution_engine,
            main_fn_name,
            &'static str,
            |string: &'static str| { Expr::Str(env.arena.alloc(string)) }
        ),
        Layout::Builtin(Builtin::EmptyList) => {
            jit_map!(execution_engine, main_fn_name, &'static str, |_| {
                Expr::List(Vec::new_in(env.arena))
            })
        }
        Layout::Builtin(Builtin::List(_, elem_layout)) => jit_map!(
            execution_engine,
            main_fn_name,
            (*const libc::c_void, usize),
            |(ptr, len): (*const libc::c_void, usize)| {
                list_to_ast(env, ptr, len, elem_layout, content)
            }
        ),
        Layout::Struct(field_layouts) => {
            jit_map!(
                execution_engine,
                main_fn_name,
                [u8; 16 /* TODO don't hardcode this! do 2 branches based on ptr_bytes*/],
                |bytes: [u8; 16]| {
                    match content {
                        Content::Structure(FlatType::Record(fields, _)) => {
                            let ptr = (&bytes).as_ptr() as *const libc::c_void;

                            struct_to_ast(env, ptr, field_layouts, fields)
                        }
                        other => {
                            unreachable!(
                                "Something had a Struct layout, but instead of a Record type, it had: {:?}",
                                other
                            );
                        }
                    }
                }
            )
        }
        other => {
            todo!("TODO add support for rendering {:?} in the REPL", other);
        }
    }
}

fn ptr_to_ast<'a>(
    env: &Env<'a, '_>,
    ptr: *const libc::c_void,
    layout: &Layout<'a>,
    content: &Content,
) -> Expr<'a> {
    match layout {
        Layout::Builtin(Builtin::Int64) => {
            let num = unsafe { *(ptr as *const i64) };

            i64_to_ast(env, num, content)
        }
        Layout::Builtin(Builtin::Float64) => {
            let num = unsafe { *(ptr as *const f64) };

            Expr::Num(env.arena.alloc(format!("{}", num)))
        }
        Layout::Builtin(Builtin::EmptyList) => Expr::List(Vec::new_in(env.arena)),
        Layout::Builtin(Builtin::List(_, elem_layout)) => {
            // Turn the (ptr, len) wrapper struct into actual ptr and len values.
            let len = unsafe { *(ptr.offset(env.ptr_bytes as isize) as *const usize) };
            let ptr = unsafe { *(ptr as *const *const libc::c_void) };

            list_to_ast(env, ptr, len, elem_layout, content)
        }
        Layout::Builtin(Builtin::EmptyStr) => Expr::Str(""),
        Layout::Builtin(Builtin::Str) => {
            let arena_str = unsafe { *(ptr as *const &'static str) };

            Expr::Str(arena_str)
        }
        Layout::Struct(field_layouts) => match content {
            Content::Structure(FlatType::Record(fields, _)) => {
                struct_to_ast(env, ptr, field_layouts, fields)
            }
            other => {
                unreachable!(
                    "Something had a Struct layout, but instead of a Record type, it had: {:?}",
                    other
                );
            }
        },
        other => {
            todo!(
                "TODO add support for rendering pointer to {:?} in the REPL",
                other
            );
        }
    }
}

fn list_to_ast<'a>(
    env: &Env<'a, '_>,
    ptr: *const libc::c_void,
    len: usize,
    elem_layout: &Layout<'a>,
    content: &Content,
) -> Expr<'a> {
    let elem_content = match content {
        Content::Structure(FlatType::Apply(Symbol::LIST_LIST, vars)) => {
            debug_assert_eq!(vars.len(), 1);

            let elem_var = *vars.first().unwrap();

            env.subs.get_without_compacting(elem_var).content
        }
        other => {
            unreachable!(
                "Something had a Struct layout, but instead of a Record type, it had: {:?}",
                other
            );
        }
    };

    let arena = env.arena;
    let mut output = Vec::with_capacity_in(len, &arena);
    let elem_size = elem_layout.stack_size(env.ptr_bytes);

    for index in 0..(len as isize) {
        let offset_bytes: isize = index * elem_size as isize;
        let elem_ptr = unsafe { ptr.offset(offset_bytes) };
        let loc_expr = &*arena.alloc(Located {
            value: ptr_to_ast(env, elem_ptr, elem_layout, &elem_content),
            region: Region::zero(),
        });

        output.push(loc_expr);
    }

    Expr::List(output)
}

fn struct_to_ast<'a>(
    env: &Env<'a, '_>,
    ptr: *const libc::c_void,
    field_layouts: &[Layout<'a>],
    fields: &MutMap<Lowercase, RecordField<Variable>>,
) -> Expr<'a> {
    let arena = env.arena;
    let subs = env.subs;
    let mut output = Vec::with_capacity_in(field_layouts.len(), &arena);
    let mut field_ptr = ptr;

    // The fields, sorted alphabetically
    let sorted_fields = {
        let mut vec = fields
            .iter()
            .collect::<std::vec::Vec<(&Lowercase, &RecordField<Variable>)>>();

        vec.sort_by(|(label1, _), (label2, _)| label1.cmp(label2));

        vec
    };

    debug_assert_eq!(sorted_fields.len(), field_layouts.len());

    for ((label, field), field_layout) in sorted_fields.iter().zip(field_layouts.iter()) {
        let content = subs.get_without_compacting(*field.as_inner()).content;
        let loc_expr = &*arena.alloc(Located {
            value: ptr_to_ast(env, field_ptr, field_layout, &content),
            region: Region::zero(),
        });

        let field_name = Located {
            value: &*arena.alloc_str(label.as_str()),
            region: Region::zero(),
        };
        let loc_field = Located {
            value: AssignedField::RequiredValue(field_name, &[], loc_expr),
            region: Region::zero(),
        };

        output.push(loc_field);

        // Advance the field pointer to the next field.
        field_ptr = unsafe { ptr.offset(field_layout.stack_size(env.ptr_bytes) as isize) };
    }

    Expr::Record {
        update: None,
        fields: output,
    }
}

fn i64_to_ast<'a>(env: &Env<'a, '_>, num: i64, content: &Content) -> Expr<'a> {
    use Content::*;

    let arena = env.arena;

    match content {
        Structure(flat_type) => {
            match flat_type {
                FlatType::Apply(Symbol::NUM_NUM, vars) => i64_to_num_expr(arena, num),
                FlatType::Record(fields, _) => {
                    // This was a single-field record that got unwrapped at runtime.
                    // Even if it was an i64 at runtime, we still need to report
                    // it as a record with the correct field name!
                    // Its type signature will tell us that.
                    debug_assert_eq!(fields.len(), 1);

                    let (label, field) = fields.iter().next().unwrap();
                    let loc_label = Located {
                        value: &*arena.alloc_str(label.as_str()),
                        region: Region::zero(),
                    };

                    let assigned_field = {
                        // We may be multiple levels deep in nested tag unions
                        // and/or records (e.g. { a: { b: { c: 5 }  } }),
                        // so we need to do this recursively on the field type.
                        let field_var = *field.as_inner();
                        let field_content = env.subs.get_without_compacting(field_var).content;
                        let loc_expr = Located {
                            value: i64_to_ast(env, num, &field_content),
                            region: Region::zero(),
                        };

                        AssignedField::RequiredValue(loc_label, &[], arena.alloc(loc_expr))
                    };
                    let loc_assigned_field = Located {
                        value: assigned_field,
                        region: Region::zero(),
                    };

                    Expr::Record {
                        update: None,
                        fields: bumpalo::vec![in arena; loc_assigned_field],
                    }
                }
                FlatType::TagUnion(tags, _) => {
                    // This was a single-tag union that got unwrapped at runtime.
                    debug_assert_eq!(tags.len(), 1);

                    let (tag_name, payload_vars) = tags.iter().next().unwrap();

                    // If this tag union represents a number, skip right to
                    // returning tis as an Expr::Num
                    if let TagName::Private(Symbol::NUM_AT_NUM) = &tag_name {
                        return i64_to_num_expr(arena, num);
                    }

                    let loc_tag_expr = {
                        let tag_name = &tag_name.as_string(env.interns, env.home);
                        let tag_expr = if tag_name.starts_with("@") {
                            Expr::PrivateTag(arena.alloc_str(tag_name))
                        } else {
                            Expr::GlobalTag(arena.alloc_str(tag_name))
                        };

                        &*arena.alloc(Located {
                            value: tag_expr,
                            region: Region::zero(),
                        })
                    };

                    let payload = {
                        // Since this has the layout of a number, there should be
                        // exactly one payload in this tag.
                        debug_assert_eq!(payload_vars.len(), 1);

                        let var = *payload_vars.iter().next().unwrap();
                        let content = env.subs.get_without_compacting(var).content;

                        let loc_payload = &*arena.alloc(Located {
                            value: i64_to_ast(env, num, &content),
                            region: Region::zero(),
                        });

                        bumpalo::vec![in arena; loc_payload]
                    };

                    Expr::Apply(loc_tag_expr, payload, CalledVia::Space)
                }
                other => {
                    panic!("Unexpected FlatType {:?} in i64_to_ast", other);
                }
            }
        }
        Alias(_, _, var) => {
            let content = env.subs.get_without_compacting(*var).content;

            i64_to_ast(env, num, &content)
        }
        other => {
            panic!("Unexpected FlatType {:?} in i64_to_ast", other);
        }
    }
}

/// This is centralized in case we want to format it differently later,
/// e.g. adding underscores for large numbers
fn i64_to_num_expr<'a>(arena: &'a Bump, num: i64) -> Expr<'a> {
    Expr::Num(arena.alloc(format!("{}", num)))
}
