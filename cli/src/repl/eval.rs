use bumpalo::collections::Vec;
use bumpalo::Bump;
use libloading::Library;
use roc_collections::all::MutMap;
use roc_gen::{run_jit_function, run_jit_function_dynamic_type};
use roc_module::ident::{Lowercase, TagName};
use roc_module::operator::CalledVia;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::layout::{union_sorted_tags_help, Builtin, Layout, UnionVariant};
use roc_parse::ast::{AssignedField, Expr, StrLiteral};
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
#[allow(clippy::too_many_arguments)]
pub unsafe fn jit_to_ast<'a>(
    arena: &'a Bump,
    lib: Library,
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

    jit_to_ast_help(&env, lib, main_fn_name, layout, content)
}

fn jit_to_ast_help<'a>(
    env: &Env<'a, '_>,
    lib: Library,
    main_fn_name: &str,
    layout: &Layout<'a>,
    content: &Content,
) -> Expr<'a> {
    match layout {
        Layout::Builtin(Builtin::Int1) => {
            run_jit_function!(lib, main_fn_name, bool, |num| bool_to_ast(
                env, num, content
            ))
        }
        Layout::Builtin(Builtin::Int8) => {
            // NOTE: this is does not handle 8-bit numbers yet
            run_jit_function!(lib, main_fn_name, u8, |num| byte_to_ast(env, num, content))
        }
        Layout::Builtin(Builtin::Int64) => {
            run_jit_function!(lib, main_fn_name, i64, |num| num_to_ast(
                env,
                i64_to_ast(env.arena, num),
                content
            ))
        }
        Layout::Builtin(Builtin::Float64) => {
            run_jit_function!(lib, main_fn_name, f64, |num| num_to_ast(
                env,
                f64_to_ast(env.arena, num),
                content
            ))
        }
        Layout::Builtin(Builtin::Str) | Layout::Builtin(Builtin::EmptyStr) => {
            run_jit_function!(lib, main_fn_name, &'static str, |string: &'static str| {
                str_to_ast(env.arena, env.arena.alloc(string))
            })
        }
        Layout::Builtin(Builtin::EmptyList) => {
            run_jit_function!(lib, main_fn_name, &'static str, |_| { Expr::List(&[]) })
        }
        Layout::Builtin(Builtin::List(_, elem_layout)) => run_jit_function!(
            lib,
            main_fn_name,
            (*const u8, usize),
            |(ptr, len): (*const u8, usize)| { list_to_ast(env, ptr, len, elem_layout, content) }
        ),
        Layout::Builtin(other) => {
            todo!("add support for rendering builtin {:?} to the REPL", other)
        }
        Layout::PhantomEmptyStruct => run_jit_function!(lib, main_fn_name, &u8, |_| {
            Expr::Record {
                update: None,
                fields: &[],
                final_comments: env.arena.alloc([]),
            }
        }),
        Layout::Struct(field_layouts) => {
            let ptr_to_ast = |ptr: *const u8| match content {
                Content::Structure(FlatType::Record(fields, _)) => {
                    struct_to_ast(env, ptr, field_layouts, fields)
                }
                Content::Structure(FlatType::EmptyRecord) => {
                    struct_to_ast(env, ptr, field_layouts, &MutMap::default())
                }
                Content::Structure(FlatType::TagUnion(tags, _)) => {
                    debug_assert_eq!(tags.len(), 1);

                    let (tag_name, payload_vars) = tags.iter().next().unwrap();

                    single_tag_union_to_ast(env, ptr, field_layouts, tag_name.clone(), payload_vars)
                }
                other => {
                    unreachable!(
                        "Something had a Struct layout, but instead of a Record or TagUnion type, it had: {:?}",
                        other
                    );
                }
            };

            let fields = [Layout::Builtin(Builtin::Int64), layout.clone()];
            let layout = Layout::Struct(&fields);

            let result_stack_size = layout.stack_size(env.ptr_bytes);

            run_jit_function_dynamic_type!(
                lib,
                main_fn_name,
                result_stack_size as usize,
                |bytes: *const u8| { ptr_to_ast(bytes as *const u8) }
            )
        }
        Layout::Union(union_layouts) => match content {
            Content::Structure(FlatType::TagUnion(tags, _)) => {
                debug_assert_eq!(union_layouts.len(), tags.len());

                let tags_vec: std::vec::Vec<(TagName, std::vec::Vec<Variable>)> =
                    tags.iter().map(|(a, b)| (a.clone(), b.clone())).collect();

                let union_variant = union_sorted_tags_help(env.arena, tags_vec, None, env.subs);

                let size = layout.stack_size(env.ptr_bytes);
                match union_variant {
                    UnionVariant::Wrapped(tags_and_layouts) => {
                        run_jit_function_dynamic_type!(
                            lib,
                            main_fn_name,
                            size as usize,
                            |ptr: *const u8| {
                                // Because this is a `Wrapped`, the first 8 bytes encode the tag ID
                                let tag_id = *(ptr as *const i64);

                                // use the tag ID as an index, to get its name and layout of any arguments
                                let (tag_name, arg_layouts) = &tags_and_layouts[tag_id as usize];

                                let tag_expr = tag_name_to_expr(env, tag_name);
                                let loc_tag_expr = &*env.arena.alloc(Located::at_zero(tag_expr));

                                let variables = &tags[tag_name];

                                // because the arg_layouts include the tag ID, it is one longer
                                debug_assert_eq!(arg_layouts.len() - 1, variables.len());

                                // skip forward to the start of the first element, ignoring the tag id
                                let ptr = ptr.offset(8);

                                let it = variables.iter().copied().zip(&arg_layouts[1..]);
                                let output = sequence_of_expr(env, ptr, it);
                                let output = output.into_bump_slice();

                                Expr::Apply(loc_tag_expr, output, CalledVia::Space)
                            }
                        )
                    }
                    _ => unreachable!("any other variant would have a different layout"),
                }
            }
            Content::Structure(FlatType::RecursiveTagUnion(_, _, _)) => {
                todo!("print recursive tag unions in the REPL")
            }
            other => unreachable!("Weird content for Union layout: {:?}", other),
        },
        Layout::RecursiveUnion(_) | Layout::RecursivePointer => {
            todo!("add support for rendering recursive tag unions in the REPL")
        }

        Layout::FunctionPointer(_, _) | Layout::Closure(_, _, _) => {
            todo!("add support for rendering functions in the REPL")
        }
        Layout::Pointer(_) => todo!("add support for rendering pointers in the REPL"),
    }
}

fn tag_name_to_expr<'a>(env: &Env<'a, '_>, tag_name: &TagName) -> Expr<'a> {
    match tag_name {
        TagName::Global(_) => Expr::GlobalTag(
            env.arena
                .alloc_str(&tag_name.as_string(env.interns, env.home)),
        ),
        TagName::Private(_) => Expr::PrivateTag(
            env.arena
                .alloc_str(&tag_name.as_string(env.interns, env.home)),
        ),
        TagName::Closure(_) => unreachable!("User cannot type this"),
    }
}

fn ptr_to_ast<'a>(
    env: &Env<'a, '_>,
    ptr: *const u8,
    layout: &Layout<'a>,
    content: &Content,
) -> Expr<'a> {
    match layout {
        Layout::Builtin(Builtin::Int64) => {
            let num = unsafe { *(ptr as *const i64) };

            num_to_ast(env, i64_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::Int1) => {
            // TODO: bits are not as expected here.
            // num is always false at the moment.
            let num = unsafe { *(ptr as *const bool) };

            bool_to_ast(env, num, content)
        }
        Layout::Builtin(Builtin::Float64) => {
            let num = unsafe { *(ptr as *const f64) };

            num_to_ast(env, f64_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::EmptyList) => Expr::List(&[]),
        Layout::Builtin(Builtin::List(_, elem_layout)) => {
            // Turn the (ptr, len) wrapper struct into actual ptr and len values.
            let len = unsafe { *(ptr.offset(env.ptr_bytes as isize) as *const usize) };
            let ptr = unsafe { *(ptr as *const *const u8) };

            list_to_ast(env, ptr, len, elem_layout, content)
        }
        Layout::Builtin(Builtin::EmptyStr) => Expr::Str(StrLiteral::PlainLine("")),
        Layout::Builtin(Builtin::Str) => {
            let arena_str = unsafe { *(ptr as *const &'static str) };

            str_to_ast(env.arena, arena_str)
        }
        Layout::Struct(field_layouts) => match content {
            Content::Structure(FlatType::Record(fields, _)) => {
                struct_to_ast(env, ptr, field_layouts, fields)
            }
            Content::Structure(FlatType::TagUnion(tags, _)) => {
                debug_assert_eq!(tags.len(), 1);

                let (tag_name, payload_vars) = tags.iter().next().unwrap();
                single_tag_union_to_ast(env, ptr, field_layouts, tag_name.clone(), payload_vars)
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
    ptr: *const u8,
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
    let elem_size = elem_layout.stack_size(env.ptr_bytes) as usize;

    for index in 0..len {
        let offset_bytes = index * elem_size;
        let elem_ptr = unsafe { ptr.add(offset_bytes) };
        let loc_expr = &*arena.alloc(Located {
            value: ptr_to_ast(env, elem_ptr, elem_layout, &elem_content),
            region: Region::zero(),
        });

        output.push(loc_expr);
    }

    let output = output.into_bump_slice();

    Expr::List(output)
}

fn single_tag_union_to_ast<'a>(
    env: &Env<'a, '_>,
    ptr: *const u8,
    field_layouts: &'a [Layout<'a>],
    tag_name: TagName,
    payload_vars: &[Variable],
) -> Expr<'a> {
    debug_assert_eq!(field_layouts.len(), payload_vars.len());

    let arena = env.arena;

    let tag_expr = tag_name_to_expr(env, &tag_name);

    let loc_tag_expr = &*arena.alloc(Located::at_zero(tag_expr));

    let it = payload_vars.iter().copied().zip(field_layouts);
    let output = sequence_of_expr(env, ptr as *const u8, it).into_bump_slice();

    Expr::Apply(loc_tag_expr, output, CalledVia::Space)
}

fn sequence_of_expr<'a, I>(
    env: &Env<'a, '_>,
    ptr: *const u8,
    sequence: I,
) -> Vec<'a, &'a Located<Expr<'a>>>
where
    I: Iterator<Item = (Variable, &'a Layout<'a>)>,
    I: ExactSizeIterator<Item = (Variable, &'a Layout<'a>)>,
{
    let arena = env.arena;
    let subs = env.subs;
    let mut output = Vec::with_capacity_in(sequence.len(), &arena);

    // We'll advance this as we iterate through the fields
    let mut field_ptr = ptr as *const u8;

    for (var, layout) in sequence {
        let content = subs.get_without_compacting(var).content;
        let expr = ptr_to_ast(env, field_ptr, layout, &content);
        let loc_expr = Located::at_zero(expr);

        output.push(&*arena.alloc(loc_expr));

        // Advance the field pointer to the next field.
        field_ptr = unsafe { field_ptr.offset(layout.stack_size(env.ptr_bytes) as isize) };
    }

    output
}

fn struct_to_ast<'a>(
    env: &Env<'a, '_>,
    ptr: *const u8,
    field_layouts: &'a [Layout<'a>],
    fields: &MutMap<Lowercase, RecordField<Variable>>,
) -> Expr<'a> {
    let arena = env.arena;
    let subs = env.subs;
    let mut output = Vec::with_capacity_in(field_layouts.len(), &arena);

    // The fields, sorted alphabetically
    let mut sorted_fields = {
        let mut vec = fields
            .iter()
            .collect::<std::vec::Vec<(&Lowercase, &RecordField<Variable>)>>();

        vec.sort_by(|(label1, _), (label2, _)| label1.cmp(label2));

        vec
    };

    if sorted_fields.len() == 1 {
        // this is a 1-field wrapper record around another record or 1-tag tag union
        let (label, field) = sorted_fields.pop().unwrap();

        let inner_content = env.subs.get_without_compacting(field.into_inner()).content;

        let loc_expr = &*arena.alloc(Located {
            value: ptr_to_ast(env, ptr, &Layout::Struct(field_layouts), &inner_content),
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

        let output = env.arena.alloc([loc_field]);

        Expr::Record {
            update: None,
            fields: output,
            final_comments: &[],
        }
    } else {
        debug_assert_eq!(sorted_fields.len(), field_layouts.len());

        // We'll advance this as we iterate through the fields
        let mut field_ptr = ptr;

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
            field_ptr =
                unsafe { field_ptr.offset(field_layout.stack_size(env.ptr_bytes) as isize) };
        }

        let output = output.into_bump_slice();

        Expr::Record {
            update: None,
            fields: output,
            final_comments: &[],
        }
    }
}

fn bool_to_ast<'a>(env: &Env<'a, '_>, value: bool, content: &Content) -> Expr<'a> {
    use Content::*;

    let arena = env.arena;

    match content {
        Structure(flat_type) => {
            match flat_type {
                FlatType::Record(fields, _) => {
                    debug_assert_eq!(fields.len(), 1);

                    let (label, field) = fields.iter().next().unwrap();
                    let loc_label = Located {
                        value: &*arena.alloc_str(label.as_str()),
                        region: Region::zero(),
                    };

                    let assigned_field = {
                        // We may be multiple levels deep in nested tag unions
                        // and/or records (e.g. { a: { b: { c: True }  } }),
                        // so we need to do this recursively on the field type.
                        let field_var = *field.as_inner();
                        let field_content = env.subs.get_without_compacting(field_var).content;
                        let loc_expr = Located {
                            value: bool_to_ast(env, value, &field_content),
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
                        fields: arena.alloc([loc_assigned_field]),
                        final_comments: arena.alloc([]),
                    }
                }
                FlatType::TagUnion(tags, _) if tags.len() == 1 => {
                    let (tag_name, payload_vars) = tags.iter().next().unwrap();

                    let loc_tag_expr = {
                        let tag_name = &tag_name.as_string(env.interns, env.home);
                        let tag_expr = if tag_name.starts_with('@') {
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
                            value: bool_to_ast(env, value, &content),
                            region: Region::zero(),
                        });

                        arena.alloc([loc_payload])
                    };

                    Expr::Apply(loc_tag_expr, payload, CalledVia::Space)
                }
                FlatType::TagUnion(tags, _) if tags.len() == 2 => {
                    let mut tags_iter = tags.iter();
                    let (tag_name_1, payload_vars_1) = tags_iter.next().unwrap();
                    let (tag_name_2, payload_vars_2) = tags_iter.next().unwrap();

                    debug_assert!(payload_vars_1.is_empty());
                    debug_assert!(payload_vars_2.is_empty());

                    let tag_name = if value {
                        max_by_key(tag_name_1, tag_name_2, |n| {
                            n.as_string(env.interns, env.home)
                        })
                    } else {
                        min_by_key(tag_name_1, tag_name_2, |n| {
                            n.as_string(env.interns, env.home)
                        })
                    };

                    tag_name_to_expr(env, tag_name)
                }
                other => {
                    unreachable!("Unexpected FlatType {:?} in bool_to_ast", other);
                }
            }
        }
        Alias(_, _, var) => {
            let content = env.subs.get_without_compacting(*var).content;

            bool_to_ast(env, value, &content)
        }
        other => {
            unreachable!("Unexpected FlatType {:?} in bool_to_ast", other);
        }
    }
}

fn byte_to_ast<'a>(env: &Env<'a, '_>, value: u8, content: &Content) -> Expr<'a> {
    use Content::*;

    let arena = env.arena;

    match content {
        Structure(flat_type) => {
            match flat_type {
                FlatType::Record(fields, _) => {
                    debug_assert_eq!(fields.len(), 1);

                    let (label, field) = fields.iter().next().unwrap();
                    let loc_label = Located {
                        value: &*arena.alloc_str(label.as_str()),
                        region: Region::zero(),
                    };

                    let assigned_field = {
                        // We may be multiple levels deep in nested tag unions
                        // and/or records (e.g. { a: { b: { c: True }  } }),
                        // so we need to do this recursively on the field type.
                        let field_var = *field.as_inner();
                        let field_content = env.subs.get_without_compacting(field_var).content;
                        let loc_expr = Located {
                            value: byte_to_ast(env, value, &field_content),
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
                        fields: arena.alloc([loc_assigned_field]),
                        final_comments: &[],
                    }
                }
                FlatType::TagUnion(tags, _) if tags.len() == 1 => {
                    let (tag_name, payload_vars) = tags.iter().next().unwrap();

                    let loc_tag_expr = {
                        let tag_name = &tag_name.as_string(env.interns, env.home);
                        let tag_expr = if tag_name.starts_with('@') {
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
                            value: byte_to_ast(env, value, &content),
                            region: Region::zero(),
                        });

                        arena.alloc([loc_payload])
                    };

                    Expr::Apply(loc_tag_expr, payload, CalledVia::Space)
                }
                FlatType::TagUnion(tags, _) => {
                    // anything with fewer tags is not a byte
                    debug_assert!(tags.len() > 2);

                    let tags_vec: std::vec::Vec<(TagName, std::vec::Vec<Variable>)> =
                        tags.iter().map(|(a, b)| (a.clone(), b.clone())).collect();

                    let union_variant = union_sorted_tags_help(env.arena, tags_vec, None, env.subs);

                    match union_variant {
                        UnionVariant::ByteUnion(tagnames) => {
                            let tag_name = &tagnames[value as usize];
                            let tag_expr = tag_name_to_expr(env, tag_name);
                            let loc_tag_expr = Located::at_zero(tag_expr);
                            Expr::Apply(env.arena.alloc(loc_tag_expr), &[], CalledVia::Space)
                        }
                        _ => unreachable!("invalid union variant for a Byte!"),
                    }
                }
                other => {
                    unreachable!("Unexpected FlatType {:?} in bool_to_ast", other);
                }
            }
        }
        Alias(_, _, var) => {
            let content = env.subs.get_without_compacting(*var).content;

            byte_to_ast(env, value, &content)
        }
        other => {
            unreachable!("Unexpected FlatType {:?} in bool_to_ast", other);
        }
    }
}

fn num_to_ast<'a>(env: &Env<'a, '_>, num_expr: Expr<'a>, content: &Content) -> Expr<'a> {
    use Content::*;

    let arena = env.arena;

    match content {
        Structure(flat_type) => {
            match flat_type {
                FlatType::Apply(Symbol::NUM_NUM, _) => num_expr,
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
                            value: num_to_ast(env, num_expr, &field_content),
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
                        fields: arena.alloc([loc_assigned_field]),
                        final_comments: arena.alloc([]),
                    }
                }
                FlatType::TagUnion(tags, _) => {
                    // This was a single-tag union that got unwrapped at runtime.
                    debug_assert_eq!(tags.len(), 1);

                    let (tag_name, payload_vars) = tags.iter().next().unwrap();

                    // If this tag union represents a number, skip right to
                    // returning tis as an Expr::Num
                    if let TagName::Private(Symbol::NUM_AT_NUM) = &tag_name {
                        return num_expr;
                    }

                    let loc_tag_expr = {
                        let tag_name = &tag_name.as_string(env.interns, env.home);
                        let tag_expr = if tag_name.starts_with('@') {
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
                            value: num_to_ast(env, num_expr, &content),
                            region: Region::zero(),
                        });

                        arena.alloc([loc_payload])
                    };

                    Expr::Apply(loc_tag_expr, payload, CalledVia::Space)
                }
                other => {
                    panic!("Unexpected FlatType {:?} in num_to_ast", other);
                }
            }
        }
        Alias(_, _, var) => {
            let content = env.subs.get_without_compacting(*var).content;

            num_to_ast(env, num_expr, &content)
        }
        other => {
            panic!("Unexpected FlatType {:?} in num_to_ast", other);
        }
    }
}

/// This is centralized in case we want to format it differently later,
/// e.g. adding underscores for large numbers
fn i64_to_ast(arena: &Bump, num: i64) -> Expr<'_> {
    Expr::Num(arena.alloc(format!("{}", num)))
}

/// This is centralized in case we want to format it differently later,
/// e.g. adding underscores for large numbers
fn f64_to_ast(arena: &Bump, num: f64) -> Expr<'_> {
    Expr::Num(arena.alloc(format!("{}", num)))
}

#[cfg(target_endian = "little")]
#[cfg(target_pointer_width = "64")]
/// TODO implement this for 32-bit and big-endian targets. NOTE: As of this writing,
/// we don't have big-endian small strings implemented yet!
fn str_to_ast<'a>(arena: &'a Bump, string: &'a str) -> Expr<'a> {
    let bytes: [u8; 16] = unsafe { std::mem::transmute::<&'a str, [u8; 16]>(string) };
    let is_small = (bytes[15] & 0b1000_0000) != 0;

    if is_small {
        let len = (bytes[15] & 0b0111_1111) as usize;
        let mut string = bumpalo::collections::String::with_capacity_in(len, arena);

        for byte in bytes.iter().take(len) {
            string.push(*byte as char);
        }

        str_slice_to_ast(arena, arena.alloc(string))
    } else {
        str_slice_to_ast(arena, string)
    }
}

fn str_slice_to_ast<'a>(_arena: &'a Bump, string: &'a str) -> Expr<'a> {
    if string.contains('\n') {
        todo!(
            "this string contains newlines, so render it as a multiline string: {:?}",
            Expr::Str(StrLiteral::PlainLine(string))
        );
    } else {
        Expr::Str(StrLiteral::PlainLine(string))
    }
}

// TODO this is currently nighly-only: use the implementation in std once it's stabilized
pub fn max_by<T, F: FnOnce(&T, &T) -> std::cmp::Ordering>(v1: T, v2: T, compare: F) -> T {
    use std::cmp::Ordering;

    match compare(&v1, &v2) {
        Ordering::Less | Ordering::Equal => v2,
        Ordering::Greater => v1,
    }
}

pub fn min_by<T, F: FnOnce(&T, &T) -> std::cmp::Ordering>(v1: T, v2: T, compare: F) -> T {
    use std::cmp::Ordering;

    match compare(&v1, &v2) {
        Ordering::Less | Ordering::Equal => v1,
        Ordering::Greater => v2,
    }
}

pub fn max_by_key<T, F: FnMut(&T) -> K, K: Ord>(v1: T, v2: T, mut f: F) -> T {
    max_by(v1, v2, |v1, v2| f(v1).cmp(&f(v2)))
}

pub fn min_by_key<T, F: FnMut(&T) -> K, K: Ord>(v1: T, v2: T, mut f: F) -> T {
    min_by(v1, v2, |v1, v2| f(v1).cmp(&f(v2)))
}
