use bumpalo::collections::Vec;
use bumpalo::Bump;
use libloading::Library;
use roc_gen_llvm::{run_jit_function, run_jit_function_dynamic_type};
use roc_module::ident::TagName;
use roc_module::operator::CalledVia;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::ProcLayout;
use roc_mono::layout::{union_sorted_tags_help, Builtin, Layout, UnionLayout, UnionVariant};
use roc_parse::ast::{AssignedField, Collection, Expr, StrLiteral};
use roc_region::all::{Located, Region};
use roc_types::subs::{Content, FlatType, GetSubsSlice, RecordFields, Subs, UnionTags, Variable};

struct Env<'a, 'env> {
    arena: &'a Bump,
    subs: &'env Subs,
    ptr_bytes: u32,
    interns: &'env Interns,
    home: ModuleId,
}

pub enum ToAstProblem {
    FunctionLayout,
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
    layout: ProcLayout<'a>,
    content: &Content,
    interns: &Interns,
    home: ModuleId,
    subs: &Subs,
    ptr_bytes: u32,
) -> Result<Expr<'a>, ToAstProblem> {
    let env = Env {
        arena,
        subs,
        ptr_bytes,
        interns,
        home,
    };

    match layout {
        ProcLayout {
            arguments: [],
            result,
        } => {
            // this is a thunk
            jit_to_ast_help(&env, lib, main_fn_name, &result, content)
        }
        _ => Err(ToAstProblem::FunctionLayout),
    }
}

fn jit_to_ast_help<'a>(
    env: &Env<'a, '_>,
    lib: Library,
    main_fn_name: &str,
    layout: &Layout<'a>,
    content: &Content,
) -> Result<Expr<'a>, ToAstProblem> {
    match layout {
        Layout::Builtin(Builtin::Int1) => Ok(run_jit_function!(lib, main_fn_name, bool, |num| {
            bool_to_ast(env, num, content)
        })),
        Layout::Builtin(Builtin::Int8) => {
            Ok(
                // NOTE: this is does not handle 8-bit numbers yet
                run_jit_function!(lib, main_fn_name, u8, |num| byte_to_ast(env, num, content)),
            )
        }
        Layout::Builtin(Builtin::Usize) => Ok(run_jit_function!(lib, main_fn_name, usize, |num| {
            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        })),
        Layout::Builtin(Builtin::Int16) => {
            Ok(run_jit_function!(lib, main_fn_name, i16, |num| num_to_ast(
                env,
                number_literal_to_ast(env.arena, num),
                content
            )))
        }
        Layout::Builtin(Builtin::Int32) => {
            Ok(run_jit_function!(lib, main_fn_name, i32, |num| num_to_ast(
                env,
                number_literal_to_ast(env.arena, num),
                content
            )))
        }
        Layout::Builtin(Builtin::Int64) => {
            Ok(run_jit_function!(lib, main_fn_name, i64, |num| num_to_ast(
                env,
                number_literal_to_ast(env.arena, num),
                content
            )))
        }
        Layout::Builtin(Builtin::Int128) => {
            Ok(run_jit_function!(
                lib,
                main_fn_name,
                i128,
                |num| num_to_ast(env, number_literal_to_ast(env.arena, num), content)
            ))
        }
        Layout::Builtin(Builtin::Float32) => {
            Ok(run_jit_function!(lib, main_fn_name, f32, |num| num_to_ast(
                env,
                number_literal_to_ast(env.arena, num),
                content
            )))
        }
        Layout::Builtin(Builtin::Float64) => {
            Ok(run_jit_function!(lib, main_fn_name, f64, |num| num_to_ast(
                env,
                number_literal_to_ast(env.arena, num),
                content
            )))
        }
        Layout::Builtin(Builtin::Str) | Layout::Builtin(Builtin::EmptyStr) => Ok(
            run_jit_function!(lib, main_fn_name, &'static str, |string: &'static str| {
                str_to_ast(env.arena, env.arena.alloc(string))
            }),
        ),
        Layout::Builtin(Builtin::EmptyList) => {
            Ok(run_jit_function!(lib, main_fn_name, &'static str, |_| {
                Expr::List(Collection::empty())
            }))
        }
        Layout::Builtin(Builtin::List(elem_layout)) => Ok(run_jit_function!(
            lib,
            main_fn_name,
            (*const u8, usize),
            |(ptr, len): (*const u8, usize)| { list_to_ast(env, ptr, len, elem_layout, content) }
        )),
        Layout::Builtin(other) => {
            todo!("add support for rendering builtin {:?} to the REPL", other)
        }
        Layout::Struct(field_layouts) => {
            let ptr_to_ast = |ptr: *const u8| match content {
                Content::Structure(FlatType::Record(fields, _)) => {
                    Ok(struct_to_ast(env, ptr, field_layouts, *fields))
                }
                Content::Structure(FlatType::EmptyRecord) => Ok(struct_to_ast(
                    env,
                    ptr,
                    field_layouts,
                    RecordFields::empty(),
                )),
                Content::Structure(FlatType::TagUnion(tags, _)) => {
                    debug_assert_eq!(tags.len(), 1);

                    let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);

                    Ok(single_tag_union_to_ast(
                        env,
                        ptr,
                        field_layouts,
                        tag_name,
                        payload_vars,
                    ))
                }
                Content::Structure(FlatType::FunctionOrTagUnion(tag_name, _, _)) => {
                    let tag_name = &env.subs[*tag_name];

                    Ok(single_tag_union_to_ast(
                        env,
                        ptr,
                        field_layouts,
                        tag_name,
                        &[],
                    ))
                }
                Content::Structure(FlatType::Func(_, _, _)) => {
                    // a function with a struct as the closure environment
                    Err(ToAstProblem::FunctionLayout)
                }
                other => {
                    unreachable!(
                        "Something had a Struct layout, but instead of a Record or TagUnion type, it had: {:?}",
                        other
                    );
                }
            };

            let fields = [Layout::Builtin(Builtin::Int64), *layout];
            let layout = Layout::Struct(&fields);

            let result_stack_size = layout.stack_size(env.ptr_bytes);

            run_jit_function_dynamic_type!(
                lib,
                main_fn_name,
                result_stack_size as usize,
                |bytes: *const u8| { ptr_to_ast(bytes as *const u8) }
            )
        }
        Layout::Union(UnionLayout::NonRecursive(union_layouts)) => {
            let union_layout = UnionLayout::NonRecursive(union_layouts);

            match content {
                Content::Structure(FlatType::TagUnion(tags, _)) => {
                    debug_assert_eq!(union_layouts.len(), tags.len());

                    let tags_vec: std::vec::Vec<(TagName, std::vec::Vec<Variable>)> = tags
                        .unsorted_iterator(env.subs, Variable::EMPTY_TAG_UNION)
                        .map(|(a, b)| (a.clone(), b.to_vec()))
                        .collect();

                    let tags_map: roc_collections::all::MutMap<_, _> =
                        tags_vec.iter().cloned().collect();

                    let union_variant =
                        union_sorted_tags_help(env.arena, tags_vec, None, env.subs, env.ptr_bytes);

                    let size = layout.stack_size(env.ptr_bytes);
                    use roc_mono::layout::WrappedVariant::*;
                    match union_variant {
                        UnionVariant::Wrapped(variant) => {
                            match variant {
                                NonRecursive {
                                    sorted_tag_layouts: tags_and_layouts,
                                } => {
                                    Ok(run_jit_function_dynamic_type!(
                                        lib,
                                        main_fn_name,
                                        size as usize,
                                        |ptr: *const u8| {
                                            // Because this is a `Wrapped`, the first 8 bytes encode the tag ID
                                            let offset = tags_and_layouts
                                                .iter()
                                                .map(|(_, fields)| {
                                                    fields
                                                        .iter()
                                                        .map(|l| l.stack_size(env.ptr_bytes))
                                                        .sum()
                                                })
                                                .max()
                                                .unwrap_or(0);

                                            let tag_id = match union_layout.tag_id_builtin() {
                                                Builtin::Int1 => {
                                                    *(ptr.add(offset as usize) as *const i8) as i64
                                                }
                                                Builtin::Int8 => {
                                                    *(ptr.add(offset as usize) as *const i8) as i64
                                                }
                                                Builtin::Int16 => {
                                                    *(ptr.add(offset as usize) as *const i16) as i64
                                                }
                                                Builtin::Int64 => {
                                                    // used by non-recursive unions at the
                                                    // moment, remove if that is no longer the case
                                                    *(ptr.add(offset as usize) as *const i64) as i64
                                                }
                                                _ => unreachable!("invalid tag id layout"),
                                            };

                                            // use the tag ID as an index, to get its name and layout of any arguments
                                            let (tag_name, arg_layouts) =
                                                &tags_and_layouts[tag_id as usize];

                                            let tag_expr = tag_name_to_expr(env, tag_name);
                                            let loc_tag_expr =
                                                &*env.arena.alloc(Located::at_zero(tag_expr));

                                            let variables = &tags_map[tag_name];

                                            debug_assert_eq!(arg_layouts.len(), variables.len());

                                            // NOTE assumes the data bytes are the first bytes
                                            let it =
                                                variables.iter().copied().zip(arg_layouts.iter());
                                            let output = sequence_of_expr(env, ptr, it);
                                            let output = output.into_bump_slice();

                                            Expr::Apply(loc_tag_expr, output, CalledVia::Space)
                                        }
                                    ))
                                }
                                Recursive {
                                    sorted_tag_layouts: tags_and_layouts,
                                } => {
                                    Ok(run_jit_function_dynamic_type!(
                                        lib,
                                        main_fn_name,
                                        size as usize,
                                        |ptr: *const u8| {
                                            // Because this is a `Wrapped`, the first 8 bytes encode the tag ID
                                            let tag_id = *(ptr as *const i64);

                                            // use the tag ID as an index, to get its name and layout of any arguments
                                            let (tag_name, arg_layouts) =
                                                &tags_and_layouts[tag_id as usize];

                                            let tag_expr = tag_name_to_expr(env, tag_name);
                                            let loc_tag_expr =
                                                &*env.arena.alloc(Located::at_zero(tag_expr));

                                            let variables = &tags_map[tag_name];

                                            // because the arg_layouts include the tag ID, it is one longer
                                            debug_assert_eq!(
                                                arg_layouts.len() - 1,
                                                variables.len()
                                            );

                                            // skip forward to the start of the first element, ignoring the tag id
                                            let ptr = ptr.offset(8);

                                            let it =
                                                variables.iter().copied().zip(&arg_layouts[1..]);
                                            let output = sequence_of_expr(env, ptr, it);
                                            let output = output.into_bump_slice();

                                            Expr::Apply(loc_tag_expr, output, CalledVia::Space)
                                        }
                                    ))
                                }
                                _ => todo!(),
                            }
                        }
                        _ => unreachable!("any other variant would have a different layout"),
                    }
                }
                Content::Structure(FlatType::RecursiveTagUnion(_, _, _)) => {
                    todo!("print recursive tag unions in the REPL")
                }
                Content::Alias(_, _, actual) => {
                    let content = env.subs.get_content_without_compacting(*actual);

                    jit_to_ast_help(env, lib, main_fn_name, layout, content)
                }
                other => unreachable!("Weird content for Union layout: {:?}", other),
            }
        }
        Layout::Union(UnionLayout::Recursive(_))
        | Layout::Union(UnionLayout::NullableWrapped { .. })
        | Layout::Union(UnionLayout::NullableUnwrapped { .. })
        | Layout::Union(UnionLayout::NonNullableUnwrapped(_))
        | Layout::RecursivePointer => {
            todo!("add support for rendering recursive tag unions in the REPL")
        }
        Layout::LambdaSet(lambda_set) => jit_to_ast_help(
            env,
            lib,
            main_fn_name,
            &lambda_set.runtime_representation(),
            content,
        ),
    }
}

fn tag_name_to_expr<'a>(env: &Env<'a, '_>, tag_name: &TagName) -> Expr<'a> {
    match tag_name {
        TagName::Global(_) => Expr::GlobalTag(
            env.arena
                .alloc_str(&tag_name.as_ident_str(env.interns, env.home)),
        ),
        TagName::Private(_) => Expr::PrivateTag(
            env.arena
                .alloc_str(&tag_name.as_ident_str(env.interns, env.home)),
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
        Layout::Builtin(Builtin::Int128) => {
            let num = unsafe { *(ptr as *const i128) };

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::Int64) => {
            let num = unsafe { *(ptr as *const i64) };

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::Int32) => {
            let num = unsafe { *(ptr as *const i32) };

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::Int16) => {
            let num = unsafe { *(ptr as *const i16) };

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::Int8) => {
            let num = unsafe { *(ptr as *const i8) };

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::Int1) => {
            // TODO: bits are not as expected here.
            // num is always false at the moment.
            let num = unsafe { *(ptr as *const bool) };

            bool_to_ast(env, num, content)
        }
        Layout::Builtin(Builtin::Usize) => {
            let num = unsafe { *(ptr as *const usize) };

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::Float64) => {
            let num = unsafe { *(ptr as *const f64) };

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::Float32) => {
            let num = unsafe { *(ptr as *const f32) };

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }
        Layout::Builtin(Builtin::EmptyList) => Expr::List(Collection::empty()),
        Layout::Builtin(Builtin::List(elem_layout)) => {
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
                struct_to_ast(env, ptr, field_layouts, *fields)
            }
            Content::Structure(FlatType::TagUnion(tags, _)) => {
                debug_assert_eq!(tags.len(), 1);

                let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);
                single_tag_union_to_ast(env, ptr, field_layouts, tag_name, payload_vars)
            }
            Content::Structure(FlatType::FunctionOrTagUnion(tag_name, _, _)) => {
                let tag_name = &env.subs[*tag_name];
                single_tag_union_to_ast(env, ptr, field_layouts, tag_name, &[])
            }
            Content::Structure(FlatType::EmptyRecord) => {
                struct_to_ast(env, ptr, &[], RecordFields::empty())
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

            let elem_var_index = vars.into_iter().next().unwrap();
            let elem_var = env.subs[elem_var_index];

            env.subs.get_content_without_compacting(elem_var)
        }
        other => {
            unreachable!(
                "Something had a Struct layout, but instead of a Record type, it had: {:?}",
                other
            );
        }
    };

    let arena = env.arena;
    let mut output = Vec::with_capacity_in(len, arena);
    let elem_size = elem_layout.stack_size(env.ptr_bytes) as usize;

    for index in 0..len {
        let offset_bytes = index * elem_size;
        let elem_ptr = unsafe { ptr.add(offset_bytes) };
        let loc_expr = &*arena.alloc(Located {
            value: ptr_to_ast(env, elem_ptr, elem_layout, elem_content),
            region: Region::zero(),
        });

        output.push(loc_expr);
    }

    let output = output.into_bump_slice();

    Expr::List(Collection::with_items(output))
}

fn single_tag_union_to_ast<'a>(
    env: &Env<'a, '_>,
    ptr: *const u8,
    field_layouts: &'a [Layout<'a>],
    tag_name: &TagName,
    payload_vars: &[Variable],
) -> Expr<'a> {
    let arena = env.arena;
    let tag_expr = tag_name_to_expr(env, tag_name);

    let loc_tag_expr = &*arena.alloc(Located::at_zero(tag_expr));

    let output = if field_layouts.len() == payload_vars.len() {
        let it = payload_vars.iter().copied().zip(field_layouts);
        sequence_of_expr(env, ptr as *const u8, it).into_bump_slice()
    } else if field_layouts.is_empty() && !payload_vars.is_empty() {
        // happens for e.g. `Foo Bar` where unit structures are nested and the inner one is dropped
        let it = payload_vars.iter().copied().zip([&Layout::Struct(&[])]);
        sequence_of_expr(env, ptr as *const u8, it).into_bump_slice()
    } else {
        unreachable!()
    };

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
    let mut output = Vec::with_capacity_in(sequence.len(), arena);

    // We'll advance this as we iterate through the fields
    let mut field_ptr = ptr as *const u8;

    for (var, layout) in sequence {
        let content = subs.get_content_without_compacting(var);
        let expr = ptr_to_ast(env, field_ptr, layout, content);
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
    record_fields: RecordFields,
) -> Expr<'a> {
    let arena = env.arena;
    let subs = env.subs;
    let mut output = Vec::with_capacity_in(field_layouts.len(), arena);

    let sorted_fields: Vec<_> = Vec::from_iter_in(
        record_fields.sorted_iterator(env.subs, Variable::EMPTY_RECORD),
        env.arena,
    );

    if sorted_fields.len() == 1 {
        // this is a 1-field wrapper record around another record or 1-tag tag union
        let (label, field) = sorted_fields.into_iter().next().unwrap();

        let inner_content = env.subs.get_content_without_compacting(field.into_inner());

        let loc_expr = &*arena.alloc(Located {
            value: ptr_to_ast(env, ptr, &Layout::Struct(field_layouts), inner_content),
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

        Expr::Record(Collection::with_items(output))
    } else {
        debug_assert_eq!(sorted_fields.len(), field_layouts.len());

        // We'll advance this as we iterate through the fields
        let mut field_ptr = ptr;

        for ((label, field), field_layout) in sorted_fields.into_iter().zip(field_layouts.iter()) {
            let var = field.into_inner();

            let content = subs.get_content_without_compacting(var);
            let loc_expr = &*arena.alloc(Located {
                value: ptr_to_ast(env, field_ptr, field_layout, content),
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

        Expr::Record(Collection::with_items(output))
    }
}

fn unpack_single_element_tag_union(subs: &Subs, tags: UnionTags) -> (&TagName, &[Variable]) {
    let (tag_name_index, payload_vars_index) = tags.iter_all().next().unwrap();

    let tag_name = &subs[tag_name_index];
    let subs_slice = subs[payload_vars_index].as_subs_slice();
    let payload_vars = subs.get_subs_slice(*subs_slice);

    (tag_name, payload_vars)
}

fn unpack_two_element_tag_union(
    subs: &Subs,
    tags: UnionTags,
) -> (&TagName, &[Variable], &TagName, &[Variable]) {
    let mut it = tags.iter_all();
    let (tag_name_index, payload_vars_index) = it.next().unwrap();

    let tag_name1 = &subs[tag_name_index];
    let subs_slice = subs[payload_vars_index].as_subs_slice();
    let payload_vars1 = subs.get_subs_slice(*subs_slice);

    let (tag_name_index, payload_vars_index) = it.next().unwrap();

    let tag_name2 = &subs[tag_name_index];
    let subs_slice = subs[payload_vars_index].as_subs_slice();
    let payload_vars2 = subs.get_subs_slice(*subs_slice);

    (tag_name1, payload_vars1, tag_name2, payload_vars2)
}

fn bool_to_ast<'a>(env: &Env<'a, '_>, value: bool, content: &Content) -> Expr<'a> {
    use Content::*;

    let arena = env.arena;

    match content {
        Structure(flat_type) => {
            match flat_type {
                FlatType::Record(fields, _) => {
                    debug_assert_eq!(fields.len(), 1);

                    let (label, field) = fields
                        .sorted_iterator(env.subs, Variable::EMPTY_RECORD)
                        .next()
                        .unwrap();

                    let loc_label = Located {
                        value: &*arena.alloc_str(label.as_str()),
                        region: Region::zero(),
                    };

                    let assigned_field = {
                        // We may be multiple levels deep in nested tag unions
                        // and/or records (e.g. { a: { b: { c: True }  } }),
                        // so we need to do this recursively on the field type.
                        let field_var = *field.as_inner();
                        let field_content = env.subs.get_content_without_compacting(field_var);
                        let loc_expr = Located {
                            value: bool_to_ast(env, value, field_content),
                            region: Region::zero(),
                        };

                        AssignedField::RequiredValue(loc_label, &[], arena.alloc(loc_expr))
                    };

                    let loc_assigned_field = Located {
                        value: assigned_field,
                        region: Region::zero(),
                    };

                    Expr::Record(Collection::with_items(arena.alloc([loc_assigned_field])))
                }
                FlatType::TagUnion(tags, _) if tags.len() == 1 => {
                    let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);

                    let loc_tag_expr = {
                        let tag_name = &tag_name.as_ident_str(env.interns, env.home);
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
                        let content = env.subs.get_content_without_compacting(var);

                        let loc_payload = &*arena.alloc(Located {
                            value: bool_to_ast(env, value, content),
                            region: Region::zero(),
                        });

                        arena.alloc([loc_payload])
                    };

                    Expr::Apply(loc_tag_expr, payload, CalledVia::Space)
                }
                FlatType::TagUnion(tags, _) if tags.len() == 2 => {
                    let (tag_name_1, payload_vars_1, tag_name_2, payload_vars_2) =
                        unpack_two_element_tag_union(env.subs, *tags);

                    debug_assert!(payload_vars_1.is_empty());
                    debug_assert!(payload_vars_2.is_empty());

                    let tag_name = if value {
                        max_by_key(tag_name_1, tag_name_2, |n| {
                            n.as_ident_str(env.interns, env.home)
                        })
                    } else {
                        min_by_key(tag_name_1, tag_name_2, |n| {
                            n.as_ident_str(env.interns, env.home)
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
            let content = env.subs.get_content_without_compacting(*var);

            bool_to_ast(env, value, content)
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

                    let (label, field) = fields
                        .sorted_iterator(env.subs, Variable::EMPTY_RECORD)
                        .next()
                        .unwrap();

                    let loc_label = Located {
                        value: &*arena.alloc_str(label.as_str()),
                        region: Region::zero(),
                    };

                    let assigned_field = {
                        // We may be multiple levels deep in nested tag unions
                        // and/or records (e.g. { a: { b: { c: True }  } }),
                        // so we need to do this recursively on the field type.
                        let field_var = *field.as_inner();
                        let field_content = env.subs.get_content_without_compacting(field_var);
                        let loc_expr = Located {
                            value: byte_to_ast(env, value, field_content),
                            region: Region::zero(),
                        };

                        AssignedField::RequiredValue(loc_label, &[], arena.alloc(loc_expr))
                    };

                    let loc_assigned_field = Located {
                        value: assigned_field,
                        region: Region::zero(),
                    };

                    Expr::Record(Collection::with_items(arena.alloc([loc_assigned_field])))
                }
                FlatType::TagUnion(tags, _) if tags.len() == 1 => {
                    let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);

                    let loc_tag_expr = {
                        let tag_name = &tag_name.as_ident_str(env.interns, env.home);
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
                        let content = env.subs.get_content_without_compacting(var);

                        let loc_payload = &*arena.alloc(Located {
                            value: byte_to_ast(env, value, content),
                            region: Region::zero(),
                        });

                        arena.alloc([loc_payload])
                    };

                    Expr::Apply(loc_tag_expr, payload, CalledVia::Space)
                }
                FlatType::TagUnion(tags, _) => {
                    // anything with fewer tags is not a byte
                    debug_assert!(tags.len() > 2);

                    let tags_vec: std::vec::Vec<(TagName, std::vec::Vec<Variable>)> = tags
                        .unsorted_iterator(env.subs, Variable::EMPTY_TAG_UNION)
                        .map(|(a, b)| (a.clone(), b.to_vec()))
                        .collect();

                    let union_variant =
                        union_sorted_tags_help(env.arena, tags_vec, None, env.subs, env.ptr_bytes);

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
            let content = env.subs.get_content_without_compacting(*var);

            byte_to_ast(env, value, content)
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

                    let (label, field) = fields
                        .sorted_iterator(env.subs, Variable::EMPTY_RECORD)
                        .next()
                        .unwrap();

                    let loc_label = Located {
                        value: &*arena.alloc_str(label.as_str()),
                        region: Region::zero(),
                    };

                    let assigned_field = {
                        // We may be multiple levels deep in nested tag unions
                        // and/or records (e.g. { a: { b: { c: 5 }  } }),
                        // so we need to do this recursively on the field type.
                        let field_var = *field.as_inner();
                        let field_content = env.subs.get_content_without_compacting(field_var);
                        let loc_expr = Located {
                            value: num_to_ast(env, num_expr, field_content),
                            region: Region::zero(),
                        };

                        AssignedField::RequiredValue(loc_label, &[], arena.alloc(loc_expr))
                    };
                    let loc_assigned_field = Located {
                        value: assigned_field,
                        region: Region::zero(),
                    };

                    Expr::Record(Collection::with_items(arena.alloc([loc_assigned_field])))
                }
                FlatType::TagUnion(tags, _) => {
                    // This was a single-tag union that got unwrapped at runtime.
                    debug_assert_eq!(tags.len(), 1);

                    let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);

                    // If this tag union represents a number, skip right to
                    // returning tis as an Expr::Num
                    if let TagName::Private(Symbol::NUM_AT_NUM) = &tag_name {
                        return num_expr;
                    }

                    let loc_tag_expr = {
                        let tag_name = &tag_name.as_ident_str(env.interns, env.home);
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
                        let content = env.subs.get_content_without_compacting(var);

                        let loc_payload = &*arena.alloc(Located {
                            value: num_to_ast(env, num_expr, content),
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
            let content = env.subs.get_content_without_compacting(*var);

            num_to_ast(env, num_expr, content)
        }
        other => {
            panic!("Unexpected FlatType {:?} in num_to_ast", other);
        }
    }
}

/// This is centralized in case we want to format it differently later,
/// e.g. adding underscores for large numbers
fn number_literal_to_ast<T: std::fmt::Display>(arena: &Bump, num: T) -> Expr<'_> {
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
        // Roc string literals are stored inside the constant section of the program
        // That means this memory is gone when the jit function is done
        // (as opposed to heap memory, which we can leak and then still use after)
        // therefore we must make an owned copy of the string here
        let string = bumpalo::collections::String::from_str_in(string, arena).into_bump_str();
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
