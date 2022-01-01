use bumpalo::collections::Vec;
use bumpalo::Bump;
use libloading::Library;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::MutMap;
use roc_gen_llvm::llvm::build::tag_pointer_tag_id_bits_and_mask;
use roc_gen_llvm::{run_jit_function, run_jit_function_dynamic_type};
use roc_module::called_via::CalledVia;
use roc_module::ident::TagName;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::ProcLayout;
use roc_mono::layout::{
    union_sorted_tags_help, Builtin, Layout, UnionLayout, UnionVariant, WrappedVariant,
};
use roc_parse::ast::{AssignedField, Collection, Expr, StrLiteral};
use roc_region::all::{Loc, Region};
use roc_types::subs::{Content, FlatType, GetSubsSlice, RecordFields, Subs, UnionTags, Variable};
use std::cmp::{max_by_key, min_by_key};

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
    content: &'a Content,
    interns: &'a Interns,
    home: ModuleId,
    subs: &'a Subs,
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

// Unrolls tag unions that are newtypes (i.e. are singleton variants with one type argument).
// This is sometimes important in synchronizing `Content`s with `Layout`s, since `Layout`s will
// always unwrap newtypes and use the content of the underlying type.
fn unroll_newtypes<'a>(
    env: &Env<'a, 'a>,
    mut content: &'a Content,
) -> (Vec<'a, &'a TagName>, &'a Content) {
    let mut newtype_tags = Vec::with_capacity_in(1, env.arena);
    loop {
        match content {
            Content::Structure(FlatType::TagUnion(tags, _))
                if tags.is_newtype_wrapper(env.subs) =>
            {
                let (tag_name, vars): (&TagName, &[Variable]) = tags
                    .unsorted_iterator(env.subs, Variable::EMPTY_TAG_UNION)
                    .next()
                    .unwrap();
                newtype_tags.push(tag_name);
                let var = vars[0];
                content = env.subs.get_content_without_compacting(var);
            }
            _ => return (newtype_tags, content),
        }
    }
}

fn apply_newtypes<'a>(
    env: &Env<'a, '_>,
    newtype_tags: Vec<'a, &'a TagName>,
    mut expr: Expr<'a>,
) -> Expr<'a> {
    for tag_name in newtype_tags.into_iter().rev() {
        let tag_expr = tag_name_to_expr(env, tag_name);
        let loc_tag_expr = &*env.arena.alloc(Loc::at_zero(tag_expr));
        let loc_arg_expr = &*env.arena.alloc(Loc::at_zero(expr));
        let loc_arg_exprs = env.arena.alloc_slice_copy(&[loc_arg_expr]);
        expr = Expr::Apply(loc_tag_expr, loc_arg_exprs, CalledVia::Space);
    }
    expr
}

fn unroll_aliases<'a>(env: &Env<'a, 'a>, mut content: &'a Content) -> &'a Content {
    while let Content::Alias(_, _, real) = content {
        content = env.subs.get_content_without_compacting(*real);
    }
    content
}

fn unroll_recursion_var<'a>(env: &Env<'a, 'a>, mut content: &'a Content) -> &'a Content {
    while let Content::RecursionVar { structure, .. } = content {
        content = env.subs.get_content_without_compacting(*structure);
    }
    content
}

fn get_tags_vars_and_variant<'a>(
    env: &Env<'a, '_>,
    tags: &UnionTags,
    opt_rec_var: Option<Variable>,
) -> (MutMap<TagName, std::vec::Vec<Variable>>, UnionVariant<'a>) {
    let tags_vec: std::vec::Vec<(TagName, std::vec::Vec<Variable>)> = tags
        .unsorted_iterator(env.subs, Variable::EMPTY_TAG_UNION)
        .map(|(a, b)| (a.clone(), b.to_vec()))
        .collect();

    let vars_of_tag: MutMap<_, _> = tags_vec.iter().cloned().collect();

    let union_variant =
        union_sorted_tags_help(env.arena, tags_vec, opt_rec_var, env.subs, env.ptr_bytes);

    (vars_of_tag, union_variant)
}

fn expr_of_tag<'a>(
    env: &Env<'a, 'a>,
    ptr_to_data: *const u8,
    tag_name: &TagName,
    arg_layouts: &'a [Layout<'a>],
    arg_vars: &[Variable],
    when_recursive: WhenRecursive<'a>,
) -> Expr<'a> {
    let tag_expr = tag_name_to_expr(env, tag_name);
    let loc_tag_expr = &*env.arena.alloc(Loc::at_zero(tag_expr));

    debug_assert_eq!(arg_layouts.len(), arg_vars.len());

    // NOTE assumes the data bytes are the first bytes
    let it = arg_vars.iter().copied().zip(arg_layouts.iter());
    let output = sequence_of_expr(env, ptr_to_data, it, when_recursive);
    let output = output.into_bump_slice();

    Expr::Apply(loc_tag_expr, output, CalledVia::Space)
}

/// Gets the tag ID of a union variant, assuming that the tag ID is stored alongside (after) the
/// tag data. The caller is expected to check that the tag ID is indeed stored this way.
fn tag_id_from_data(union_layout: UnionLayout, data_ptr: *const u8, ptr_bytes: u32) -> i64 {
    let offset = union_layout.data_size_without_tag_id(ptr_bytes).unwrap();

    unsafe {
        match union_layout.tag_id_builtin() {
            Builtin::Bool => *(data_ptr.add(offset as usize) as *const i8) as i64,
            Builtin::Int(IntWidth::U8) => *(data_ptr.add(offset as usize) as *const i8) as i64,
            Builtin::Int(IntWidth::U16) => *(data_ptr.add(offset as usize) as *const i16) as i64,
            Builtin::Int(IntWidth::U64) => {
                // used by non-recursive unions at the
                // moment, remove if that is no longer the case
                *(data_ptr.add(offset as usize) as *const i64) as i64
            }
            _ => unreachable!("invalid tag id layout"),
        }
    }
}

fn deref_ptr_of_ptr(ptr_of_ptr: *const u8, ptr_bytes: u32) -> *const u8 {
    unsafe {
        match ptr_bytes {
            // Our LLVM codegen represents pointers as i32/i64s.
            4 => *(ptr_of_ptr as *const i32) as *const u8,
            8 => *(ptr_of_ptr as *const i64) as *const u8,
            _ => unreachable!(),
        }
    }
}

/// Gets the tag ID of a union variant from its recursive pointer (that is, the pointer to the
/// pointer to the data of the union variant). Returns
///   - the tag ID
///   - the pointer to the data of the union variant, unmasked if the pointer held the tag ID
fn tag_id_from_recursive_ptr(
    union_layout: UnionLayout,
    rec_ptr: *const u8,
    ptr_bytes: u32,
) -> (i64, *const u8) {
    let tag_in_ptr = union_layout.stores_tag_id_in_pointer(ptr_bytes);
    if tag_in_ptr {
        let masked_ptr_to_data = deref_ptr_of_ptr(rec_ptr, ptr_bytes) as i64;
        let (tag_id_bits, tag_id_mask) = tag_pointer_tag_id_bits_and_mask(ptr_bytes);
        let tag_id = masked_ptr_to_data & (tag_id_mask as i64);

        // Clear the tag ID data from the pointer
        let ptr_to_data = ((masked_ptr_to_data >> tag_id_bits) << tag_id_bits) as *const u8;
        (tag_id as i64, ptr_to_data)
    } else {
        let ptr_to_data = deref_ptr_of_ptr(rec_ptr, ptr_bytes);
        let tag_id = tag_id_from_data(union_layout, ptr_to_data, ptr_bytes);
        (tag_id, ptr_to_data)
    }
}

fn jit_to_ast_help<'a>(
    env: &Env<'a, 'a>,
    lib: Library,
    main_fn_name: &str,
    layout: &Layout<'a>,
    content: &'a Content,
) -> Result<Expr<'a>, ToAstProblem> {
    let (newtype_tags, content) = unroll_newtypes(env, content);
    let content = unroll_aliases(env, content);
    let result = match layout {
        Layout::Builtin(Builtin::Bool) => Ok(run_jit_function!(lib, main_fn_name, bool, |num| {
            bool_to_ast(env, num, content)
        })),
        Layout::Builtin(Builtin::Int(int_width)) => {
            use IntWidth::*;

            macro_rules! helper {
                ($ty:ty) => {
                    run_jit_function!(lib, main_fn_name, $ty, |num| num_to_ast(
                        env,
                        number_literal_to_ast(env.arena, num),
                        content
                    ))
                };
            }

            let result = match int_width {
                U8 | I8 => {
                    // NOTE: this is does not handle 8-bit numbers yet
                    run_jit_function!(lib, main_fn_name, u8, |num| byte_to_ast(env, num, content))
                }
                U16 => helper!(u16),
                U32 => helper!(u32),
                U64 => helper!(u64),
                U128 => helper!(u128),
                I16 => helper!(i16),
                I32 => helper!(i32),
                I64 => helper!(i64),
                I128 => helper!(i128),
            };

            Ok(result)
        }
        Layout::Builtin(Builtin::Float(float_width)) => {
            use FloatWidth::*;

            macro_rules! helper {
                ($ty:ty) => {
                    run_jit_function!(lib, main_fn_name, $ty, |num| num_to_ast(
                        env,
                        number_literal_to_ast(env.arena, num),
                        content
                    ))
                };
            }

            let result = match float_width {
                F32 => helper!(f32),
                F64 => helper!(f64),
                F128 => todo!("F128 not implemented"),
            };

            Ok(result)
        }
        Layout::Builtin(Builtin::Str) => Ok(run_jit_function!(
            lib,
            main_fn_name,
            &'static str,
            |string: &'static str| { str_to_ast(env.arena, env.arena.alloc(string)) }
        )),
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

            let fields = [Layout::u64(), *layout];
            let layout = Layout::Struct(&fields);

            let result_stack_size = layout.stack_size(env.ptr_bytes);

            run_jit_function_dynamic_type!(
                lib,
                main_fn_name,
                result_stack_size as usize,
                |bytes: *const u8| { ptr_to_ast(bytes as *const u8) }
            )
        }
        Layout::Union(UnionLayout::NonRecursive(_)) => {
            let size = layout.stack_size(env.ptr_bytes);
            Ok(run_jit_function_dynamic_type!(
                lib,
                main_fn_name,
                size as usize,
                |ptr: *const u8| {
                    ptr_to_ast(env, ptr, layout, WhenRecursive::Unreachable, content)
                }
            ))
        }
        Layout::Union(UnionLayout::Recursive(_))
        | Layout::Union(UnionLayout::NonNullableUnwrapped(_))
        | Layout::Union(UnionLayout::NullableUnwrapped { .. })
        | Layout::Union(UnionLayout::NullableWrapped { .. }) => {
            let size = layout.stack_size(env.ptr_bytes);
            Ok(run_jit_function_dynamic_type!(
                lib,
                main_fn_name,
                size as usize,
                |ptr: *const u8| {
                    ptr_to_ast(env, ptr, layout, WhenRecursive::Loop(*layout), content)
                }
            ))
        }
        Layout::RecursivePointer => {
            unreachable!("RecursivePointers can only be inside structures")
        }
        Layout::LambdaSet(lambda_set) => jit_to_ast_help(
            env,
            lib,
            main_fn_name,
            &lambda_set.runtime_representation(),
            content,
        ),
    };
    result.map(|e| apply_newtypes(env, newtype_tags, e))
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

/// Represents the layout of `RecursivePointer`s in a tag union, when recursive
/// tag unions are relevant.
#[derive(Clone, Copy, Debug, PartialEq)]
enum WhenRecursive<'a> {
    Unreachable,
    Loop(Layout<'a>),
}

fn ptr_to_ast<'a>(
    env: &Env<'a, 'a>,
    ptr: *const u8,
    layout: &Layout<'a>,
    when_recursive: WhenRecursive<'a>,
    content: &'a Content,
) -> Expr<'a> {
    macro_rules! helper {
        ($ty:ty) => {{
            let num = unsafe { *(ptr as *const $ty) };

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }};
    }

    let (newtype_tags, content) = unroll_newtypes(env, content);
    let content = unroll_aliases(env, content);
    let expr = match layout {
        Layout::Builtin(Builtin::Bool) => {
            // TODO: bits are not as expected here.
            // num is always false at the moment.
            let num = unsafe { *(ptr as *const bool) };

            bool_to_ast(env, num, content)
        }
        Layout::Builtin(Builtin::Int(int_width)) => {
            use IntWidth::*;

            match int_width {
                U8 => helper!(u8),
                U16 => helper!(u16),
                U32 => helper!(u32),
                U64 => helper!(u64),
                U128 => helper!(u128),
                I8 => helper!(i8),
                I16 => helper!(i16),
                I32 => helper!(i32),
                I64 => helper!(i64),
                I128 => helper!(i128),
            }
        }
        Layout::Builtin(Builtin::Float(float_width)) => {
            use FloatWidth::*;

            match float_width {
                F32 => helper!(f32),
                F64 => helper!(f64),
                F128 => todo!("F128 not implemented"),
            }
        }
        Layout::Builtin(Builtin::List(elem_layout)) => {
            // Turn the (ptr, len) wrapper struct into actual ptr and len values.
            let len = unsafe { *(ptr.offset(env.ptr_bytes as isize) as *const usize) };
            let ptr = unsafe { *(ptr as *const *const u8) };

            list_to_ast(env, ptr, len, elem_layout, content)
        }
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
        Layout::RecursivePointer => {
            match (content, when_recursive) {
                (Content::RecursionVar {
                    structure,
                    opt_name: _,
                }, WhenRecursive::Loop(union_layout)) => {
                    let content = env.subs.get_content_without_compacting(*structure);
                    ptr_to_ast(env, ptr, &union_layout, when_recursive, content)
                }
                other => unreachable!("Something had a RecursivePointer layout, but instead of being a RecursionVar and having a known recursive layout, I found {:?}", other),
            }
        }
        Layout::Union(UnionLayout::NonRecursive(union_layouts)) => {
            let union_layout = UnionLayout::NonRecursive(union_layouts);

            let tags = match content {
                Content::Structure(FlatType::TagUnion(tags, _)) => tags,
                other => unreachable!("Weird content for nonrecursive Union layout: {:?}", other),
            };

            debug_assert_eq!(union_layouts.len(), tags.len());

            let (vars_of_tag, union_variant) = get_tags_vars_and_variant(env, tags, None);

            let tags_and_layouts = match union_variant {
                UnionVariant::Wrapped(WrappedVariant::NonRecursive {
                    sorted_tag_layouts
                }) => sorted_tag_layouts,
                other => unreachable!("This layout tag union layout is nonrecursive but the variant isn't; found variant {:?}", other),
            };

            // Because this is a `NonRecursive`, the tag ID is definitely after the data.
            let tag_id =
                tag_id_from_data(union_layout, ptr, env.ptr_bytes);

            // use the tag ID as an index, to get its name and layout of any arguments
            let (tag_name, arg_layouts) =
                &tags_and_layouts[tag_id as usize];

            expr_of_tag(
                env,
                ptr,
                tag_name,
                arg_layouts,
                &vars_of_tag[tag_name],
                WhenRecursive::Unreachable,
            )
        }
        Layout::Union(union_layout @ UnionLayout::Recursive(union_layouts)) => {
            let (rec_var, tags) = match content {
                Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, _)) => (rec_var, tags),
                _ => unreachable!("any other content would have a different layout"),
            };
            debug_assert_eq!(union_layouts.len(), tags.len());

            let (vars_of_tag, union_variant) =
                get_tags_vars_and_variant(env, tags, Some(*rec_var));

            let tags_and_layouts = match union_variant {
                UnionVariant::Wrapped(WrappedVariant::Recursive {
                    sorted_tag_layouts
                }) => sorted_tag_layouts,
                _ => unreachable!("any other variant would have a different layout"),
            };

            let (tag_id, ptr_to_data) = tag_id_from_recursive_ptr(*union_layout, ptr, env.ptr_bytes);

            let (tag_name, arg_layouts) = &tags_and_layouts[tag_id as usize];
            expr_of_tag(
                env,
                ptr_to_data,
                tag_name,
                arg_layouts,
                &vars_of_tag[tag_name],
                when_recursive,
            )
        }
        Layout::Union(UnionLayout::NonNullableUnwrapped(_)) => {
            let (rec_var, tags) = match unroll_recursion_var(env, content) {
                Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, _)) => (rec_var, tags),
                other => unreachable!("Unexpected content for NonNullableUnwrapped: {:?}", other),
            };
            debug_assert_eq!(tags.len(), 1);

            let (vars_of_tag, union_variant) = get_tags_vars_and_variant(env, tags, Some(*rec_var));

            let (tag_name, arg_layouts) = match union_variant {
                UnionVariant::Wrapped(WrappedVariant::NonNullableUnwrapped {
                    tag_name, fields,
                }) => (tag_name, fields),
                _ => unreachable!("any other variant would have a different layout"),
            };

            let ptr_to_data = deref_ptr_of_ptr(ptr, env.ptr_bytes);

            expr_of_tag(
                env,
                ptr_to_data,
                &tag_name,
                arg_layouts,
                &vars_of_tag[&tag_name],
                when_recursive,
            )
        }
        Layout::Union(UnionLayout::NullableUnwrapped { .. }) => {
            let (rec_var, tags) = match unroll_recursion_var(env, content) {
                Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, _)) => (rec_var, tags),
                other => unreachable!("Unexpected content for NonNullableUnwrapped: {:?}", other),
            };
            debug_assert!(tags.len() <= 2);

            let (vars_of_tag, union_variant) = get_tags_vars_and_variant(env, tags, Some(*rec_var));

            let (nullable_name, other_name, other_arg_layouts) = match union_variant {
                UnionVariant::Wrapped(WrappedVariant::NullableUnwrapped {
                    nullable_id: _,
                    nullable_name,
                    other_name,
                    other_fields,
                }) => (nullable_name, other_name, other_fields),
                _ => unreachable!("any other variant would have a different layout"),
            };

            let ptr_to_data = deref_ptr_of_ptr(ptr, env.ptr_bytes);
            if ptr_to_data.is_null() {
                tag_name_to_expr(env, &nullable_name)
            } else {
                expr_of_tag(
                    env,
                    ptr_to_data,
                    &other_name,
                    other_arg_layouts,
                    &vars_of_tag[&other_name],
                    when_recursive,
                )
            }
        }
        Layout::Union(union_layout @ UnionLayout::NullableWrapped { .. }) => {
            let (rec_var, tags) = match unroll_recursion_var(env, content) {
                Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, _)) => (rec_var, tags),
                other => unreachable!("Unexpected content for NonNullableUnwrapped: {:?}", other),
            };

            let (vars_of_tag, union_variant) = get_tags_vars_and_variant(env, tags, Some(*rec_var));

            let (nullable_id, nullable_name, tags_and_layouts) = match union_variant {
                UnionVariant::Wrapped(WrappedVariant::NullableWrapped {
                    nullable_id,
                    nullable_name,
                    sorted_tag_layouts,
                }) => (nullable_id, nullable_name, sorted_tag_layouts),
                _ => unreachable!("any other variant would have a different layout"),
            };

            let ptr_to_data = deref_ptr_of_ptr(ptr, env.ptr_bytes);
            if ptr_to_data.is_null() {
                tag_name_to_expr(env, &nullable_name)
            } else {
                let (tag_id, ptr_to_data) = tag_id_from_recursive_ptr(*union_layout, ptr, env.ptr_bytes);

                let tag_id = if tag_id > nullable_id.into() { tag_id - 1 } else { tag_id };

                let (tag_name, arg_layouts) = &tags_and_layouts[tag_id as usize];
                expr_of_tag(
                    env,
                    ptr_to_data,
                    tag_name,
                    arg_layouts,
                    &vars_of_tag[tag_name],
                    when_recursive,
                )
            }
        }
        other => {
            todo!(
                "TODO add support for rendering pointer to {:?} in the REPL",
                other
            );
        }
    };
    apply_newtypes(env, newtype_tags, expr)
}

fn list_to_ast<'a>(
    env: &Env<'a, 'a>,
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
        let loc_expr = &*arena.alloc(Loc {
            value: ptr_to_ast(
                env,
                elem_ptr,
                elem_layout,
                WhenRecursive::Unreachable,
                elem_content,
            ),
            region: Region::zero(),
        });

        output.push(loc_expr);
    }

    let output = output.into_bump_slice();

    Expr::List(Collection::with_items(output))
}

fn single_tag_union_to_ast<'a>(
    env: &Env<'a, 'a>,
    ptr: *const u8,
    field_layouts: &'a [Layout<'a>],
    tag_name: &TagName,
    payload_vars: &[Variable],
) -> Expr<'a> {
    let arena = env.arena;
    let tag_expr = tag_name_to_expr(env, tag_name);

    let loc_tag_expr = &*arena.alloc(Loc::at_zero(tag_expr));

    let output = if field_layouts.len() == payload_vars.len() {
        let it = payload_vars.iter().copied().zip(field_layouts);
        sequence_of_expr(env, ptr as *const u8, it, WhenRecursive::Unreachable).into_bump_slice()
    } else if field_layouts.is_empty() && !payload_vars.is_empty() {
        // happens for e.g. `Foo Bar` where unit structures are nested and the inner one is dropped
        let it = payload_vars.iter().copied().zip([&Layout::Struct(&[])]);
        sequence_of_expr(env, ptr as *const u8, it, WhenRecursive::Unreachable).into_bump_slice()
    } else {
        unreachable!()
    };

    Expr::Apply(loc_tag_expr, output, CalledVia::Space)
}

fn sequence_of_expr<'a, I>(
    env: &Env<'a, 'a>,
    ptr: *const u8,
    sequence: I,
    when_recursive: WhenRecursive<'a>,
) -> Vec<'a, &'a Loc<Expr<'a>>>
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
        let expr = ptr_to_ast(env, field_ptr, layout, when_recursive, content);
        let loc_expr = Loc::at_zero(expr);

        output.push(&*arena.alloc(loc_expr));

        // Advance the field pointer to the next field.
        field_ptr = unsafe { field_ptr.offset(layout.stack_size(env.ptr_bytes) as isize) };
    }

    output
}

fn struct_to_ast<'a>(
    env: &Env<'a, 'a>,
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

        let loc_expr = &*arena.alloc(Loc {
            value: ptr_to_ast(
                env,
                ptr,
                &Layout::Struct(field_layouts),
                WhenRecursive::Unreachable,
                inner_content,
            ),
            region: Region::zero(),
        });

        let field_name = Loc {
            value: &*arena.alloc_str(label.as_str()),
            region: Region::zero(),
        };
        let loc_field = Loc {
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
            let loc_expr = &*arena.alloc(Loc {
                value: ptr_to_ast(
                    env,
                    field_ptr,
                    field_layout,
                    WhenRecursive::Unreachable,
                    content,
                ),
                region: Region::zero(),
            });

            let field_name = Loc {
                value: &*arena.alloc_str(label.as_str()),
                region: Region::zero(),
            };
            let loc_field = Loc {
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
    let subs_slice = subs[payload_vars_index];
    let payload_vars = subs.get_subs_slice(subs_slice);

    (tag_name, payload_vars)
}

fn unpack_two_element_tag_union(
    subs: &Subs,
    tags: UnionTags,
) -> (&TagName, &[Variable], &TagName, &[Variable]) {
    let mut it = tags.iter_all();
    let (tag_name_index, payload_vars_index) = it.next().unwrap();

    let tag_name1 = &subs[tag_name_index];
    let subs_slice = subs[payload_vars_index];
    let payload_vars1 = subs.get_subs_slice(subs_slice);

    let (tag_name_index, payload_vars_index) = it.next().unwrap();

    let tag_name2 = &subs[tag_name_index];
    let subs_slice = subs[payload_vars_index];
    let payload_vars2 = subs.get_subs_slice(subs_slice);

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

                    let loc_label = Loc {
                        value: &*arena.alloc_str(label.as_str()),
                        region: Region::zero(),
                    };

                    let assigned_field = {
                        // We may be multiple levels deep in nested tag unions
                        // and/or records (e.g. { a: { b: { c: True }  } }),
                        // so we need to do this recursively on the field type.
                        let field_var = *field.as_inner();
                        let field_content = env.subs.get_content_without_compacting(field_var);
                        let loc_expr = Loc {
                            value: bool_to_ast(env, value, field_content),
                            region: Region::zero(),
                        };

                        AssignedField::RequiredValue(loc_label, &[], arena.alloc(loc_expr))
                    };

                    let loc_assigned_field = Loc {
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

                        &*arena.alloc(Loc {
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

                        let loc_payload = &*arena.alloc(Loc {
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

                    let loc_label = Loc {
                        value: &*arena.alloc_str(label.as_str()),
                        region: Region::zero(),
                    };

                    let assigned_field = {
                        // We may be multiple levels deep in nested tag unions
                        // and/or records (e.g. { a: { b: { c: True }  } }),
                        // so we need to do this recursively on the field type.
                        let field_var = *field.as_inner();
                        let field_content = env.subs.get_content_without_compacting(field_var);
                        let loc_expr = Loc {
                            value: byte_to_ast(env, value, field_content),
                            region: Region::zero(),
                        };

                        AssignedField::RequiredValue(loc_label, &[], arena.alloc(loc_expr))
                    };

                    let loc_assigned_field = Loc {
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

                        &*arena.alloc(Loc {
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

                        let loc_payload = &*arena.alloc(Loc {
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
                            let loc_tag_expr = Loc::at_zero(tag_expr);
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

                    let loc_label = Loc {
                        value: &*arena.alloc_str(label.as_str()),
                        region: Region::zero(),
                    };

                    let assigned_field = {
                        // We may be multiple levels deep in nested tag unions
                        // and/or records (e.g. { a: { b: { c: 5 }  } }),
                        // so we need to do this recursively on the field type.
                        let field_var = *field.as_inner();
                        let field_content = env.subs.get_content_without_compacting(field_var);
                        let loc_expr = Loc {
                            value: num_to_ast(env, num_expr, field_content),
                            region: Region::zero(),
                        };

                        AssignedField::RequiredValue(loc_label, &[], arena.alloc(loc_expr))
                    };
                    let loc_assigned_field = Loc {
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

                        &*arena.alloc(Loc {
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

                        let loc_payload = &*arena.alloc(Loc {
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
