use bumpalo::collections::Vec;
use bumpalo::Bump;
use std::cmp::{max_by_key, min_by_key};

use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::MutMap;
use roc_module::called_via::CalledVia;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::ProcLayout;
use roc_mono::layout::{
    union_sorted_tags_help, Builtin, Layout, LayoutCache, UnionLayout, UnionVariant, WrappedVariant,
};
use roc_parse::ast::{AssignedField, Collection, Expr, StrLiteral};
use roc_region::all::{Loc, Region};
use roc_target::TargetInfo;
use roc_types::subs::{Content, FlatType, GetSubsSlice, RecordFields, Subs, UnionTags, Variable};

use crate::{ReplApp, ReplAppMemory};

struct Env<'a, 'env> {
    arena: &'a Bump,
    subs: &'env Subs,
    target_info: TargetInfo,
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
pub fn jit_to_ast<'a, A: ReplApp<'a>>(
    arena: &'a Bump,
    app: &'a A,
    main_fn_name: &str,
    layout: ProcLayout<'a>,
    content: &'a Content,
    interns: &'a Interns,
    home: ModuleId,
    subs: &'a Subs,
    target_info: TargetInfo,
) -> Result<Expr<'a>, ToAstProblem> {
    let env = Env {
        arena,
        subs,
        target_info,
        interns,
        home,
    };

    match layout {
        ProcLayout {
            arguments: [],
            result,
        } => {
            // this is a thunk
            jit_to_ast_help(&env, app, main_fn_name, &result, content)
        }
        _ => Err(ToAstProblem::FunctionLayout),
    }
}

#[derive(Debug)]
enum NewtypeKind<'a> {
    Tag(&'a TagName),
    RecordField(&'a str),
}

/// Unrolls types that are newtypes. These include
///   - Singleton tags with one type argument (e.g. `Container Str`)
///   - Records with exactly one field (e.g. `{ number: Nat }`)
///
/// This is important in synchronizing `Content`s with `Layout`s, since `Layout`s will
/// always unwrap newtypes and use the content of the underlying type.
///
/// The returned list of newtype containers is ordered by increasing depth. As an example,
/// `A ({b : C 123})` will have the unrolled list `[Tag(A), RecordField(b), Tag(C)]`.
fn unroll_newtypes<'a>(
    env: &Env<'a, 'a>,
    mut content: &'a Content,
) -> (Vec<'a, NewtypeKind<'a>>, &'a Content) {
    let mut newtype_containers = Vec::with_capacity_in(1, env.arena);
    let mut force_alias_content = None;
    loop {
        match content {
            Content::Structure(FlatType::TagUnion(tags, _))
                if tags.is_newtype_wrapper_of_global_tag(env.subs) =>
            {
                let (tag_name, vars): (&TagName, &[Variable]) = tags
                    .unsorted_iterator(env.subs, Variable::EMPTY_TAG_UNION)
                    .next()
                    .unwrap();
                newtype_containers.push(NewtypeKind::Tag(tag_name));
                let var = vars[0];
                content = env.subs.get_content_without_compacting(var);
            }
            Content::Structure(FlatType::Record(fields, _)) if fields.len() == 1 => {
                let (label, field) = fields
                    .sorted_iterator(env.subs, Variable::EMPTY_RECORD)
                    .next()
                    .unwrap();
                newtype_containers.push(NewtypeKind::RecordField(
                    env.arena.alloc_str(label.as_str()),
                ));
                let field_var = *field.as_inner();
                content = env.subs.get_content_without_compacting(field_var);
            }
            Content::Alias(_, _, real_var) => {
                // We need to pass through aliases too, because their underlying types may have
                // unrolled newtypes. In such cases return the list of unrolled newtypes, but keep
                // the content as the alias for readability. For example,
                //   T : { a : Str }
                //   v : T
                //   v = { a : "value" }
                //   v
                // Here we need the newtype container to be `[RecordField(a)]`, but the content to
                // remain as the alias `T`.
                force_alias_content = Some(content);
                content = env.subs.get_content_without_compacting(*real_var);
            }
            _ => return (newtype_containers, force_alias_content.unwrap_or(content)),
        }
    }
}

fn apply_newtypes<'a>(
    env: &Env<'a, '_>,
    newtype_containers: Vec<'a, NewtypeKind<'a>>,
    mut expr: Expr<'a>,
) -> Expr<'a> {
    let arena = env.arena;
    // Reverse order of what we receieve from `unroll_newtypes` since we want the deepest
    // container applied first.
    for container in newtype_containers.into_iter().rev() {
        match container {
            NewtypeKind::Tag(tag_name) => {
                let tag_expr = tag_name_to_expr(env, tag_name);
                let loc_tag_expr = &*arena.alloc(Loc::at_zero(tag_expr));
                let loc_arg_expr = &*arena.alloc(Loc::at_zero(expr));
                let loc_arg_exprs = arena.alloc_slice_copy(&[loc_arg_expr]);
                expr = Expr::Apply(loc_tag_expr, loc_arg_exprs, CalledVia::Space);
            }
            NewtypeKind::RecordField(field_name) => {
                let label = Loc::at_zero(*arena.alloc(field_name));
                let field_val = arena.alloc(Loc::at_zero(expr));
                let field = Loc::at_zero(AssignedField::RequiredValue(label, &[], field_val));
                expr = Expr::Record(Collection::with_items(&*arena.alloc([field])))
            }
        }
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
        union_sorted_tags_help(env.arena, tags_vec, opt_rec_var, env.subs, env.target_info);

    (vars_of_tag, union_variant)
}

fn expr_of_tag<'a, M: ReplAppMemory>(
    env: &Env<'a, 'a>,
    mem: &'a M,
    data_addr: usize,
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
    let output = sequence_of_expr(env, mem, data_addr, it, when_recursive);
    let output = output.into_bump_slice();

    Expr::Apply(loc_tag_expr, output, CalledVia::Space)
}

/// Gets the tag ID of a union variant, assuming that the tag ID is stored alongside (after) the
/// tag data. The caller is expected to check that the tag ID is indeed stored this way.
fn tag_id_from_data<'a, M: ReplAppMemory>(
    env: &Env<'a, 'a>,
    mem: &M,
    union_layout: UnionLayout,
    data_addr: usize,
) -> i64 {
    let offset = union_layout
        .data_size_without_tag_id(env.target_info)
        .unwrap();
    let tag_id_addr = data_addr + offset as usize;

    match union_layout.tag_id_builtin() {
        Builtin::Bool => mem.deref_bool(tag_id_addr) as i64,
        Builtin::Int(IntWidth::U8) => mem.deref_u8(tag_id_addr) as i64,
        Builtin::Int(IntWidth::U16) => mem.deref_u16(tag_id_addr) as i64,
        Builtin::Int(IntWidth::U64) => {
            // used by non-recursive unions at the
            // moment, remove if that is no longer the case
            mem.deref_i64(tag_id_addr)
        }
        _ => unreachable!("invalid tag id layout"),
    }
}

/// Gets the tag ID of a union variant from its recursive pointer (that is, the pointer to the
/// pointer to the data of the union variant). Returns
///   - the tag ID
///   - the address of the data of the union variant, unmasked if the pointer held the tag ID
fn tag_id_from_recursive_ptr<'a, M: ReplAppMemory>(
    env: &Env<'a, 'a>,
    mem: &M,
    union_layout: UnionLayout,
    rec_addr: usize,
) -> (i64, usize) {
    let tag_in_ptr = union_layout.stores_tag_id_in_pointer(env.target_info);
    let addr_with_id = mem.deref_usize(rec_addr);

    if tag_in_ptr {
        let (_, tag_id_mask) = UnionLayout::tag_id_pointer_bits_and_mask(env.target_info);
        let tag_id = addr_with_id & tag_id_mask;
        let data_addr = addr_with_id & !tag_id_mask;
        (tag_id as i64, data_addr)
    } else {
        let tag_id = tag_id_from_data(env, mem, union_layout, addr_with_id);
        (tag_id, addr_with_id)
    }
}

const OPAQUE_FUNCTION: Expr = Expr::Var {
    module_name: "",
    ident: "<function>",
};

fn jit_to_ast_help<'a, A: ReplApp<'a>>(
    env: &Env<'a, 'a>,
    app: &'a A,
    main_fn_name: &str,
    layout: &Layout<'a>,
    content: &'a Content,
) -> Result<Expr<'a>, ToAstProblem> {
    let (newtype_containers, content) = unroll_newtypes(env, content);
    let content = unroll_aliases(env, content);
    let result = match layout {
        Layout::Builtin(Builtin::Bool) => Ok(app
            .call_function(main_fn_name, |mem: &A::Memory, num: bool| {
                bool_to_ast(env, mem, num, content)
            })),
        Layout::Builtin(Builtin::Int(int_width)) => {
            use IntWidth::*;

            macro_rules! helper {
                ($ty:ty) => {
                    app.call_function(main_fn_name, |_, num: $ty| {
                        num_to_ast(env, number_literal_to_ast(env.arena, num), content)
                    })
                };
            }

            let result = match int_width {
                U8 | I8 => {
                    // NOTE: `helper!` does not handle 8-bit numbers yet
                    app.call_function(main_fn_name, |mem: &A::Memory, num: u8| {
                        byte_to_ast(env, mem, num, content)
                    })
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
                    app.call_function(main_fn_name, |_, num: $ty| {
                        num_to_ast(env, number_literal_to_ast(env.arena, num), content)
                    })
                };
            }

            let result = match float_width {
                F32 => helper!(f32),
                F64 => helper!(f64),
                F128 => todo!("F128 not implemented"),
            };

            Ok(result)
        }
        Layout::Builtin(Builtin::Str) => Ok(app
            .call_function(main_fn_name, |_, string: &'static str| {
                str_to_ast(env.arena, env.arena.alloc(string))
            })),
        Layout::Builtin(Builtin::List(elem_layout)) => Ok(app.call_function(
            main_fn_name,
            |mem: &A::Memory, (addr, len): (usize, usize)| {
                list_to_ast(env, mem, addr, len, elem_layout, content)
            },
        )),
        Layout::Builtin(other) => {
            todo!("add support for rendering builtin {:?} to the REPL", other)
        }
        Layout::Struct { field_layouts, .. } => {
            let struct_addr_to_ast = |mem: &'a A::Memory, addr: usize| match content {
                Content::Structure(FlatType::Record(fields, _)) => {
                    Ok(struct_to_ast(env, mem, addr, *fields))
                }
                Content::Structure(FlatType::EmptyRecord) => {
                    Ok(struct_to_ast(env, mem, addr, RecordFields::empty()))
                }
                Content::Structure(FlatType::TagUnion(tags, _)) => {
                    debug_assert_eq!(tags.len(), 1);

                    let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);

                    Ok(single_tag_union_to_ast(
                        env,
                        mem,
                        addr,
                        field_layouts,
                        tag_name,
                        payload_vars,
                    ))
                }
                Content::Structure(FlatType::FunctionOrTagUnion(tag_name, _, _)) => {
                    let tag_name = &env.subs[*tag_name];

                    Ok(single_tag_union_to_ast(
                        env,
                        mem,
                        addr,
                        field_layouts,
                        tag_name,
                        &[],
                    ))
                }
                Content::Structure(FlatType::Func(_, _, _)) => {
                    // a function with a struct as the closure environment
                    Ok(OPAQUE_FUNCTION)
                }
                other => {
                    unreachable!(
                        "Something had a Struct layout, but instead of a Record or TagUnion type, it had: {:?}",
                        other
                    );
                }
            };

            let fields = [Layout::u64(), *layout];
            let layout = Layout::struct_no_name_order(&fields);

            let result_stack_size = layout.stack_size(env.target_info);

            app.call_function_dynamic_size(
                main_fn_name,
                result_stack_size as usize,
                struct_addr_to_ast,
            )
        }
        Layout::Union(UnionLayout::NonRecursive(_)) => {
            let size = layout.stack_size(env.target_info);
            Ok(app.call_function_dynamic_size(
                main_fn_name,
                size as usize,
                |mem: &'a A::Memory, addr: usize| {
                    addr_to_ast(env, mem, addr, layout, WhenRecursive::Unreachable, content)
                },
            ))
        }
        Layout::Union(UnionLayout::Recursive(_))
        | Layout::Union(UnionLayout::NonNullableUnwrapped(_))
        | Layout::Union(UnionLayout::NullableUnwrapped { .. })
        | Layout::Union(UnionLayout::NullableWrapped { .. }) => {
            let size = layout.stack_size(env.target_info);
            Ok(app.call_function_dynamic_size(
                main_fn_name,
                size as usize,
                |mem: &'a A::Memory, addr: usize| {
                    addr_to_ast(
                        env,
                        mem,
                        addr,
                        layout,
                        WhenRecursive::Loop(*layout),
                        content,
                    )
                },
            ))
        }
        Layout::RecursivePointer => {
            unreachable!("RecursivePointers can only be inside structures")
        }
        Layout::LambdaSet(_) => Ok(OPAQUE_FUNCTION),
    };
    result.map(|e| apply_newtypes(env, newtype_containers, e))
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

fn addr_to_ast<'a, M: ReplAppMemory>(
    env: &Env<'a, 'a>,
    mem: &'a M,
    addr: usize,
    layout: &Layout<'a>,
    when_recursive: WhenRecursive<'a>,
    content: &'a Content,
) -> Expr<'a> {
    macro_rules! helper {
        ($method: ident, $ty: ty) => {{
            let num: $ty = mem.$method(addr);

            num_to_ast(env, number_literal_to_ast(env.arena, num), content)
        }};
    }

    let (newtype_containers, content) = unroll_newtypes(env, content);
    let content = unroll_aliases(env, content);
    let expr = match (content, layout) {
        (Content::Structure(FlatType::Func(_, _, _)), _)
        | (_, Layout::LambdaSet(_)) => OPAQUE_FUNCTION,
        (_, Layout::Builtin(Builtin::Bool)) => {
            // TODO: bits are not as expected here.
            // num is always false at the moment.
            let num: bool = mem.deref_bool(addr);

            bool_to_ast(env, mem, num, content)
        }
        (_, Layout::Builtin(Builtin::Int(int_width))) => {
            use IntWidth::*;

            match int_width {
                U8 => helper!(deref_u8, u8),
                U16 => helper!(deref_u16, u16),
                U32 => helper!(deref_u32, u32),
                U64 => helper!(deref_u64, u64),
                U128 => helper!(deref_u128, u128),
                I8 => helper!(deref_i8, i8),
                I16 => helper!(deref_i16, i16),
                I32 => helper!(deref_i32, i32),
                I64 => helper!(deref_i64, i64),
                I128 => helper!(deref_i128, i128),
            }
        }
        (_, Layout::Builtin(Builtin::Float(float_width))) => {
            use FloatWidth::*;

            match float_width {
                F32 => helper!(deref_f32, f32),
                F64 => helper!(deref_f64, f64),
                F128 => todo!("F128 not implemented"),
            }
        }
        (_, Layout::Builtin(Builtin::List(elem_layout))) => {
            let elem_addr = mem.deref_usize(addr);
            let len = mem.deref_usize(addr + env.target_info.ptr_width() as usize);

            list_to_ast(env, mem, elem_addr, len, elem_layout, content)
        }
        (_, Layout::Builtin(Builtin::Str)) => {
            let arena_str = mem.deref_str(addr);

            str_to_ast(env.arena, arena_str)
        }
        (_, Layout::Struct{field_layouts, ..}) => match content {
            Content::Structure(FlatType::Record(fields, _)) => {
                struct_to_ast(env, mem, addr, *fields)
            }
            Content::Structure(FlatType::TagUnion(tags, _)) => {
                debug_assert_eq!(tags.len(), 1);

                let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);
                single_tag_union_to_ast(env, mem, addr, field_layouts, tag_name, payload_vars)
            }
            Content::Structure(FlatType::FunctionOrTagUnion(tag_name, _, _)) => {
                let tag_name = &env.subs[*tag_name];
                single_tag_union_to_ast(env, mem, addr, field_layouts, tag_name, &[])
            }
            Content::Structure(FlatType::EmptyRecord) => {
                struct_to_ast(env, mem, addr, RecordFields::empty())
            }
            other => {
                unreachable!(
                    "Something had a Struct layout, but instead of a Record type, it had: {:?}",
                    other
                );
            }
        },
        (_, Layout::RecursivePointer) => {
            match (content, when_recursive) {
                (Content::RecursionVar {
                    structure,
                    opt_name: _,
                }, WhenRecursive::Loop(union_layout)) => {
                    let content = env.subs.get_content_without_compacting(*structure);
                    addr_to_ast(env, mem, addr, &union_layout, when_recursive, content)
                }
                other => unreachable!("Something had a RecursivePointer layout, but instead of being a RecursionVar and having a known recursive layout, I found {:?}", other),
            }
        }
        (_, Layout::Union(UnionLayout::NonRecursive(union_layouts))) => {
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
            let tag_id = tag_id_from_data(env, mem, union_layout, addr);

            // use the tag ID as an index, to get its name and layout of any arguments
            let (tag_name, arg_layouts) =
                &tags_and_layouts[tag_id as usize];

            expr_of_tag(
                env,
                mem,
                addr,
                tag_name,
                arg_layouts,
                &vars_of_tag[tag_name],
                WhenRecursive::Unreachable,
            )
        }
        (_, Layout::Union(union_layout @ UnionLayout::Recursive(union_layouts))) => {
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

            let (tag_id, ptr_to_data) = tag_id_from_recursive_ptr(env, mem, *union_layout, addr);

            let (tag_name, arg_layouts) = &tags_and_layouts[tag_id as usize];
            expr_of_tag(
                env,
                mem,
                ptr_to_data,
                tag_name,
                arg_layouts,
                &vars_of_tag[tag_name],
                when_recursive,
            )
        }
        (_, Layout::Union(UnionLayout::NonNullableUnwrapped(_))) => {
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

            let data_addr = mem.deref_usize(addr);

            expr_of_tag(
                env,
                mem,
                data_addr,
                &tag_name,
                arg_layouts,
                &vars_of_tag[&tag_name],
                when_recursive,
            )
        }
        (_, Layout::Union(UnionLayout::NullableUnwrapped { .. })) => {
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

            let data_addr = mem.deref_usize(addr);
            if data_addr == 0 {
                tag_name_to_expr(env, &nullable_name)
            } else {
                expr_of_tag(
                    env,
                    mem,
                    data_addr,
                    &other_name,
                    other_arg_layouts,
                    &vars_of_tag[&other_name],
                    when_recursive,
                )
            }
        }
        (_, Layout::Union(union_layout @ UnionLayout::NullableWrapped { .. })) => {
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

            let data_addr = mem.deref_usize(addr);
            if data_addr == 0 {
                tag_name_to_expr(env, &nullable_name)
            } else {
                let (tag_id, data_addr) = tag_id_from_recursive_ptr(env, mem, *union_layout, addr);

                let tag_id = if tag_id > nullable_id.into() { tag_id - 1 } else { tag_id };

                let (tag_name, arg_layouts) = &tags_and_layouts[tag_id as usize];
                expr_of_tag(
                    env,
                    mem,
                    data_addr,
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
    apply_newtypes(env, newtype_containers, expr)
}

fn list_to_ast<'a, M: ReplAppMemory>(
    env: &Env<'a, 'a>,
    mem: &'a M,
    addr: usize,
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
    let elem_size = elem_layout.stack_size(env.target_info) as usize;

    for index in 0..len {
        let offset_bytes = index * elem_size;
        let elem_addr = addr + offset_bytes;
        let (newtype_containers, elem_content) = unroll_newtypes(env, elem_content);
        let expr = addr_to_ast(
            env,
            mem,
            elem_addr,
            elem_layout,
            WhenRecursive::Unreachable,
            elem_content,
        );
        let expr = Loc::at_zero(apply_newtypes(env, newtype_containers, expr));

        output.push(&*arena.alloc(expr));
    }

    let output = output.into_bump_slice();

    Expr::List(Collection::with_items(output))
}

fn single_tag_union_to_ast<'a, M: ReplAppMemory>(
    env: &Env<'a, 'a>,
    mem: &'a M,
    addr: usize,
    field_layouts: &'a [Layout<'a>],
    tag_name: &TagName,
    payload_vars: &[Variable],
) -> Expr<'a> {
    let arena = env.arena;
    let tag_expr = tag_name_to_expr(env, tag_name);

    let loc_tag_expr = &*arena.alloc(Loc::at_zero(tag_expr));

    let output = if field_layouts.len() == payload_vars.len() {
        let it = payload_vars.iter().copied().zip(field_layouts);
        sequence_of_expr(env, mem, addr, it, WhenRecursive::Unreachable).into_bump_slice()
    } else if field_layouts.is_empty() && !payload_vars.is_empty() {
        // happens for e.g. `Foo Bar` where unit structures are nested and the inner one is dropped
        let it = payload_vars.iter().copied().zip([&Layout::UNIT]);
        sequence_of_expr(env, mem, addr, it, WhenRecursive::Unreachable).into_bump_slice()
    } else {
        unreachable!()
    };

    Expr::Apply(loc_tag_expr, output, CalledVia::Space)
}

fn sequence_of_expr<'a, I, M: ReplAppMemory>(
    env: &Env<'a, 'a>,
    mem: &'a M,
    addr: usize,
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
    let mut field_addr = addr;

    for (var, layout) in sequence {
        let content = subs.get_content_without_compacting(var);
        let expr = addr_to_ast(env, mem, field_addr, layout, when_recursive, content);
        let loc_expr = Loc::at_zero(expr);

        output.push(&*arena.alloc(loc_expr));

        // Advance the field pointer to the next field.
        field_addr += layout.stack_size(env.target_info) as usize;
    }

    output
}

fn struct_to_ast<'a, M: ReplAppMemory>(
    env: &Env<'a, 'a>,
    mem: &'a M,
    addr: usize,
    record_fields: RecordFields,
) -> Expr<'a> {
    let arena = env.arena;
    let subs = env.subs;
    let mut output = Vec::with_capacity_in(record_fields.len(), arena);

    let sorted_fields: Vec<_> = Vec::from_iter_in(
        record_fields.sorted_iterator(env.subs, Variable::EMPTY_RECORD),
        arena,
    );

    let mut layout_cache = LayoutCache::new(env.target_info);
    // We recalculate the layouts here because we will have compiled the record so that its fields
    // are sorted by descending alignment, and then alphabetic, but the type of the record is
    // always only sorted alphabetically. We want to arrange the rendered record in the order of
    // the type.
    let field_to_layout: MutMap<Lowercase, Layout> = sorted_fields
        .iter()
        .map(|(label, field)| {
            let layout = layout_cache
                .from_var(arena, *field.as_inner(), env.subs)
                .unwrap();
            (label.clone(), layout)
        })
        .collect();

    if sorted_fields.len() == 1 {
        // this is a 1-field wrapper record around another record or 1-tag tag union
        let (label, field) = sorted_fields.into_iter().next().unwrap();

        let inner_content = env.subs.get_content_without_compacting(field.into_inner());
        debug_assert_eq!(field_to_layout.len(), 1);
        let inner_layouts = arena.alloc([field_to_layout.into_values().next().unwrap()]);

        let loc_expr = &*arena.alloc(Loc {
            value: addr_to_ast(
                env,
                mem,
                addr,
                &Layout::struct_no_name_order(inner_layouts),
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

        let output = arena.alloc([loc_field]);

        Expr::Record(Collection::with_items(output))
    } else {
        debug_assert_eq!(sorted_fields.len(), field_to_layout.len());

        // We'll advance this as we iterate through the fields
        let mut field_addr = addr;

        for (label, field) in sorted_fields.into_iter() {
            let var = field.into_inner();

            let content = subs.get_content_without_compacting(var);
            let field_layout = field_to_layout.get(&label).unwrap();

            let loc_expr = &*arena.alloc(Loc {
                value: addr_to_ast(
                    env,
                    mem,
                    field_addr,
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
            field_addr += field_layout.stack_size(env.target_info) as usize;
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

fn bool_to_ast<'a, M: ReplAppMemory>(
    env: &Env<'a, '_>,
    mem: &M,
    value: bool,
    content: &Content,
) -> Expr<'a> {
    use Content::*;

    let arena = env.arena;

    match content {
        Structure(flat_type) => {
            match flat_type {
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
                            value: bool_to_ast(env, mem, value, content),
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

            bool_to_ast(env, mem, value, content)
        }
        other => {
            unreachable!("Unexpected FlatType {:?} in bool_to_ast", other);
        }
    }
}

fn byte_to_ast<'a, M: ReplAppMemory>(
    env: &Env<'a, '_>,
    mem: &M,
    value: u8,
    content: &Content,
) -> Expr<'a> {
    use Content::*;

    let arena = env.arena;

    match content {
        Structure(flat_type) => {
            match flat_type {
                FlatType::TagUnion(tags, _) if tags.len() == 1 => {
                    let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);

                    // If this tag union represents a number, skip right to
                    // returning it as an Expr::Num
                    if let TagName::Private(Symbol::NUM_AT_NUM) = &tag_name {
                        return Expr::Num(env.arena.alloc_str(&value.to_string()));
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
                            value: byte_to_ast(env, mem, value, content),
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

                    let union_variant = union_sorted_tags_help(
                        env.arena,
                        tags_vec,
                        None,
                        env.subs,
                        env.target_info,
                    );

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

            byte_to_ast(env, mem, value, content)
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
                FlatType::TagUnion(tags, _) => {
                    // This was a single-tag union that got unwrapped at runtime.
                    debug_assert_eq!(tags.len(), 1);

                    let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);

                    // If this tag union represents a number, skip right to
                    // returning it as an Expr::Num
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
        RangedNumber(typ, _) => {
            num_to_ast(env, num_expr, env.subs.get_content_without_compacting(*typ))
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
/// NOTE: As of this writing, we don't have big-endian small strings implemented yet!
fn str_to_ast<'a>(arena: &'a Bump, string: &'a str) -> Expr<'a> {
    const STR_SIZE: usize = 2 * std::mem::size_of::<usize>();

    let bytes: [u8; STR_SIZE] = unsafe { std::mem::transmute(string) };
    let is_small = (bytes[STR_SIZE - 1] & 0b1000_0000) != 0;

    if is_small {
        let len = (bytes[STR_SIZE - 1] & 0b0111_1111) as usize;
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
