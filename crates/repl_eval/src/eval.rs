use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_types::types::AliasKind;
use std::cmp::{max_by_key, min_by_key};

use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::MutMap;
use roc_module::called_via::CalledVia;
use roc_module::ident::TagName;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::ProcLayout;
use roc_mono::layout::{
    self, union_sorted_tags_pub, Builtin, Layout, LayoutCache, LayoutInterner, UnionLayout,
    UnionVariant, WrappedVariant,
};
use roc_parse::ast::{AssignedField, Collection, Expr, StrLiteral};
use roc_region::all::{Loc, Region};
use roc_std::RocDec;
use roc_target::TargetInfo;
use roc_types::subs::{Content, FlatType, GetSubsSlice, RecordFields, Subs, UnionTags, Variable};

use crate::{ReplApp, ReplAppMemory};

struct Env<'a, 'env> {
    arena: &'a Bump,
    subs: &'env Subs,
    target_info: TargetInfo,
    interns: &'a Interns,
    layout_cache: LayoutCache<'a>,
}

#[derive(Debug)]
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
    app: &mut A,
    main_fn_name: &str,
    layout: ProcLayout<'a>,
    content: &Content,
    subs: &Subs,
    interns: &'a Interns,
    layout_interner: LayoutInterner<'a>,
    target_info: TargetInfo,
) -> Result<Expr<'a>, ToAstProblem> {
    let mut env = Env {
        arena,
        subs,
        target_info,
        interns,
        layout_cache: LayoutCache::new(layout_interner, target_info),
    };

    match layout {
        ProcLayout {
            arguments: [],
            result,
            captures_niche: _,
        } => {
            // this is a thunk
            jit_to_ast_help(&mut env, app, main_fn_name, &result, content)
        }
        _ => Err(ToAstProblem::FunctionLayout),
    }
}

#[derive(Debug)]
enum NewtypeKind {
    Tag(TagName),
    RecordField(String),
    Opaque(Symbol),
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
///
/// If we pass through aliases, the top-level alias that should be displayed to the user is passed
/// back as an option.
///
/// Returns (new type containers, optional alias content, real content).
fn unroll_newtypes_and_aliases<'a, 'env>(
    env: &Env<'a, 'env>,

    mut content: &'env Content,
) -> (Vec<'a, NewtypeKind>, Option<&'env Content>, &'env Content) {
    let mut newtype_containers = Vec::with_capacity_in(1, env.arena);
    let mut alias_content = None;
    loop {
        match content {
            Content::Structure(FlatType::TagUnion(tags, _))
                if tags.is_newtype_wrapper(env.subs) =>
            {
                let (tag_name, vars): (&TagName, &[Variable]) = tags
                    .unsorted_iterator(env.subs, Variable::EMPTY_TAG_UNION)
                    .next()
                    .unwrap();
                newtype_containers.push(NewtypeKind::Tag(tag_name.clone()));
                let var = vars[0];
                content = env.subs.get_content_without_compacting(var);
            }
            Content::Structure(FlatType::Record(fields, _)) if fields.len() == 1 => {
                let (label, field) = fields
                    .sorted_iterator(env.subs, Variable::EMPTY_RECORD)
                    .next()
                    .unwrap();
                newtype_containers.push(NewtypeKind::RecordField(label.to_string()));
                content = env.subs.get_content_without_compacting(field.into_inner());
            }
            Content::Alias(name, _, real_var, kind) => {
                // We need to pass through aliases too, because their underlying types may have
                // unrolled newtypes. For example,
                //   T : { a : Str }
                //   v : T
                //   v = { a : "value" }
                //   v
                // Here we need the newtype container to be `[RecordField(a)]`.
                //
                // At the end of the day what we should show to the user is the alias content, not
                // what's inside, so keep that around too.
                if *kind == AliasKind::Opaque && name.module_id() != ModuleId::NUM {
                    newtype_containers.push(NewtypeKind::Opaque(*name));
                }
                alias_content = Some(content);
                content = env.subs.get_content_without_compacting(*real_var);
            }
            _ => return (newtype_containers, alias_content, content),
        }
    }
}

fn apply_newtypes<'a>(
    env: &Env<'a, '_>,
    newtype_containers: &'a [NewtypeKind],
    mut expr: Expr<'a>,
) -> Expr<'a> {
    let arena = env.arena;
    // Reverse order of what we receieve from `unroll_newtypes_and_aliases` since
    // we want the deepest container applied first.
    for container in newtype_containers.iter().rev() {
        match container {
            NewtypeKind::Tag(tag_name) => {
                let tag_expr = tag_name_to_expr(env, tag_name);
                let loc_tag_expr = &*arena.alloc(Loc::at_zero(tag_expr));
                let loc_arg_expr = &*arena.alloc(Loc::at_zero(expr));
                let loc_arg_exprs = arena.alloc_slice_copy(&[loc_arg_expr]);
                expr = Expr::Apply(loc_tag_expr, loc_arg_exprs, CalledVia::Space);
            }
            NewtypeKind::RecordField(field_name) => {
                let label = Loc::at_zero(field_name.as_str());
                let field_val = arena.alloc(Loc::at_zero(expr));
                let field = Loc::at_zero(AssignedField::RequiredValue(label, &[], field_val));
                expr = Expr::Record(Collection::with_items(&*arena.alloc([field])))
            }
            NewtypeKind::Opaque(name) => {
                let opaque_name = arena.alloc(format!("@{}", name.as_str(env.interns)));
                let opaque_ref = &*arena.alloc(Loc::at_zero(Expr::OpaqueRef(opaque_name)));
                let loc_arg_expr = &*arena.alloc(Loc::at_zero(expr));
                let loc_arg_exprs = arena.alloc_slice_copy(&[loc_arg_expr]);
                expr = Expr::Apply(opaque_ref, loc_arg_exprs, CalledVia::Space);
            }
        }
    }
    expr
}

fn unroll_recursion_var<'env>(env: &Env<'_, 'env>, mut content: &'env Content) -> &'env Content {
    while let Content::RecursionVar { structure, .. } = content {
        content = env.subs.get_content_without_compacting(*structure);
    }
    content
}

fn get_tags_vars_and_variant<'a>(
    env: &mut Env<'a, '_>,
    tags: &UnionTags,
    opt_rec_var: Option<Variable>,
) -> (MutMap<TagName, std::vec::Vec<Variable>>, UnionVariant<'a>) {
    let tags_vec: std::vec::Vec<(TagName, std::vec::Vec<Variable>)> = tags
        .unsorted_iterator(env.subs, Variable::EMPTY_TAG_UNION)
        .map(|(a, b)| (a.clone(), b.to_vec()))
        .collect();

    let vars_of_tag: MutMap<_, _> = tags_vec.iter().cloned().collect();

    let union_variant = {
        let mut layout_env = layout::Env::from_components(
            &mut env.layout_cache,
            env.subs,
            env.arena,
            env.target_info,
        );
        union_sorted_tags_pub(&mut layout_env, tags_vec, opt_rec_var)
    };

    (vars_of_tag, union_variant)
}

fn expr_of_tag<'a, 'env, M: ReplAppMemory>(
    env: &mut Env<'a, 'env>,
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
fn tag_id_from_data<'a, 'env, M: ReplAppMemory>(
    env: &Env<'a, 'env>,
    mem: &M,
    union_layout: UnionLayout<'a>,
    data_addr: usize,
) -> i64 {
    let offset = union_layout
        .data_size_without_tag_id(&env.layout_cache.interner, env.target_info)
        .unwrap();
    let tag_id_addr = data_addr + offset as usize;

    use roc_mono::layout::Discriminant::*;
    match union_layout.discriminant() {
        U0 => 0,
        U1 => mem.deref_bool(tag_id_addr) as i64,
        U8 => mem.deref_u8(tag_id_addr) as i64,
        U16 => mem.deref_u16(tag_id_addr) as i64,
    }
}

/// Gets the tag ID of a union variant from its recursive pointer (that is, the pointer to the
/// pointer to the data of the union variant). Returns
///   - the tag ID
///   - the address of the data of the union variant, unmasked if the pointer held the tag ID
fn tag_id_from_recursive_ptr<'a, M: ReplAppMemory>(
    env: &Env<'a, '_>,
    mem: &M,
    union_layout: UnionLayout<'a>,
    rec_addr: usize,
) -> (i64, usize) {
    let tag_in_ptr = union_layout.stores_tag_id_in_pointer(env.target_info);

    if tag_in_ptr {
        let (tag_id, data_addr) = mem.deref_pointer_with_tag_id(rec_addr);
        (tag_id as _, data_addr as _)
    } else {
        let addr_with_id = mem.deref_usize(rec_addr);
        let tag_id = tag_id_from_data(env, mem, union_layout, addr_with_id);
        (tag_id, addr_with_id)
    }
}

const OPAQUE_FUNCTION: Expr = Expr::Var {
    module_name: "",
    ident: "<function>",
};

fn jit_to_ast_help<'a, A: ReplApp<'a>>(
    env: &mut Env<'a, '_>,
    app: &mut A,
    main_fn_name: &str,
    layout: &Layout<'a>,
    content: &Content,
) -> Result<Expr<'a>, ToAstProblem> {
    let (newtype_containers, alias_content, raw_content) =
        unroll_newtypes_and_aliases(env, content);

    macro_rules! num_helper {
        ($ty:ty) => {
            app.call_function(main_fn_name, |_, num: $ty| {
                number_literal_to_ast(env.arena, num)
            })
        };
    }

    let result = match layout {
        Layout::Builtin(Builtin::Bool) => Ok(app
            .call_function(main_fn_name, |mem: &A::Memory, num: bool| {
                bool_to_ast(env, mem, num, raw_content)
            })),
        Layout::Builtin(Builtin::Int(int_width)) => {
            use Content::*;
            use IntWidth::*;

            let result = match (alias_content, int_width) {
                (Some(Alias(Symbol::NUM_UNSIGNED8, ..)), U8) => num_helper!(u8),
                (_, U8) => {
                    // This is not a number, it's a tag union or something else
                    app.call_function(main_fn_name, |mem: &A::Memory, num: u8| {
                        byte_to_ast(env, mem, num, raw_content)
                    })
                }
                // The rest are numbers... for now
                (_, U16) => num_helper!(u16),
                (_, U32) => num_helper!(u32),
                (_, U64) => num_helper!(u64),
                (_, U128) => num_helper!(u128),
                (_, I8) => num_helper!(i8),
                (_, I16) => num_helper!(i16),
                (_, I32) => num_helper!(i32),
                (_, I64) => num_helper!(i64),
                (_, I128) => num_helper!(i128),
            };

            Ok(result)
        }
        Layout::Builtin(Builtin::Float(float_width)) => {
            use FloatWidth::*;

            let result = match float_width {
                F32 => num_helper!(f32),
                F64 => num_helper!(f64),
                F128 => todo!("F128 not implemented"),
            };

            Ok(result)
        }
        Layout::Builtin(Builtin::Decimal) => Ok(num_helper!(RocDec)),
        Layout::Builtin(Builtin::Str) => {
            let body = |mem: &A::Memory, addr| {
                let string = mem.deref_str(addr);
                let arena_str = env.arena.alloc_str(string);
                Expr::Str(StrLiteral::PlainLine(arena_str))
            };

            Ok(app.call_function_returns_roc_str(env.target_info, main_fn_name, body))
        }
        Layout::Builtin(Builtin::List(elem_layout)) => {
            //
            Ok(app.call_function_returns_roc_list(
                main_fn_name,
                |mem: &A::Memory, (addr, len, _cap)| {
                    list_to_ast(env, mem, addr, len, elem_layout, raw_content)
                },
            ))
        }
        Layout::Struct { field_layouts, .. } => {
            let fields = [Layout::u64(), *layout];
            let layout = Layout::struct_no_name_order(env.arena.alloc(fields));

            let result_stack_size = layout.stack_size(&env.layout_cache.interner, env.target_info);

            let struct_addr_to_ast = |mem: &'a A::Memory, addr: usize| match raw_content {
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

            app.call_function_dynamic_size(
                main_fn_name,
                result_stack_size as usize,
                struct_addr_to_ast,
            )
        }
        Layout::Union(UnionLayout::NonRecursive(_)) => {
            let size = layout.stack_size(&env.layout_cache.interner, env.target_info);
            Ok(app.call_function_dynamic_size(
                main_fn_name,
                size as usize,
                |mem: &'a A::Memory, addr: usize| {
                    addr_to_ast(
                        env,
                        mem,
                        addr,
                        layout,
                        WhenRecursive::Unreachable,
                        raw_content,
                    )
                },
            ))
        }
        Layout::Union(UnionLayout::Recursive(_))
        | Layout::Union(UnionLayout::NonNullableUnwrapped(_))
        | Layout::Union(UnionLayout::NullableUnwrapped { .. })
        | Layout::Union(UnionLayout::NullableWrapped { .. }) => {
            let size = layout.stack_size(&env.layout_cache.interner, env.target_info);
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
                        raw_content,
                    )
                },
            ))
        }
        Layout::RecursivePointer => {
            unreachable!("RecursivePointers can only be inside structures")
        }
        Layout::LambdaSet(_) => Ok(OPAQUE_FUNCTION),
        Layout::Boxed(_) => {
            let size = layout.stack_size(&env.layout_cache.interner, env.target_info);
            Ok(app.call_function_dynamic_size(
                main_fn_name,
                size as usize,
                |mem: &A::Memory, addr| {
                    addr_to_ast(
                        env,
                        mem,
                        addr,
                        layout,
                        WhenRecursive::Unreachable,
                        raw_content,
                    )
                },
            ))
        }
    };
    result.map(|e| apply_newtypes(env, newtype_containers.into_bump_slice(), e))
}

fn tag_name_to_expr<'a>(env: &Env<'a, '_>, tag_name: &TagName) -> Expr<'a> {
    Expr::Tag(env.arena.alloc_str(&tag_name.as_ident_str()))
}

/// Represents the layout of `RecursivePointer`s in a tag union, when recursive
/// tag unions are relevant.
#[derive(Clone, Copy, Debug, PartialEq)]
enum WhenRecursive<'a> {
    Unreachable,
    Loop(Layout<'a>),
}

fn addr_to_ast<'a, M: ReplAppMemory>(
    env: &mut Env<'a, '_>,
    mem: &'a M,
    addr: usize,
    layout: &Layout<'a>,
    when_recursive: WhenRecursive<'a>,
    content: &Content,
) -> Expr<'a> {
    macro_rules! helper {
        ($method: ident, $ty: ty) => {{
            let num: $ty = mem.$method(addr);

            number_literal_to_ast(env.arena, num)
        }};
    }

    let (newtype_containers, _alias_content, raw_content) =
        unroll_newtypes_and_aliases(env, content);

    let expr = match (raw_content, layout) {
        (Content::Structure(FlatType::Func(_, _, _)), _) | (_, Layout::LambdaSet(_)) => {
            OPAQUE_FUNCTION
        }
        (_, Layout::Builtin(Builtin::Bool)) => {
            // TODO: bits are not as expected here.
            // num is always false at the moment.
            let num: bool = mem.deref_bool(addr);

            bool_to_ast(env, mem, num, raw_content)
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
            let _cap = mem.deref_usize(addr + 2 * env.target_info.ptr_width() as usize);

            list_to_ast(env, mem, elem_addr, len, elem_layout, raw_content)
        }
        (_, Layout::Builtin(Builtin::Str)) => {
            let string = mem.deref_str(addr);
            let arena_str = env.arena.alloc_str(string);
            Expr::Str(StrLiteral::PlainLine(arena_str))
        }
        (_, Layout::Struct { field_layouts, .. }) => match raw_content {
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
        (_, Layout::RecursivePointer) => match (raw_content, when_recursive) {
            (
                Content::RecursionVar {
                    structure,
                    opt_name: _,
                },
                WhenRecursive::Loop(union_layout),
            ) => {
                let content = env.subs.get_content_without_compacting(*structure);
                addr_to_ast(env, mem, addr, &union_layout, when_recursive, content)
            }

            (
                Content::RecursionVar {
                    structure,
                    opt_name: _,
                },
                WhenRecursive::Unreachable,
            ) => {
                // It's possible to hit a recursive pointer before the full type layout; just
                // figure out the actual recursive structure layout at this point.
                let content = env.subs.get_content_without_compacting(*structure);
                let union_layout = env.layout_cache
                    .from_var(env.arena, *structure, env.subs)
                    .expect("no layout for structure");
                debug_assert!(matches!(union_layout, Layout::Union(..)));
                let when_recursive = WhenRecursive::Loop(union_layout);
                addr_to_ast(env, mem, addr, &union_layout, when_recursive, content)
            }
            other => unreachable!("Something had a RecursivePointer layout, but instead of being a RecursionVar and having a known recursive layout, I found {:?}", other),
        },
        (_, Layout::Union(UnionLayout::NonRecursive(union_layouts))) => {
            let union_layout = UnionLayout::NonRecursive(union_layouts);

            let tags = match raw_content {
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
            let (tag_name, arg_layouts) = &tags_and_layouts[tag_id as usize];

            expr_of_tag(
                env,
                mem,
                addr,
                tag_name.expect_tag_ref(),
                arg_layouts,
                &vars_of_tag[tag_name.expect_tag_ref()],
                WhenRecursive::Unreachable,
            )
        }
        (_, Layout::Union(union_layout @ UnionLayout::Recursive(union_layouts))) => {
            let (rec_var, tags) = match raw_content {
                Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, _)) => {
                    (rec_var, tags)
                }
                Content::RecursionVar { structure, ..} => {
                    match env.subs.get_content_without_compacting(*structure) {
                        Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, _)) => {
                            (rec_var, tags)
                        }
                        content => unreachable!("any other content should have a different layout, but we saw {:#?}", roc_types::subs::SubsFmtContent(content, env.subs)),
                    }
                }
                _ => unreachable!("any other content should have a different layout, but we saw {:#?}", roc_types::subs::SubsFmtContent(raw_content, env.subs)),
            };
            debug_assert_eq!(union_layouts.len(), tags.len());

            let (vars_of_tag, union_variant) = get_tags_vars_and_variant(env, tags, Some(*rec_var));

            let tags_and_layouts = match union_variant {
                UnionVariant::Wrapped(WrappedVariant::Recursive { sorted_tag_layouts }) => {
                    sorted_tag_layouts
                }
                _ => unreachable!("any other variant would have a different layout"),
            };

            let (tag_id, ptr_to_data) = tag_id_from_recursive_ptr(env, mem, *union_layout, addr);

            let (tag_name, arg_layouts) = &tags_and_layouts[tag_id as usize];
            expr_of_tag(
                env,
                mem,
                ptr_to_data,
                tag_name.expect_tag_ref(),
                arg_layouts,
                &vars_of_tag[tag_name.expect_tag_ref()],
                when_recursive,
            )
        }
        (_, Layout::Union(UnionLayout::NonNullableUnwrapped(_))) => {
            let (rec_var, tags) = match unroll_recursion_var(env, raw_content) {
                Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, _)) => {
                    (rec_var, tags)
                }
                other => unreachable!("Unexpected content for NonNullableUnwrapped: {:?}", other),
            };
            debug_assert_eq!(tags.len(), 1);

            let (vars_of_tag, union_variant) = get_tags_vars_and_variant(env, tags, Some(*rec_var));

            let (tag_name, arg_layouts) = match union_variant {
                UnionVariant::Wrapped(WrappedVariant::NonNullableUnwrapped {
                    tag_name,
                    fields,
                }) => (tag_name.expect_tag(), fields),
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
            let (rec_var, tags) = match unroll_recursion_var(env, raw_content) {
                Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, _)) => {
                    (rec_var, tags)
                }
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
                }) => (
                    nullable_name.expect_tag(),
                    other_name.expect_tag(),
                    other_fields,
                ),
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
            let (rec_var, tags) = match unroll_recursion_var(env, raw_content) {
                Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, _)) => {
                    (rec_var, tags)
                }
                other => unreachable!("Unexpected content for NonNullableUnwrapped: {:?}", other),
            };

            let (vars_of_tag, union_variant) = get_tags_vars_and_variant(env, tags, Some(*rec_var));

            let (nullable_id, nullable_name, tags_and_layouts) = match union_variant {
                UnionVariant::Wrapped(WrappedVariant::NullableWrapped {
                    nullable_id,
                    nullable_name,
                    sorted_tag_layouts,
                }) => (nullable_id, nullable_name.expect_tag(), sorted_tag_layouts),
                _ => unreachable!("any other variant would have a different layout"),
            };

            let data_addr = mem.deref_usize(addr);
            if data_addr == 0 {
                tag_name_to_expr(env, &nullable_name)
            } else {
                let (tag_id, data_addr) = tag_id_from_recursive_ptr(env, mem, *union_layout, addr);

                let tag_id = if tag_id > nullable_id.into() {
                    tag_id - 1
                } else {
                    tag_id
                };

                let (tag_name, arg_layouts) = &tags_and_layouts[tag_id as usize];
                expr_of_tag(
                    env,
                    mem,
                    data_addr,
                    tag_name.expect_tag_ref(),
                    arg_layouts,
                    &vars_of_tag[tag_name.expect_tag_ref()],
                    when_recursive,
                )
            }
        }
        (
            Content::Structure(FlatType::Apply(Symbol::BOX_BOX_TYPE, args)),
            Layout::Boxed(inner_layout),
        ) => {
            debug_assert_eq!(args.len(), 1);

            let inner_var_index = args.into_iter().next().unwrap();
            let inner_var = env.subs[inner_var_index];
            let inner_content = env.subs.get_content_without_compacting(inner_var);

            let addr_of_inner = mem.deref_usize(addr);
            let inner_expr = addr_to_ast(
                env,
                mem,
                addr_of_inner,
                inner_layout,
                WhenRecursive::Unreachable,
                inner_content,
            );

            let box_box = env.arena.alloc(Loc::at_zero(Expr::Var {
                module_name: "Box",
                ident: "box",
            }));
            let box_box_arg = &*env.arena.alloc(Loc::at_zero(inner_expr));
            let box_box_args = env.arena.alloc([box_box_arg]);

            Expr::Apply(box_box, box_box_args, CalledVia::Space)
        }
        (_, Layout::Boxed(_)) => {
            unreachable!("Box layouts can only be behind a `Box.Box` application")
        }
        other => {
            todo!(
                "TODO add support for rendering pointer to {:?} in the REPL",
                other
            );
        }
    };
    apply_newtypes(env, newtype_containers.into_bump_slice(), expr)
}

fn list_to_ast<'a, M: ReplAppMemory>(
    env: &mut Env<'a, '_>,
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
    let elem_size = elem_layout.stack_size(&env.layout_cache.interner, env.target_info) as usize;

    for index in 0..len {
        let offset_bytes = index * elem_size;
        let elem_addr = addr + offset_bytes;
        let (newtype_containers, _alias_content, elem_content) =
            unroll_newtypes_and_aliases(env, elem_content);
        let expr = addr_to_ast(
            env,
            mem,
            elem_addr,
            elem_layout,
            WhenRecursive::Unreachable,
            elem_content,
        );
        let expr = Loc::at_zero(apply_newtypes(
            env,
            newtype_containers.into_bump_slice(),
            expr,
        ));

        output.push(&*arena.alloc(expr));
    }

    let output = output.into_bump_slice();

    Expr::List(Collection::with_items(output))
}

fn single_tag_union_to_ast<'a, 'env, M: ReplAppMemory>(
    env: &mut Env<'a, 'env>,
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

fn sequence_of_expr<'a, 'env, I, M: ReplAppMemory>(
    env: &mut Env<'a, 'env>,
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
        field_addr += layout.stack_size(&env.layout_cache.interner, env.target_info) as usize;
    }

    output
}

fn struct_to_ast<'a, 'env, M: ReplAppMemory>(
    env: &mut Env<'a, 'env>,
    mem: &'a M,
    addr: usize,
    record_fields: RecordFields,
) -> Expr<'a> {
    let arena = env.arena;
    let subs = env.subs;
    let mut output = Vec::with_capacity_in(record_fields.len(), arena);

    if record_fields.len() == 1 {
        // this is a 1-field wrapper record around another record or 1-tag tag union
        let (label, field) = record_fields
            .sorted_iterator(subs, Variable::EMPTY_RECORD)
            .next()
            .unwrap();

        let inner_content = env.subs.get_content_without_compacting(field.into_inner());
        let field_layout = env
            .layout_cache
            .from_var(arena, field.into_inner(), env.subs)
            .unwrap();
        let inner_layouts = arena.alloc([field_layout]);

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
        // We'll advance this as we iterate through the fields
        let mut field_addr = addr;

        // We recalculate the layouts here because we will have compiled the record so that its fields
        // are sorted by descending alignment, and then alphabetic, but the type of the record is
        // always only sorted alphabetically. We want to arrange the rendered record in the order of
        // the type.
        for (label, field) in record_fields.sorted_iterator(subs, Variable::EMPTY_RECORD) {
            let content = subs.get_content_without_compacting(field.into_inner());
            let field_layout = env
                .layout_cache
                .from_var(arena, field.into_inner(), env.subs)
                .unwrap();

            let loc_expr = &*arena.alloc(Loc {
                value: addr_to_ast(
                    env,
                    mem,
                    field_addr,
                    &field_layout,
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
            field_addr +=
                field_layout.stack_size(&env.layout_cache.interner, env.target_info) as usize;
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
                        let tag_name = &tag_name.as_ident_str();
                        let tag_expr = Expr::Tag(arena.alloc_str(tag_name));

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
                        max_by_key(tag_name_1, tag_name_2, |n| n.as_ident_str())
                    } else {
                        min_by_key(tag_name_1, tag_name_2, |n| n.as_ident_str())
                    };

                    tag_name_to_expr(env, tag_name)
                }
                other => {
                    unreachable!("Unexpected FlatType {:?} in bool_to_ast", other);
                }
            }
        }
        Alias(_, _, var, _) => {
            let content = env.subs.get_content_without_compacting(*var);

            bool_to_ast(env, mem, value, content)
        }
        other => {
            unreachable!("Unexpected FlatType {:?} in bool_to_ast", other);
        }
    }
}

fn byte_to_ast<'a, M: ReplAppMemory>(
    env: &mut Env<'a, '_>,
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

                    let loc_tag_expr = {
                        let tag_name = &tag_name.as_ident_str();
                        let tag_expr = Expr::Tag(arena.alloc_str(tag_name));

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

                    let union_variant = {
                        let mut layout_env = layout::Env::from_components(
                            &mut env.layout_cache,
                            env.subs,
                            env.arena,
                            env.target_info,
                        );
                        union_sorted_tags_pub(&mut layout_env, tags_vec, None)
                    };

                    match union_variant {
                        UnionVariant::ByteUnion(tagnames) => {
                            let tag_name = &tagnames[value as usize].expect_tag_ref();
                            let tag_expr = tag_name_to_expr(env, tag_name);
                            let loc_tag_expr = Loc::at_zero(tag_expr);
                            Expr::Apply(env.arena.alloc(loc_tag_expr), &[], CalledVia::Space)
                        }
                        _ => unreachable!("invalid union variant for a Byte!"),
                    }
                }
                other => {
                    unreachable!("Unexpected FlatType {:?} in byte_to_ast", other);
                }
            }
        }
        Alias(_, _, var, _) => {
            let content = env.subs.get_content_without_compacting(*var);

            byte_to_ast(env, mem, value, content)
        }
        other => {
            unreachable!("Unexpected FlatType {:?} in byte_to_ast", other);
        }
    }
}

/// This is centralized in case we want to format it differently later,
/// e.g. adding underscores for large numbers
fn number_literal_to_ast<T: std::fmt::Display>(arena: &Bump, num: T) -> Expr<'_> {
    use std::fmt::Write;

    let mut string = bumpalo::collections::String::with_capacity_in(64, arena);
    write!(string, "{}", num).unwrap();
    Expr::Num(string.into_bump_str())
}
