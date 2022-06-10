use crate::types::{RocNum, RocTagUnion, RocType, TypeId, Types};
use indexmap::IndexMap;
use roc_mono::layout::UnionLayout;
use roc_target::{Architecture, TargetInfo};
use std::convert::TryInto;
use std::fmt::Display;

pub static TEMPLATE: &[u8] = include_bytes!("../templates/template.rs");
pub static HEADER: &[u8] = include_bytes!("../templates/header.rs");
const INDENT: &str = "    ";
const DISCRIMINANT_DOC_COMMENT: &str =
    "/// Returns which variant this tag union holds. Note that this never includes a payload!";

type Impls = IndexMap<Impl, IndexMap<String, Vec<TargetInfo>>>;
type Impl = Option<String>;

/// Recursive tag unions need a custom Clone which bumps refcount.
const RECURSIVE_TAG_UNION_CLONE: &str = r#"fn clone(&self) -> Self {
    if let Some(storage) = self.storage() {
        let mut new_storage = storage.get();
        if !new_storage.is_readonly() {
            new_storage.increment_reference_count();
            storage.set(new_storage);
        }
    }

    Self {
        pointer: self.pointer
    }
}"#;

const RECURSIVE_TAG_UNION_STORAGE: &str = r#"#[inline(always)]
    fn storage(&self) -> Option<&core::cell::Cell<roc_std::Storage>> {
        if self.pointer.is_null() {
            None
        } else {
            unsafe {
                Some(&*self.pointer.cast::<core::cell::Cell<roc_std::Storage>>().sub(1))
            }
        }
    }"#;

/// Add the given declaration body, along with the architecture, to the Impls.
/// This can optionally be within an `impl`, or if no `impl` is specified,
/// then it's added at the top level.
fn add_decl(impls: &mut Impls, opt_impl: Impl, target_info: TargetInfo, body: String) {
    let decls = impls.entry(opt_impl).or_default();
    let targets = decls.entry(body).or_default();

    targets.push(target_info);
}

pub fn emit(types_and_targets: &[(Types, TargetInfo)]) -> String {
    let mut buf = String::new();
    let mut impls: Impls = IndexMap::default();

    for (types, target_info) in types_and_targets {
        for id in types.sorted_ids() {
            add_type(*target_info, id, types, &mut impls);
        }
    }

    for (opt_impl, decls) in impls {
        let has_impl;

        if let Some(impl_str) = opt_impl {
            has_impl = true;

            buf.push('\n');
            buf.push_str(&impl_str);
            buf.push_str(" {");
        } else {
            has_impl = false;
        }

        for (decl, targets) in decls {
            // If we're inside an `impl` block, indent the cfg annotation
            let indent = if has_impl { INDENT } else { "" };

            // Push a newline and potentially an indent before the #[cfg(...)] line
            buf.push('\n');
            buf.push_str(indent);

            match targets.len() {
                1 => {
                    let arch = arch_to_str(targets.get(0).unwrap().architecture);

                    buf.push_str(&format!("#[cfg(target_arch = \"{arch}\")]"));
                }
                _ => {
                    // We should never have a decl recorded with 0 targets!
                    debug_assert_ne!(targets.len(), 0);

                    let alternatives = targets
                        .iter()
                        .map(|target_info| {
                            format!(
                                "{indent}{INDENT}target_arch = \"{}\"",
                                arch_to_str(target_info.architecture)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(",\n");

                    buf.push_str(&format!("#[cfg(any(\n{alternatives}\n{indent}))]"));
                }
            }

            buf.push('\n'); // newline after the #[cfg(...)] line

            // indent and print the decl (e.g. a `fn`), with a newline at the end
            buf.push_str(indent);
            buf.push_str(&decl);
            buf.push('\n');
        }

        // If this was an impl, it needs a closing brace at the end.
        if has_impl {
            buf.push_str("}\n");
        }
    }

    buf
}

fn add_type(target_info: TargetInfo, id: TypeId, types: &Types, impls: &mut Impls) {
    match types.get_type(id) {
        RocType::Struct { name, fields } => {
            add_struct(&name, target_info, &fields, id, types, impls, false)
        }
        RocType::TagUnionPayload { name, fields } => {
            add_struct(&name, target_info, &fields, id, types, impls, true)
        }
        RocType::TagUnion(tag_union) => {
            match tag_union {
                RocTagUnion::Enumeration { tags, name } => {
                    if tags.len() == 1 {
                        // An enumeration with one tag is a zero-sized unit type, so
                        // represent it as a zero-sized struct (e.g. "struct Foo()").
                        let derive = derive_str(types.get_type(id), types, true);
                        let struct_name = type_name(id, types);
                        let body = format!("{derive}\nstruct {struct_name}();");

                        add_decl(impls, None, target_info, body);
                    } else {
                        add_enumeration(
                            &name,
                            target_info,
                            types.get_type(id),
                            tags.iter(),
                            types,
                            impls,
                        )
                    }
                }
                RocTagUnion::NonRecursive {
                    tags,
                    name,
                    discriminant_type,
                } => {
                    // Empty tag unions can never come up at runtime,
                    // and so don't need declared types.
                    if !tags.is_empty() {
                        // The discriminant is placed immediately after the last byte of
                        // the longest variant. That means if we take the total size
                        // and subtract the size of the discriminant, we have its offset.
                        //
                        // Importantly, we should use the size *without* alignment rounding;
                        // otherwise, that might not be where the discriminant actually is!
                        let discriminant_offset =
                            types.size_ignoring_alignment(id) - discriminant_type.size();

                        add_tag_union(
                            Recursiveness::NonRecursive,
                            &name,
                            target_info,
                            id,
                            &tags,
                            discriminant_offset,
                            types,
                            impls,
                        );
                    }
                }
                RocTagUnion::Recursive {
                    tags,
                    name,
                    discriminant_type,
                } => {
                    // Empty tag unions can never come up at runtime,
                    // and so don't need declared types.
                    if !tags.is_empty() {
                        // The discriminant is placed immediately after the last byte of
                        // the longest variant. That means if we take the total size
                        // and subtract the size of the discriminant, we have its offset.
                        //
                        // Importantly, we should use the size *without* alignment rounding;
                        // otherwise, that might not be where the discriminant actually is!
                        let discriminant_offset =
                            types.size_ignoring_alignment(id) - discriminant_type.size();

                        add_tag_union(
                            Recursiveness::Recursive,
                            &name,
                            target_info,
                            id,
                            &tags,
                            discriminant_offset,
                            types,
                            impls,
                        );
                    }
                }
                RocTagUnion::NullableWrapped { .. } => {
                    todo!();
                }
                RocTagUnion::NullableUnwrapped {
                    name,
                    null_tag,
                    non_null_tag,
                    non_null_payload,
                    null_represents_first_tag,
                } => add_nullable_unwrapped(
                    &name,
                    target_info,
                    id,
                    &null_tag,
                    &non_null_tag,
                    *non_null_payload,
                    *null_represents_first_tag,
                    types,
                    impls,
                ),
                RocTagUnion::NonNullableUnwrapped { .. } => {
                    todo!();
                }
            }
        }
        // These types don't need to be declared in Rust.
        RocType::Num(_)
        | RocType::Bool
        | RocType::RocStr
        | RocType::RocDict(_, _)
        | RocType::RocSet(_)
        | RocType::RocList(_)
        | RocType::RocBox(_) => {}
        RocType::RecursivePointer { .. } => {
            // This is recursively pointing to a type that should already have been added,
            // so no extra work needs to happen.
        }
    }
}

fn add_discriminant(
    name: &str,
    target_info: TargetInfo,
    tag_names: Vec<String>,
    types: &Types,
    impls: &mut Impls,
) -> String {
    // The tag union's discriminant, e.g.
    //
    // #[repr(u8)]
    // pub enum tag_MyTagUnion {
    //     Bar,
    //     Foo,
    // }
    let discriminant_name = format!("discriminant_{name}");
    let discriminant_type = RocType::TagUnion(RocTagUnion::Enumeration {
        name: discriminant_name.clone(),
        tags: tag_names.clone(),
    });

    add_enumeration(
        &discriminant_name,
        target_info,
        &discriminant_type,
        tag_names.into_iter(),
        types,
        impls,
    );

    discriminant_name
}

#[derive(Copy, Clone)]
enum Recursiveness {
    Recursive,
    NonRecursive,
}

fn add_tag_union(
    recursiveness: Recursiveness,
    name: &str,
    target_info: TargetInfo,
    type_id: TypeId,
    tags: &[(String, Option<TypeId>)],
    discriminant_offset: u32,
    types: &Types,
    impls: &mut Impls,
) {
    // We should never be attempting to bindgen empty tag unions; RocType should not
    // have let this happen.
    debug_assert_ne!(tags.len(), 0);

    let tag_names = tags.iter().map(|(name, _)| name).cloned().collect();
    let discriminant_name = add_discriminant(name, target_info, tag_names, types, impls);
    let typ = types.get_type(type_id);
    let size_rounded_to_alignment = types.size_rounded_to_alignment(type_id);
    let (actual_self, actual_self_mut, actual_other, union_name) = match recursiveness {
        Recursiveness::Recursive => (
            "(&*self.union_pointer())",
            "(&mut *self.union_pointer())",
            "(&*other.union_pointer())",
            format!("union_{name}"),
        ),
        Recursiveness::NonRecursive => ("self", "self", "other", name.to_string()),
    };

    {
        // Recursive tag unions have a public struct, not a public union
        let pub_str;
        let decl_union_name: &str;

        match recursiveness {
            Recursiveness::Recursive => {
                add_decl(
                    impls,
                    None,
                    target_info,
                    format!(
                        r#"
pub struct {name} {{
    pointer: *mut {union_name},
}}
"#
                    ),
                );

                pub_str = "";
                decl_union_name = &union_name;
            }
            Recursiveness::NonRecursive => {
                pub_str = "pub ";
                decl_union_name = name;
            }
        };

        // No #[derive(...)] for unions; we have to generate each impl ourselves!
        let mut buf = format!("#[repr(C)]\n{pub_str}union {decl_union_name} {{\n");

        for (tag_name, opt_payload_id) in tags {
            // If there's no payload, we don't need a discriminant for it.
            if let Some(payload_id) = opt_payload_id {
                let payload_type = types.get_type(*payload_id);

                buf.push_str(&format!("{INDENT}{tag_name}: "));

                if payload_type.has_pointer(types) {
                    // types with pointers need ManuallyDrop
                    // because rust unions don't (and can't)
                    // know how to drop them automatically!
                    buf.push_str(&format!(
                        "core::mem::ManuallyDrop<{}>,\n",
                        type_name(*payload_id, types)
                    ));
                } else {
                    buf.push_str(&type_name(*payload_id, types));
                    buf.push_str(",\n");
                }
            }
        }

        if tags.len() > 1 {
            // When there's a discriminant (so, multiple tags) and there is
            // no alignment padding after the largest variant,
            // the compiler will make extra room for the discriminant.
            // We need that to be reflected in the overall size of the enum,
            // so add an extra variant with the appropriate size.
            //
            // (Do this even if theoretically shouldn't be necessary, since
            // there's no runtime cost and it more explicitly syncs the
            // union's size with what we think it should be.)
            buf.push_str(&format!(
                "{INDENT}_sizer: [u8; {size_rounded_to_alignment}],\n"
            ));
        }

        buf.push('}');

        add_decl(impls, None, target_info, buf);
    }

    // The impl for the tag union
    {
        let opt_impl = Some(format!("impl {name}"));
        let bitmask;

        match recursiveness {
            Recursiveness::Recursive => {
                add_decl(
                    impls,
                    opt_impl.clone(),
                    target_info,
                    RECURSIVE_TAG_UNION_STORAGE.to_string(),
                );

                if tags.len() <= max_pointer_tagged_variants(target_info.architecture) {
                    bitmask = format!("{:#b}", tagged_pointer_bitmask(target_info.architecture));

                    add_decl(
                        impls,
                        opt_impl.clone(),
                        target_info,
                        format!(
                            r#"{DISCRIMINANT_DOC_COMMENT}
    pub fn discriminant(&self) -> {discriminant_name} {{
        // The discriminant is stored in the unused bytes at the end of the recursive pointer
        unsafe {{ core::mem::transmute::<u8, {discriminant_name}>((self.pointer as u8) & {bitmask}) }}
    }}"#
                        ),
                    );

                    add_decl(
                        impls,
                        opt_impl.clone(),
                        target_info,
                        format!(
                            r#"/// Internal helper
    fn tag_discriminant(pointer: *mut {union_name}, discriminant: {discriminant_name}) -> *mut {union_name} {{
        // The discriminant is stored in the unused bytes at the end of the union pointer
        let untagged = (pointer as usize) & (!{bitmask} as usize);
        let tagged = untagged | (discriminant as usize);

        tagged as *mut {union_name}
    }}"#
                        ),
                    );

                    add_decl(
                        impls,
                        opt_impl.clone(),
                        target_info,
                        format!(
                            r#"/// Internal helper
    fn union_pointer(&self) -> *mut {union_name} {{
        // The discriminant is stored in the unused bytes at the end of the union pointer
        ((self.pointer as usize) & (!{bitmask} as usize)) as *mut {union_name}
    }}"#
                        ),
                    );
                } else {
                    todo!(
                        "Support {} tags in a recursive tag union on target_info {:?}. (This is too many tags for pointer tagging to work, so we need to bindgen something different.)",
                        tags.len(),
                        target_info
                    );
                }
            }
            Recursiveness::NonRecursive => {
                // The bitmask doesn't come up in a nonrecursive tag union.
                bitmask = String::new();

                // An old design, which ended up not working out, was that the tag union
                // was a struct containing two fields: one for the `union`, and another
                // for the discriminant.
                //
                // The problem with this was alignment; e.g. if you have one variant with a
                // RocStr in it and another with an I128, then the `union` has a size of 32B
                // and the discriminant is right after it - making the size of the whole struct
                // round up to 48B total, since it has an alignment of 16 from the I128.
                //
                // However, Roc will generate the more efficient thing here: the whole thing will
                // be 32B, and the discriminant will appear at offset 24 - right after the end of
                // the RocStr. The current design recognizes this and works with it, by representing
                // the entire structure as a union and manually setting the tag at the appropriate offset.
                add_decl(
                    impls,
                    opt_impl.clone(),
                    target_info,
                    format!(
                        r#"{DISCRIMINANT_DOC_COMMENT}
    pub fn discriminant(&self) -> {discriminant_name} {{
        unsafe {{
            let bytes = core::mem::transmute::<&Self, &[u8; core::mem::size_of::<Self>()]>(self);

            core::mem::transmute::<u8, {discriminant_name}>(*bytes.as_ptr().add({discriminant_offset}))
        }}
    }}"#
                    ),
                );

                add_decl(
                    impls,
                    opt_impl.clone(),
                    target_info,
                    format!(
                        r#"/// Internal helper
    fn set_discriminant(&mut self, discriminant: {discriminant_name}) {{
        let discriminant_ptr: *mut {discriminant_name} = (self as *mut {name}).cast();

        unsafe {{
            *(discriminant_ptr.add({discriminant_offset})) = discriminant;
        }}
    }}"#
                    ),
                );
            }
        }

        for (tag_name, opt_payload_id) in tags {
            // Add a convenience constructor function to the impl, e.g.
            //
            // /// Construct a tag named Foo, with the appropriate payload
            // pub fn Foo(payload: roc_std::RocStr) -> Self {
            //     Self {
            //         tag: tag_MyTagUnion::Foo,
            //         discriminant: discriminant_MyTagUnion {
            //             Foo: core::mem::ManuallyDrop::new(payload),
            //         },
            //     }
            // }
            if let Some(payload_id) = opt_payload_id {
                let payload_type = types.get_type(*payload_id);
                let self_for_into;
                let payload_args;
                let args_to_payload;
                let owned_ret_type;
                let borrowed_ret_type;
                let owned_get_payload;
                let borrowed_get_payload;
                let owned_ret;
                let borrowed_ret;

                match recursiveness {
                    Recursiveness::Recursive => {
                        if payload_type.has_pointer(types) {
                            owned_get_payload = format!(
                                r#"{{
            let ptr = (self.pointer as usize & !{bitmask}) as *mut {union_name};

            core::mem::ManuallyDrop::take(&mut (*ptr).{tag_name})
        }}"#
                            );
                            borrowed_get_payload = format!(
                                r#"{{
            let ptr = (self.pointer as usize & !{bitmask}) as *mut {union_name};

            &(*ptr).{tag_name}
        }}"#
                            );
                            // we need `mut self` for the argument because of ManuallyDrop
                            self_for_into = "mut self";
                        } else {
                            owned_get_payload = format!(
                                r#"{{
            let ptr = (self.pointer as usize & !{bitmask}) as *mut {union_name};

            core::ptr::read(ptr).{tag_name}
        }}"#
                            );
                            borrowed_get_payload = format!(
                                r#"{{
            let ptr = (self.pointer as usize & !{bitmask}) as *mut {union_name};

            (&ptr).{tag_name}
        }}"#
                            );
                            // we don't need `mut self` unless we need ManuallyDrop
                            self_for_into = "self";
                        };
                    }
                    Recursiveness::NonRecursive => {
                        if payload_type.has_pointer(types) {
                            owned_get_payload =
                                format!("core::mem::ManuallyDrop::take(&mut self.{tag_name})");
                            borrowed_get_payload = format!("&self.{tag_name}");
                            // we need `mut self` for the argument because of ManuallyDrop
                            self_for_into = "mut self";
                        } else {
                            owned_get_payload = format!("self.{tag_name}");
                            borrowed_get_payload = format!("&self.{tag_name}");
                            // we don't need `mut self` unless we need ManuallyDrop
                            self_for_into = "self";
                        };
                    }
                }

                match payload_type {
                    RocType::RocStr
                    | RocType::Bool
                    | RocType::Num(_)
                    | RocType::RocList(_)
                    | RocType::RocDict(_, _)
                    | RocType::RocSet(_)
                    | RocType::RocBox(_)
                    | RocType::TagUnion(_)
                    | RocType::RecursivePointer { .. } => {
                        owned_ret_type = type_name(*payload_id, types);
                        borrowed_ret_type = format!("&{}", owned_ret_type);
                        owned_ret = "payload".to_string();
                        borrowed_ret = format!("&{owned_ret}");
                        payload_args = format!("arg: {owned_ret_type}");
                        args_to_payload = if payload_type.has_pointer(types) {
                            "core::mem::ManuallyDrop::new(arg)".to_string()
                        } else {
                            "arg".to_string()
                        };
                    }
                    RocType::Struct { fields, .. } => {
                        let answer =
                            tag_union_struct_help(fields.iter(), *payload_id, types, false);

                        owned_ret = answer.owned_ret;
                        borrowed_ret = answer.borrowed_ret;
                        owned_ret_type = answer.owned_ret_type;
                        borrowed_ret_type = answer.borrowed_ret_type;
                        payload_args = answer.payload_args;
                        args_to_payload = answer.args_to_payload;
                    }
                    RocType::TagUnionPayload { fields, .. } => {
                        let answer = tag_union_struct_help(fields.iter(), *payload_id, types, true);

                        owned_ret = answer.owned_ret;
                        borrowed_ret = answer.borrowed_ret;
                        owned_ret_type = answer.owned_ret_type;
                        borrowed_ret_type = answer.borrowed_ret_type;
                        payload_args = answer.payload_args;
                        args_to_payload = answer.args_to_payload;
                    }
                };

                {
                    let body = match recursiveness {
                        Recursiveness::Recursive => {
                            format!(
                                r#"
        let size = core::mem::size_of::<{union_name}>();
        let align = core::mem::align_of::<{union_name}>() as u32;

        unsafe {{
            let ptr = crate::roc_alloc(size, align) as *mut {union_name};

            *ptr = {union_name} {{
                {tag_name}: {args_to_payload}
            }};

            Self {{
                pointer: Self::tag_discriminant(ptr, {discriminant_name}::{tag_name}),
            }}
        }}"#
                            )
                        }
                        Recursiveness::NonRecursive => {
                            format!(
                                r#"
        let mut answer = Self {{
            {tag_name}: {args_to_payload}
        }};

        answer.set_discriminant({discriminant_name}::{tag_name});

        answer"#
                            )
                        }
                    };

                    add_decl(
                        impls,
                        opt_impl.clone(),
                        target_info,
                        format!(
                            r#"/// Construct a tag named {tag_name}, with the appropriate payload
    pub fn {tag_name}({payload_args}) -> Self {{{body}
    }}"#
                        ),
                    );
                }

                add_decl(
                    impls,
                    opt_impl.clone(),
                    target_info,
                    format!(
                        r#"/// Unsafely assume the given {name} has a .discriminant() of {tag_name} and convert it to {tag_name}'s payload.
    /// (Always examine .discriminant() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .discriminant() doesn't return {tag_name}.
    pub unsafe fn into_{tag_name}({self_for_into}) -> {owned_ret_type} {{
        debug_assert_eq!(self.discriminant(), {discriminant_name}::{tag_name});

        let payload = {owned_get_payload};

        {owned_ret}
    }}"#,
                    ),
                );

                add_decl(
                    impls,
                    opt_impl.clone(),
                    target_info,
                    format!(
                        r#"/// Unsafely assume the given {name} has a .discriminant() of {tag_name} and return its payload.
    /// (Always examine .discriminant() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .discriminant() doesn't return {tag_name}.
    pub unsafe fn as_{tag_name}(&self) -> {borrowed_ret_type} {{
        debug_assert_eq!(self.discriminant(), {discriminant_name}::{tag_name});

        let payload = {borrowed_get_payload};

        {borrowed_ret}
    }}"#,
                    ),
                );
            } else {
                add_decl(
                    impls,
                    opt_impl.clone(),
                    target_info,
                    format!(
                        r#"/// A tag named {tag_name}, which has no payload.
    pub const {tag_name}: Self = unsafe {{
        let mut bytes = [0; core::mem::size_of::<{name}>()];

        bytes[{discriminant_offset}] = {discriminant_name}::{tag_name} as u8;

        core::mem::transmute::<[u8; core::mem::size_of::<{name}>()], {name}>(bytes)
    }};"#,
                    ),
                );

                add_decl(
                    impls,
                    opt_impl.clone(),
                    target_info,
                    format!(
                        r#"/// Other `into_` methods return a payload, but since the {tag_name} tag
    /// has no payload, this does nothing and is only here for completeness.
    pub fn into_{tag_name}(self) {{
        ()
    }}"#,
                    ),
                );

                add_decl(
                    impls,
                    opt_impl.clone(),
                    target_info,
                    format!(
                        r#"/// Other `as` methods return a payload, but since the {tag_name} tag
    /// has no payload, this does nothing and is only here for completeness.
    pub unsafe fn as_{tag_name}(&self) {{
        ()
    }}"#,
                    ),
                );
            }
        }
    }

    // The Drop impl for the tag union
    {
        let opt_impl = Some(format!("impl Drop for {name}"));
        let mut buf = String::new();

        write_impl_tags(
            2,
            tags.iter(),
            &discriminant_name,
            &mut buf,
            |tag_name, opt_payload_id| {
                match opt_payload_id {
                    Some(payload_id) if types.get_type(payload_id).has_pointer(types) => {
                        format!("unsafe {{ core::mem::ManuallyDrop::drop(&mut {actual_self_mut}.{tag_name}) }},",)
                    }
                    _ => {
                        // If it had no payload, or if the payload had no pointers,
                        // there's nothing to clean up, so do `=> {}` for the branch.
                        "{}".to_string()
                    }
                }
            },
        );

        add_decl(
            impls,
            opt_impl,
            target_info,
            format!("fn drop(&mut self) {{\n{buf}{INDENT}}}"),
        );
    }

    // The PartialEq impl for the tag union
    {
        let opt_impl_prefix = if typ.has_float(types) {
            String::new()
        } else {
            format!("impl Eq for {name} {{}}\n\n")
        };
        let opt_impl = Some(format!("{opt_impl_prefix}impl PartialEq for {name}"));
        let mut buf = r#"fn eq(&self, other: &Self) -> bool {
            if self.discriminant() != other.discriminant() {
                return false;
            }

            unsafe {
"#
        .to_string();

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            &mut buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!("{actual_self}.{tag_name} == {actual_other}.{tag_name},")
                } else {
                    // if the tags themselves had been unequal, we already would have
                    // early-returned with false, so this means the tags were equal
                    // and there's no payload; return true!
                    "true,".to_string()
                }
            },
        );

        buf.push_str(INDENT);
        buf.push_str(INDENT);
        buf.push_str("}\n");
        buf.push_str(INDENT);
        buf.push('}');

        add_decl(impls, opt_impl, target_info, buf);
    }

    // The PartialOrd impl for the tag union
    {
        let opt_impl = Some(format!("impl PartialOrd for {name}"));
        let mut buf = r#"fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        match self.discriminant().partial_cmp(&other.discriminant()) {
            Some(core::cmp::Ordering::Equal) => {}
            not_eq => return not_eq,
        }

        unsafe {
"#
        .to_string();

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            &mut buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!("{actual_self}.{tag_name}.partial_cmp(&{actual_other}.{tag_name}),",)
                } else {
                    // if the tags themselves had been unequal, we already would have
                    // early-returned, so this means the tags were equal and there's
                    // no payload; return Equal!
                    "Some(core::cmp::Ordering::Equal),".to_string()
                }
            },
        );

        buf.push_str(INDENT);
        buf.push_str(INDENT);
        buf.push_str("}\n");
        buf.push_str(INDENT);
        buf.push('}');

        add_decl(impls, opt_impl, target_info, buf);
    }

    // The Ord impl for the tag union
    {
        let opt_impl = Some(format!("impl Ord for {name}"));
        let mut buf = r#"fn cmp(&self, other: &Self) -> core::cmp::Ordering {
            match self.discriminant().cmp(&other.discriminant()) {
                core::cmp::Ordering::Equal => {}
                not_eq => return not_eq,
            }

            unsafe {
"#
        .to_string();

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            &mut buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!("{actual_self}.{tag_name}.cmp(&{actual_other}.{tag_name}),",)
                } else {
                    // if the tags themselves had been unequal, we already would have
                    // early-returned, so this means the tags were equal and there's
                    // no payload; return Equal!
                    "core::cmp::Ordering::Equal,".to_string()
                }
            },
        );

        buf.push_str(INDENT);
        buf.push_str(INDENT);
        buf.push_str("}\n");
        buf.push_str(INDENT);
        buf.push('}');

        add_decl(impls, opt_impl, target_info, buf);
    }

    // The Clone impl for the tag union
    {
        let opt_impl_prefix = if typ.has_pointer(types) {
            String::new()
        } else {
            format!("impl Copy for {name} {{}}\n\n")
        };

        let opt_impl = Some(format!("{opt_impl_prefix}impl Clone for {name}"));
        let body = match recursiveness {
            Recursiveness::Recursive => RECURSIVE_TAG_UNION_CLONE.to_string(),
            Recursiveness::NonRecursive => {
                let mut buf = r#"fn clone(&self) -> Self {
        let mut answer = unsafe {
"#
                .to_string();

                write_impl_tags(
                    3,
                    tags.iter(),
                    &discriminant_name,
                    &mut buf,
                    |tag_name, opt_payload_id| {
                        if opt_payload_id.is_some() {
                            match recursiveness {
                                Recursiveness::Recursive => {
                                    format!(
                                        r#"Self {{
                    {union_name} {{
                        {tag_name}: self.pointer
                    }}
                }},"#,
                                    )
                                }
                                Recursiveness::NonRecursive => {
                                    format!(
                                        r#"Self {{
                    {tag_name}: {actual_self}.{tag_name}.clone(),
                }},"#,
                                    )
                                }
                            }
                        } else {
                            // when there's no payload, initialize to garbage memory.
                            format!(
                                r#"core::mem::transmute::<
                    core::mem::MaybeUninit<{name}>,
                    {name},
                >(core::mem::MaybeUninit::uninit()),"#,
                            )
                        }
                    },
                );

                buf.push_str(
                    r#"
        };

        answer.set_discriminant(self.discriminant());

        answer
    }"#,
                );

                buf
            }
        };

        add_decl(impls, opt_impl, target_info, body);
    }

    // The Hash impl for the tag union
    {
        let opt_impl = Some(format!("impl core::hash::Hash for {name}"));
        let mut buf = r#"fn hash<H: core::hash::Hasher>(&self, state: &mut H) {"#.to_string();

        write_impl_tags(
            2,
            tags.iter(),
            &discriminant_name,
            &mut buf,
            |tag_name, opt_payload_id| {
                let hash_tag = format!("{discriminant_name}::{tag_name}.hash(state)");

                if opt_payload_id.is_some() {
                    format!(
                        r#"unsafe {{
                    {hash_tag};
                    {actual_self}.{tag_name}.hash(state);
                }},"#
                    )
                } else {
                    format!("{},", hash_tag)
                }
            },
        );

        buf.push_str(INDENT);
        buf.push('}');

        add_decl(impls, opt_impl, target_info, buf);
    }

    // The Debug impl for the tag union
    {
        let opt_impl = Some(format!("impl core::fmt::Debug for {name}"));
        let mut buf = format!(
            r#"fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
        f.write_str("{name}::")?;

        unsafe {{
"#
        );

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            &mut buf,
            |tag_name, opt_payload_id| match opt_payload_id {
                Some(payload_id) => {
                    // If it's a ManuallyDrop, we need a `*` prefix to dereference it
                    // (because otherwise we're using ManuallyDrop's Debug instance
                    // rather than the Debug instance of the value it wraps).
                    let payload_type = types.get_type(payload_id);
                    let deref_str = if payload_type.has_pointer(types) {
                        "&*"
                    } else {
                        "&"
                    };

                    let fields_str = match payload_type {
                        RocType::RocStr
                        | RocType::Bool
                        | RocType::Num(_)
                        | RocType::RocList(_)
                        | RocType::RocDict(_, _)
                        | RocType::RocSet(_)
                        | RocType::RocBox(_)
                        | RocType::TagUnion(_)
                        | RocType::Struct { .. }
                        | RocType::RecursivePointer { .. } => {
                            format!(".field({deref_str}{actual_self}.{tag_name})")
                        }
                        RocType::TagUnionPayload { fields, .. } => {
                            let mut buf = Vec::new();

                            for (label, _) in fields {
                                // Needs an "f" prefix
                                buf.push(format!(
                                    ".field(&({deref_str}{actual_self}.{tag_name}).f{label})"
                                ));
                            }

                            buf.join("\n")
                        }
                    };

                    format!(
                        r#"f.debug_tuple("{tag_name}")
        {fields_str}
        .finish(),"#,
                    )
                }
                None => format!(r#"f.write_str("{tag_name}"),"#),
            },
        );

        buf.push_str(INDENT);
        buf.push_str(INDENT);
        buf.push_str("}\n");
        buf.push_str(INDENT);
        buf.push('}');

        add_decl(impls, opt_impl, target_info, buf);
    }
}

fn write_impl_tags<
    'a,
    I: IntoIterator<Item = &'a (String, Option<TypeId>)>,
    F: Fn(&str, Option<TypeId>) -> String,
>(
    indentations: usize,
    tags: I,
    discriminant_name: &str,
    buf: &mut String,
    to_branch_str: F,
) {
    write_indents(indentations, buf);

    buf.push_str("match self.discriminant() {\n");

    for (tag_name, opt_payload_id) in tags {
        let branch_str = to_branch_str(tag_name, *opt_payload_id);

        write_indents(indentations + 1, buf);

        buf.push_str(&format!(
            "{discriminant_name}::{tag_name} => {branch_str}\n"
        ));
    }

    write_indents(indentations, buf);

    buf.push_str("}\n");
}

fn add_enumeration<I: ExactSizeIterator<Item = S>, S: AsRef<str> + Display>(
    name: &str,
    target_info: TargetInfo,
    typ: &RocType,
    tags: I,
    types: &Types,
    impls: &mut Impls,
) {
    let tag_bytes: usize = UnionLayout::discriminant_size(tags.len())
        .stack_size()
        .try_into()
        .unwrap();

    let derive = derive_str(typ, types, false);
    let repr_bytes = tag_bytes * 8;

    // e.g. "#[repr(u8)]\npub enum Foo {\n"
    let mut buf = format!("{derive}\n#[repr(u{repr_bytes})]\npub enum {name} {{\n");

    // Debug impls should never vary by target_info.
    let mut debug_buf = format!(
        r#"impl core::fmt::Debug for {name} {{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
        match self {{
"#
    );

    for (index, tag_name) in tags.enumerate() {
        buf.push_str(&format!("{INDENT}{tag_name} = {index},\n"));

        write_indents(3, &mut debug_buf);

        debug_buf.push_str(&format!(
            "Self::{tag_name} => f.write_str(\"{name}::{tag_name}\"),\n"
        ));
    }

    buf.push_str(&format!(
        "}}\n\n{debug_buf}{INDENT}{INDENT}}}\n{INDENT}}}\n}}"
    ));

    add_decl(impls, None, target_info, buf);
}

fn add_struct<S: Display>(
    name: &str,
    target_info: TargetInfo,
    fields: &[(S, TypeId)],
    struct_id: TypeId,
    types: &Types,
    impls: &mut Impls,
    is_tag_union_payload: bool,
) {
    let derive = derive_str(types.get_type(struct_id), types, true);
    let pub_str = if is_tag_union_payload { "" } else { "pub " };
    let repr = if fields.len() == 1 {
        "transparent"
    } else {
        "C"
    };
    let mut buf = format!("{derive}\n#[repr({repr})]\n{pub_str}struct {name} {{\n");

    for (label, type_id) in fields {
        let type_str = type_name(*type_id, types);

        // Tag union payloads have numbered fields, so we prefix them
        // with an "f" because Rust doesn't allow struct fields to be numbers.
        let label = if is_tag_union_payload {
            format!("f{label}")
        } else {
            format!("{label}")
        };

        buf.push_str(&format!("{INDENT}pub {label}: {type_str},\n",));
    }

    buf.push('}');

    add_decl(impls, None, target_info, buf);
}

fn type_name(id: TypeId, types: &Types) -> String {
    match types.get_type(id) {
        RocType::RocStr => "roc_std::RocStr".to_string(),
        RocType::Bool => "bool".to_string(),
        RocType::Num(RocNum::U8) => "u8".to_string(),
        RocType::Num(RocNum::U16) => "u16".to_string(),
        RocType::Num(RocNum::U32) => "u32".to_string(),
        RocType::Num(RocNum::U64) => "u64".to_string(),
        RocType::Num(RocNum::U128) => "roc_std::U128".to_string(),
        RocType::Num(RocNum::I8) => "i8".to_string(),
        RocType::Num(RocNum::I16) => "i16".to_string(),
        RocType::Num(RocNum::I32) => "i32".to_string(),
        RocType::Num(RocNum::I64) => "i64".to_string(),
        RocType::Num(RocNum::I128) => "roc_std::I128".to_string(),
        RocType::Num(RocNum::F32) => "f32".to_string(),
        RocType::Num(RocNum::F64) => "f64".to_string(),
        RocType::Num(RocNum::F128) => "roc_std::F128".to_string(),
        RocType::Num(RocNum::Dec) => "roc_std::RocDec".to_string(),
        RocType::RocDict(key_id, val_id) => format!(
            "roc_std::RocDict<{}, {}>",
            type_name(*key_id, types),
            type_name(*val_id, types)
        ),
        RocType::RocSet(elem_id) => format!("roc_std::RocSet<{}>", type_name(*elem_id, types)),
        RocType::RocList(elem_id) => format!("roc_std::RocList<{}>", type_name(*elem_id, types)),
        RocType::RocBox(elem_id) => format!("roc_std::RocBox<{}>", type_name(*elem_id, types)),
        RocType::Struct { name, .. }
        | RocType::TagUnionPayload { name, .. }
        | RocType::TagUnion(RocTagUnion::NonRecursive { name, .. })
        | RocType::TagUnion(RocTagUnion::Recursive { name, .. })
        | RocType::TagUnion(RocTagUnion::Enumeration { name, .. })
        | RocType::TagUnion(RocTagUnion::NullableWrapped { name, .. })
        | RocType::TagUnion(RocTagUnion::NullableUnwrapped { name, .. })
        | RocType::TagUnion(RocTagUnion::NonNullableUnwrapped { name, .. }) => name.clone(),
        RocType::RecursivePointer(content) => type_name(*content, types),
    }
}

/// This explicitly asks for whether to include Debug because in the very specific
/// case of a struct that's a payload for a recursive tag union, typ.has_enumeration()
/// will return true, but actually we want to derive Debug here anyway.
fn derive_str(typ: &RocType, types: &Types, include_debug: bool) -> String {
    let mut buf = "#[derive(Clone, ".to_string();

    if !typ.has_pointer(types) {
        buf.push_str("Copy, ");
    }

    if include_debug {
        buf.push_str("Debug, ");
    }

    if !typ.has_enumeration(types) {
        buf.push_str("Default, ");
    }

    if !typ.has_float(types) {
        buf.push_str("Eq, Ord, Hash, ");
    }

    buf.push_str("PartialEq, PartialOrd)]");

    buf
}

#[allow(clippy::too_many_arguments)]
fn add_nullable_unwrapped(
    name: &str,
    target_info: TargetInfo,
    id: TypeId,
    null_tag: &str,
    non_null_tag: &str,
    non_null_payload: TypeId,
    _null_represents_first_tag: bool, // TODO use this!
    types: &Types,
    impls: &mut Impls,
) {
    let mut tag_names = vec![null_tag.to_string(), non_null_tag.to_string()];

    tag_names.sort();

    let discriminant_name = add_discriminant(name, target_info, tag_names, types, impls);
    let payload_type = types.get_type(non_null_payload);
    let payload_type_name = type_name(non_null_payload, types);
    let has_pointer = payload_type.has_pointer(types);

    // The opaque struct for the tag union
    {
        // This struct needs its own Clone impl because it has
        // a refcount to bump
        let derive_extras = if types.get_type(id).has_float(types) {
            ""
        } else {
            ", Eq, Ord, Hash"
        };
        let body = format!(
            r#"#[repr(transparent)]
#[derive(PartialEq, PartialOrd{derive_extras})]
pub struct {name} {{
    pointer: *mut core::mem::ManuallyDrop<{payload_type_name}>,
}}"#
        );

        add_decl(impls, None, target_info, body);
    }

    // The impl for the tag union
    {
        let opt_impl = Some(format!("impl {name}"));

        add_decl(
            impls,
            opt_impl.clone(),
            target_info,
            RECURSIVE_TAG_UNION_STORAGE.to_string(),
        );

        add_decl(
            impls,
            opt_impl.clone(),
            target_info,
            format!(
                r#"{DISCRIMINANT_DOC_COMMENT}
    pub fn discriminant(&self) -> {discriminant_name} {{
        if self.pointer.is_null() {{
            {discriminant_name}::{null_tag}
        }} else {{
            {discriminant_name}::{non_null_tag}
        }}
    }}"#
            ),
        );

        let owned_ret_type;
        let borrowed_ret_type;
        let payload_args;
        let args_to_payload;
        let owned_ret;
        let borrowed_ret;

        match payload_type {
            RocType::RocStr
            | RocType::Bool
            | RocType::Num(_)
            | RocType::RocList(_)
            | RocType::RocDict(_, _)
            | RocType::RocSet(_)
            | RocType::RocBox(_)
            | RocType::TagUnion(_)
            | RocType::RecursivePointer { .. } => {
                owned_ret_type = type_name(non_null_payload, types);
                borrowed_ret_type = format!("&{}", owned_ret_type);
                payload_args = format!("arg: {owned_ret_type}");
                args_to_payload = "arg".to_string();
                owned_ret = "payload".to_string();
                borrowed_ret = format!("&{owned_ret}");
            }
            RocType::Struct { fields, .. } => {
                let answer = tag_union_struct_help(fields.iter(), non_null_payload, types, false);

                payload_args = answer.payload_args;
                args_to_payload = answer.args_to_payload;
                owned_ret = answer.owned_ret;
                borrowed_ret = answer.borrowed_ret;
                owned_ret_type = answer.owned_ret_type;
                borrowed_ret_type = answer.borrowed_ret_type;
            }
            RocType::TagUnionPayload { fields, .. } => {
                let answer = tag_union_struct_help(fields.iter(), non_null_payload, types, true);

                payload_args = answer.payload_args;
                args_to_payload = answer.args_to_payload;
                owned_ret = answer.owned_ret;
                borrowed_ret = answer.borrowed_ret;
                owned_ret_type = answer.owned_ret_type;
                borrowed_ret_type = answer.borrowed_ret_type;
            }
        };

        // Add a convenience constructor function for the tag with the payload, e.g.
        //
        // /// Construct a tag named Cons, with the appropriate payload
        // pub fn Cons(payload: roc_std::RocStr) -> Self {
        //     let size = core::mem::size_of::<roc_std::RocStr>();
        //     let align = core::mem::align_of::<roc_std::RocStr>();
        //
        //     unsafe {
        //         let pointer =
        //             roc_alloc(size, align as u32) as *mut core::mem::ManuallyDrop<roc_std::RocStr>;
        //
        //         *pointer = core::mem::ManuallyDrop::new(payload);
        //
        //         Self { pointer }
        //     }
        // }
        add_decl(
            impls,
            opt_impl.clone(),
            target_info,
            format!(
                r#"/// Construct a tag named {non_null_tag}, with the appropriate payload
    pub fn {non_null_tag}({payload_args}) -> Self {{
        let payload_align = core::mem::align_of::<{payload_type_name}>();
        let self_align = core::mem::align_of::<Self>();
        let size = self_align + core::mem::size_of::<{payload_type_name}>();
        let payload = {args_to_payload};

        unsafe {{
            // Store the payload at `self_align` bytes after the allocation,
            // to leave room for the refcount.
            let alloc_ptr = crate::roc_alloc(size, payload_align as u32);
            let payload_ptr = alloc_ptr.cast::<u8>().add(self_align).cast::<core::mem::ManuallyDrop<{payload_type_name}>>();

            *payload_ptr = payload;

            // The reference count is stored immediately before the payload,
            // which isn't necessarily the same as alloc_ptr - e.g. when alloc_ptr
            // needs an alignment of 16.
            let storage_ptr = payload_ptr.cast::<roc_std::Storage>().sub(1);
            storage_ptr.write(roc_std::Storage::new_reference_counted());

            Self {{ pointer: payload_ptr }}
        }}
    }}"#,
            ),
        );

        {
            let assign_payload = if has_pointer {
                "core::mem::ManuallyDrop::take(&mut *self.pointer)"
            } else {
                "*self.pointer"
            };

            add_decl(
                impls,
                opt_impl.clone(),
                target_info,
                format!(
                    r#"/// Unsafely assume the given {name} has a .discriminant() of {non_null_tag} and convert it to {non_null_tag}'s payload.
    /// (Always examine .discriminant() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .discriminant() doesn't return {non_null_tag}.
    pub unsafe fn into_{non_null_tag}(self) -> {owned_ret_type} {{
        debug_assert_eq!(self.discriminant(), {discriminant_name}::{non_null_tag});

        let payload = {assign_payload};

        core::mem::drop::<Self>(self);

        {owned_ret}
    }}"#,
                ),
            );
        }

        add_decl(
            impls,
            opt_impl.clone(),
            target_info,
            format!(
                r#"/// Unsafely assume the given {name} has a .discriminant() of {non_null_tag} and return its payload.
    /// (Always examine .discriminant() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .discriminant() doesn't return {non_null_tag}.
    pub unsafe fn as_{non_null_tag}(&self) -> {borrowed_ret_type} {{
        debug_assert_eq!(self.discriminant(), {discriminant_name}::{non_null_tag});

        let payload = &*self.pointer;

        {borrowed_ret}
    }}"#,
            ),
        );

        // Add a convenience constructor function for the nullable tag, e.g.
        //
        // /// A tag named Nil, which has no payload.
        // pub const Nil: Self = Self {
        //     pointer: core::ptr::null_mut(),
        // };
        add_decl(
            impls,
            opt_impl.clone(),
            target_info,
            format!(
                r#"/// A tag named {null_tag}, which has no payload.
    pub const {null_tag}: Self = Self {{
        pointer: core::ptr::null_mut(),
    }};"#,
            ),
        );

        add_decl(
            impls,
            opt_impl.clone(),
            target_info,
            format!(
                r#"/// Other `into_` methods return a payload, but since the {null_tag} tag
    /// has no payload, this does nothing and is only here for completeness.
    pub fn into_{null_tag}(self) {{
        ()
    }}"#,
            ),
        );

        add_decl(
            impls,
            opt_impl,
            target_info,
            format!(
                r#"/// Other `as` methods return a payload, but since the {null_tag} tag
    /// has no payload, this does nothing and is only here for completeness.
    pub unsafe fn as_{null_tag}(&self) {{
        ()
    }}"#,
            ),
        );
    }

    // The Clone impl for the tag union
    {
        // Note that these never have Copy because they always contain a pointer.
        let opt_impl = Some(format!("impl Clone for {name}"));

        add_decl(
            impls,
            opt_impl,
            target_info,
            RECURSIVE_TAG_UNION_CLONE.to_string(),
        );
    }

    // The Drop impl for the tag union
    {
        let opt_impl = Some(format!("impl Drop for {name}"));

        add_decl(
            impls,
            opt_impl,
            target_info,
            format!(
                r#"fn drop(&mut self) {{
        if let Some(storage) = self.storage() {{
            // Decrement the refcount and return early if no dealloc is needed
            {{
                let mut new_storage = storage.get();

                if new_storage.is_readonly() {{
                    return;
                }}

                let needs_dealloc = new_storage.decrease();

                if !needs_dealloc {{
                    // Write the storage back.
                    storage.set(new_storage);

                    return;
                }}
            }}

            if !self.pointer.is_null() {{
                // If there is a payload, drop it first.
               let payload = unsafe {{ core::mem::ManuallyDrop::take(&mut *self.pointer) }};

                core::mem::drop::<{payload_type_name}>(payload);
            }}

            // Dealloc the pointer
            unsafe {{
                let alignment = core::mem::align_of::<Self>().max(core::mem::align_of::<roc_std::Storage>());
                let alloc_ptr = self.pointer.cast::<u8>().sub(alignment);

                crate::roc_dealloc(
                    alloc_ptr as *mut core::ffi::c_void,
                    alignment as u32,
                );
            }}
        }}
    }}"#
            ),
        );
    }

    // The Debug impl for the tag union
    {
        let opt_impl = Some(format!("impl core::fmt::Debug for {name}"));
        let extra_deref = if has_pointer { "*" } else { "" };

        let fields_str = match payload_type {
            RocType::RocStr
            | RocType::Bool
            | RocType::Num(_)
            | RocType::RocList(_)
            | RocType::RocDict(_, _)
            | RocType::RocSet(_)
            | RocType::RocBox(_)
            | RocType::TagUnion(_)
            | RocType::RecursivePointer { .. } => {
                format!(
                    r#"f.debug_tuple("{non_null_tag}").field(&*{extra_deref}self.pointer).finish()"#
                )
            }
            RocType::Struct { fields, .. } => {
                let mut buf = Vec::new();

                for (label, _) in fields {
                    buf.push(format!(".field(&(&*{extra_deref}self.pointer).{label})"));
                }

                buf.join(&format!("\n{INDENT}{INDENT}{INDENT}{INDENT}{INDENT}"))
            }
            RocType::TagUnionPayload { fields, .. } => {
                let mut buf = Vec::new();

                for (label, _) in fields {
                    // Needs an "f" prefix
                    buf.push(format!(".field(&(&*{extra_deref}self.pointer).f{label})"));
                }

                buf.join(&format!("\n{INDENT}{INDENT}{INDENT}{INDENT}{INDENT}"))
            }
        };

        let body = format!(
            r#"fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
        if self.pointer.is_null() {{
            f.write_str("{name}::{null_tag}")
        }} else {{
            f.write_str("{name}::")?;

            unsafe {{
                f.debug_tuple("{non_null_tag}")
                    {fields_str}
                    .finish()
            }}
        }}
    }}"#
        );

        add_decl(impls, opt_impl, target_info, body);
    }
}

fn arch_to_str(architecture: Architecture) -> &'static str {
    match architecture {
        Architecture::X86_64 => "x86_64",
        Architecture::X86_32 => "x86",
        Architecture::Aarch64 => "aarch64",
        Architecture::Aarch32 => "arm",
        Architecture::Wasm32 => "wasm32",
    }
}

fn write_indents(indentations: usize, buf: &mut String) {
    for _ in 0..indentations {
        buf.push_str(INDENT);
    }
}

fn max_pointer_tagged_variants(architecture: Architecture) -> usize {
    match architecture {
        // On a 64-bit system, pointers have 3 bits that are unused, so return 2^3 = 8
        Architecture::X86_64 | Architecture::Aarch64 => 8,
        // On a 32-bit system, pointers have 2 bits that are unused, so return 2^4 = 4
        Architecture::X86_32 | Architecture::Aarch32 | Architecture::Wasm32 => 4,
    }
}

#[inline(always)]
fn tagged_pointer_bitmask(architecture: Architecture) -> u8 {
    match architecture {
        // On a 64-bit system, pointers have 3 bits that are unused
        Architecture::X86_64 | Architecture::Aarch64 => 0b0000_0111,
        // On a 32-bit system, pointers have 2 bits that are unused
        Architecture::X86_32 | Architecture::Aarch32 | Architecture::Wasm32 => 0b0000_0011,
    }
}

struct StructIngredients {
    payload_args: String,
    args_to_payload: String,
    owned_ret: String,
    borrowed_ret: String,
    owned_ret_type: String,
    borrowed_ret_type: String,
}

fn tag_union_struct_help<'a, I: Iterator<Item = &'a (L, TypeId)>, L: Display + PartialOrd + 'a>(
    fields: I,
    payload_id: TypeId,
    types: &Types,
    is_tag_union_payload: bool,
) -> StructIngredients {
    let mut sorted_fields = fields.collect::<Vec<&(L, TypeId)>>();

    sorted_fields.sort_by(|(label1, _), (label2, _)| label1.partial_cmp(label2).unwrap());

    let mut ret_types = Vec::new();
    let mut ret_values = Vec::new();

    for (label, type_id) in sorted_fields.iter() {
        let label = if is_tag_union_payload {
            // Tag union payload fields need "f" prefix
            // because they're numbers
            format!("f{}", label)
        } else {
            format!("{}", label)
        };

        ret_values.push(format!("payload.{label}"));
        ret_types.push(type_name(*type_id, types));
    }

    let payload_type_name = type_name(payload_id, types);
    let payload_args = ret_types
        .iter()
        .enumerate()
        .map(|(index, typ)| format!("arg{index}: {typ}"))
        .collect::<Vec<String>>()
        .join(", ");
    let args_to_payload = format!(
        "core::mem::ManuallyDrop::new({payload_type_name} {{\n{}\n{INDENT}{INDENT}{INDENT}{INDENT}}})",
        sorted_fields
            .iter()
            .enumerate()
            .map(|(index, (label, _))| {
                let mut indents = String::new();

                for _ in 0..5 {
                    indents.push_str(INDENT);
                }

                let label = if is_tag_union_payload {
                    // Tag union payload fields need "f" prefix
                    // because they're numbers
                    format!("f{}", label)
                } else {
                    format!("{}", label)
                };

                format!("{indents}{label}: arg{index},")
            })
            .collect::<Vec<String>>()
            .join("\n")
    );
    let owned_ret;
    let borrowed_ret;
    let owned_ret_type;
    let borrowed_ret_type;

    if ret_types.len() == 1 {
        owned_ret_type = ret_types.join("");
        borrowed_ret_type = format!("&{owned_ret_type}");

        let ret_val = ret_values.first().unwrap();
        owned_ret = format!("\n{INDENT}{INDENT}{ret_val}");
        borrowed_ret = format!("\n{INDENT}{INDENT}&{ret_val}");
    } else {
        owned_ret_type = format!("({})", ret_types.join(", "));
        borrowed_ret_type = format!(
            "({})",
            ret_types
                .iter()
                .map(|ret_type| { format!("&{ret_type}") })
                .collect::<Vec<String>>()
                .join(", ")
        );
        owned_ret = {
            let lines = ret_values
                .iter()
                .map(|line| format!("\n{INDENT}{INDENT}{INDENT}{line}"))
                .collect::<Vec<String>>()
                .join(", ");

            format!("({lines}\n{INDENT}{INDENT})")
        };
        borrowed_ret = {
            let lines = ret_values
                .iter()
                .map(|line| format!("\n{INDENT}{INDENT}{INDENT}&{line}"))
                .collect::<Vec<String>>()
                .join(", ");

            format!("({lines}\n{INDENT}{INDENT})")
        };
    }

    StructIngredients {
        payload_args,
        args_to_payload,
        owned_ret,
        borrowed_ret,
        owned_ret_type,
        borrowed_ret_type,
    }
}
