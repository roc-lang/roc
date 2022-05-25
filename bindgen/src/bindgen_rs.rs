use crate::types::{Field, RocTagUnion, RocType, TypeId, Types};
use indexmap::IndexMap;
use roc_mono::layout::UnionLayout;
use roc_target::{Architecture, TargetInfo};
use std::convert::TryInto;
use std::fmt::Display;

pub static TEMPLATE: &[u8] = include_bytes!("../templates/template.rs");
pub static HEADER: &[u8] = include_bytes!("../templates/header.rs");
const INDENT: &str = "    ";
const VARIANT_DOC_COMMENT: &str =
    "/// Returns which variant this tag union holds. Note that this never includes a payload!";

type Impls = IndexMap<Impl, IndexMap<String, Vec<Architecture>>>;
type Impl = Option<String>;

/// Add the given declaration body, along with the architecture, to the Impls.
/// This can optionally be within an `impl`, or if no `impl` is specified,
/// then it's added at the top level.
fn add_decl(impls: &mut Impls, opt_impl: Impl, architecture: Architecture, body: String) {
    let decls = impls.entry(opt_impl).or_default();
    let architectures = decls.entry(body).or_default();

    architectures.push(architecture);
}

pub fn emit(types_by_architecture: &[(Architecture, Types)]) -> String {
    let mut buf = String::new();
    let mut impls: Impls = IndexMap::default();

    for (architecture, types) in types_by_architecture.iter() {
        for id in types.sorted_ids() {
            add_type(*architecture, id, types, &mut impls);
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

        for (decl, architectures) in decls {
            // If we're inside an `impl` block, indent the cfg annotation
            let indent = if has_impl { INDENT } else { "" };

            // Push a newline and potentially an indent before the #[cfg(...)] line
            buf.push('\n');
            buf.push_str(indent);

            match architectures.len() {
                1 => {
                    let arch = arch_to_str(architectures.get(0).unwrap());

                    buf.push_str(&format!("r#[cfg(target_arch = \"{arch}\")]"));
                }
                _ => {
                    // We should never have a decl recorded with 0 architectures!
                    debug_assert_ne!(architectures.len(), 0);

                    let alternatives = architectures
                        .iter()
                        .map(|arch| {
                            format!("{indent}{INDENT}target_arch = \"{}\"", arch_to_str(arch))
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

fn add_type(architecture: Architecture, id: TypeId, types: &Types, impls: &mut Impls) {
    match types.get(id) {
        RocType::Struct { name, fields } => {
            add_struct(name, architecture, fields, id, types, impls)
        }
        RocType::TagUnion(tag_union) => {
            match tag_union {
                RocTagUnion::Enumeration { tags, name } => {
                    if tags.len() == 1 {
                        // An enumeration with one tag is a zero-sized unit type, so
                        // represent it as a zero-sized struct (e.g. "struct Foo()").
                        let derive = derive_str(types.get(id), types, true);
                        let struct_name = type_name(id, types);
                        let body = format!("{derive}\nstruct {struct_name}();");

                        add_decl(impls, None, architecture, body);
                    } else {
                        add_enumeration(
                            name,
                            architecture,
                            types.get(id),
                            tags.iter(),
                            types,
                            impls,
                        )
                    }
                }
                RocTagUnion::NonRecursive { tags, name } => {
                    // Empty tag unions can never come up at runtime,
                    // and so don't need declared types.
                    if !tags.is_empty() {
                        add_tag_union(name, architecture, id, tags, types, impls);
                    }
                }
                RocTagUnion::Recursive { .. } => {
                    todo!();
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
                    name,
                    architecture,
                    id,
                    null_tag,
                    non_null_tag,
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
        RocType::U8
        | RocType::U16
        | RocType::U32
        | RocType::U64
        | RocType::U128
        | RocType::I8
        | RocType::I16
        | RocType::I32
        | RocType::I64
        | RocType::I128
        | RocType::F32
        | RocType::F64
        | RocType::F128
        | RocType::Bool
        | RocType::RocDec
        | RocType::RocStr
        | RocType::RocDict(_, _)
        | RocType::RocSet(_)
        | RocType::RocList(_)
        | RocType::RocBox(_) => {}
        RocType::TransparentWrapper { name, content } => {
            let typ = types.get(id);
            let derive = derive_str(typ, types, typ.has_enumeration(types));
            let body = format!(
                "{derive}\n#[repr(transparent)]\npub struct {name}({});",
                type_name(*content, types)
            );

            add_decl(impls, None, architecture, body);
        }
    }
}

fn add_discriminant(
    name: &str,
    architecture: Architecture,
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
    let discriminant_name = format!("variant_{name}");
    let discriminant_type = RocType::TagUnion(RocTagUnion::Enumeration {
        name: discriminant_name.clone(),
        tags: tag_names.clone(),
    });

    add_enumeration(
        &discriminant_name,
        architecture,
        &discriminant_type,
        tag_names.into_iter(),
        types,
        impls,
    );

    discriminant_name
}

fn add_tag_union(
    name: &str,
    architecture: Architecture,
    type_id: TypeId,
    tags: &[(String, Option<TypeId>)],
    types: &Types,
    impls: &mut Impls,
) {
    let tag_names = tags.iter().map(|(name, _)| name).cloned().collect();
    let discriminant_name = add_discriminant(name, architecture, tag_names, types, impls);
    let typ = types.get(type_id);
    // TODO also do this for other targets. Remember, these can change based on more
    // than just pointer width; e.g. on wasm, the alignments of U16 and U8 are both 4!
    let target_info = TargetInfo::from(&target_lexicon::Triple::host());
    let discriminant_offset = RocTagUnion::discriminant_offset(tags, types, target_info);
    let size = typ.size(types, target_info);

    {
        // No #[derive(...)] for unions; we have to generate each impl ourselves!
        let mut buf = format!("#[repr(C)]\npub union {name} {{\n");

        for (tag_name, opt_payload_id) in tags {
            // If there's no payload, we don't need a variant for it.
            if let Some(payload_id) = opt_payload_id {
                let payload_type = types.get(*payload_id);

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

        // When there's no alignment padding after the largest variant,
        // the compiler will make extra room for the discriminant.
        // We need that to be reflected in the overall size of the enum,
        // so add an extra variant with the appropriate size.
        //
        // (Do this even if theoretically shouldn't be necessary, since
        // there's no runtime cost and it more explicitly syncs the
        // union's size with what we think it should be.)
        buf.push_str(&format!("{INDENT}_sizer: [u8; {size}],\n}}"));

        add_decl(impls, None, architecture, buf);
    }

    // The impl for the tag union
    {
        let opt_impl = Some(format!("impl {name}"));

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
            architecture,
            format!(
                r#"{VARIANT_DOC_COMMENT}
    pub fn variant(&self) -> {discriminant_name} {{
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
            architecture,
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

        for (tag_name, opt_payload_id) in tags {
            // Add a convenience constructor function to the impl, e.g.
            //
            // /// Construct a tag named Foo, with the appropriate payload
            // pub fn Foo(payload: roc_std::RocStr) -> Self {
            //     Self {
            //         tag: tag_MyTagUnion::Foo,
            //         variant: variant_MyTagUnion {
            //             Foo: core::mem::ManuallyDrop::new(payload),
            //         },
            //     }
            // }
            if let Some(payload_id) = opt_payload_id {
                let payload_type = types.get(*payload_id);
                let payload_type_name = type_name(*payload_id, types);

                let (init_payload, get_payload, ref_if_needed, self_for_into) =
                    if payload_type.has_pointer(types) {
                        (
                            "core::mem::ManuallyDrop::new(payload)",
                            format!("core::mem::ManuallyDrop::take(&mut self.{tag_name})",),
                            // Since this is a ManuallyDrop, our `as_` method will need
                            // to dereference the variant (e.g. `&self.Foo`)
                            "&",
                            // we need `mut self` for the argument because of ManuallyDrop
                            "mut self",
                        )
                    } else {
                        (
                            "payload",
                            format!("self.{tag_name}"),
                            // Since this is not a ManuallyDrop, our `as_` method will not
                            // want to dereference the variant (e.g. `self.Foo` with no '&')
                            "",
                            // we don't need `mut self` unless we need ManuallyDrop
                            "self",
                        )
                    };

                add_decl(
                    impls,
                    opt_impl.clone(),
                    architecture,
                    format!(
                        r#"/// Construct a tag named {tag_name}, with the appropriate payload
    pub fn {tag_name}(payload: {payload_type_name}) -> Self {{
        let mut answer = Self {{
            {tag_name}: {init_payload}
        }};

        answer.set_discriminant({discriminant_name}::{tag_name});

        answer
    }}"#
                    ),
                );

                let (ret_type_str, ret) = match payload_type {
                    RocType::RocStr
                    | RocType::Bool
                    | RocType::I8
                    | RocType::U8
                    | RocType::I16
                    | RocType::U16
                    | RocType::I32
                    | RocType::U32
                    | RocType::I64
                    | RocType::U64
                    | RocType::I128
                    | RocType::U128
                    | RocType::F32
                    | RocType::F64
                    | RocType::F128
                    | RocType::RocDec
                    | RocType::RocList(_)
                    | RocType::RocDict(_, _)
                    | RocType::RocSet(_)
                    | RocType::RocBox(_)
                    | RocType::TagUnion(_)
                    | RocType::TransparentWrapper { .. } => {
                        (payload_type_name.clone(), get_payload)
                    }
                    RocType::Struct { name, fields } => {
                        todo!();
                    }
                };

                add_decl(
                    impls,
                    opt_impl.clone(),
                    architecture,
                    format!(
                        r#"/// Unsafely assume the given {name} has a .variant() of {tag_name} and convert it to {tag_name}'s payload.
    /// (Always examine .variant() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .variant() doesn't return {tag_name}.
    pub unsafe fn into_{tag_name}({self_for_into}) -> {ret_type_str} {{
        debug_assert_eq!(self.variant(), {discriminant_name}::{tag_name});
        {ret}
    }}"#,
                    ),
                );

                add_decl(
                    impls,
                    opt_impl.clone(),
                    architecture,
                    format!(
                        r#"/// Unsafely assume the given {name} has a .variant() of {tag_name} and return its payload.
    /// (Always examine .variant() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .variant() doesn't return {tag_name}.
    pub unsafe fn as_{tag_name}(&self) -> {ref_if_needed}{payload_type_name} {{
        debug_assert_eq!(self.variant(), {discriminant_name}::{tag_name});
        {ref_if_needed}self.{tag_name}
    }}"#,
                    ),
                );
            } else {
                add_decl(
                    impls,
                    opt_impl.clone(),
                    architecture,
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
                    architecture,
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
                    architecture,
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
                    Some(payload_id) if types.get(payload_id).has_pointer(types) => {
                        format!("unsafe {{ core::mem::ManuallyDrop::drop(&mut self.{tag_name}) }},",)
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
            architecture,
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
            if self.variant() != other.variant() {
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
                    format!("self.{tag_name} == other.{tag_name},")
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

        add_decl(impls, opt_impl, architecture, buf);
    }

    // The PartialOrd impl for the tag union
    {
        let opt_impl = Some(format!("impl PartialOrd for {name}"));
        let mut buf = r#"fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
            match self.variant().partial_cmp(&other.variant()) {
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
                    format!("self.{tag_name}.partial_cmp(&other.{tag_name}),",)
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

        add_decl(impls, opt_impl, architecture, buf);
    }

    // The Ord impl for the tag union
    {
        let opt_impl = Some(format!("impl Ord for {name}"));
        let mut buf = r#"fn cmp(&self, other: &Self) -> core::cmp::Ordering {
            match self.variant().cmp(&other.variant()) {
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
                    format!("self.{tag_name}.cmp(&other.{tag_name}),",)
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

        add_decl(impls, opt_impl, architecture, buf);
    }

    // The Clone impl for the tag union
    {
        let opt_impl_prefix = if typ.has_pointer(types) {
            String::new()
        } else {
            format!("impl Copy for {name} {{}}\n\n")
        };

        let opt_impl = Some(format!("{opt_impl_prefix}impl Clone for {name}"));
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
                    format!(
                        r#"Self {{
                    {tag_name}: self.{tag_name}.clone(),
                }},"#,
                    )
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

        answer.set_discriminant(self.variant());

        answer
    }"#,
        );

        add_decl(impls, opt_impl, architecture, buf);
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
                    self.{tag_name}.hash(state);
                }},"#
                    )
                } else {
                    format!("{},", hash_tag)
                }
            },
        );

        buf.push_str(INDENT);
        buf.push('}');

        add_decl(impls, opt_impl, architecture, buf);
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
                    let deref_str = if types.get(payload_id).has_pointer(types) {
                        "&*"
                    } else {
                        "&"
                    };

                    format!(
                        r#"f.debug_tuple("{tag_name}").field({deref_str}self.{tag_name}).finish(),"#,
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

        add_decl(impls, opt_impl, architecture, buf);
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

    buf.push_str("match self.variant() {\n");

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
    architecture: Architecture,
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

    // Debug impls should never vary by architecture.
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

    add_decl(impls, None, architecture, buf);
}

fn add_struct(
    name: &str,
    architecture: Architecture,
    fields: &[Field],
    struct_id: TypeId,
    types: &Types,
    impls: &mut Impls,
) {
    match fields.len() {
        0 => {
            // An empty record is zero-sized and won't end up being passed to/from the host.
        }
        1 => {
            // Unwrap single-field records
            add_type(
                architecture,
                fields.first().unwrap().type_id(),
                types,
                impls,
            )
        }
        _ => {
            let derive = derive_str(types.get(struct_id), types, true);
            let mut buf = format!("{derive}\n#[repr(C)]\npub struct {name} {{\n");

            for field in fields {
                buf.push_str(&format!(
                    "{INDENT}pub {}: {},\n",
                    field.label(),
                    type_name(field.type_id(), types)
                ));
            }

            buf.push('}');

            add_decl(impls, None, architecture, buf);
        }
    }
}

fn type_name(id: TypeId, types: &Types) -> String {
    match types.get(id) {
        RocType::U8 => "u8".to_string(),
        RocType::U16 => "u16".to_string(),
        RocType::U32 => "u32".to_string(),
        RocType::U64 => "u64".to_string(),
        RocType::U128 => "roc_std::U128".to_string(),
        RocType::I8 => "i8".to_string(),
        RocType::I16 => "i16".to_string(),
        RocType::I32 => "i32".to_string(),
        RocType::I64 => "i64".to_string(),
        RocType::I128 => "roc_std::I128".to_string(),
        RocType::F32 => "f32".to_string(),
        RocType::F64 => "f64".to_string(),
        RocType::F128 => "roc_std::F128".to_string(),
        RocType::Bool => "bool".to_string(),
        RocType::RocDec => "roc_std::RocDec".to_string(),
        RocType::RocStr => "roc_std::RocStr".to_string(),
        RocType::RocDict(key_id, val_id) => format!(
            "roc_std::RocDict<{}, {}>",
            type_name(*key_id, types),
            type_name(*val_id, types)
        ),
        RocType::RocSet(elem_id) => format!("roc_std::RocSet<{}>", type_name(*elem_id, types)),
        RocType::RocList(elem_id) => format!("roc_std::RocList<{}>", type_name(*elem_id, types)),
        RocType::RocBox(elem_id) => format!("roc_std::RocBox<{}>", type_name(*elem_id, types)),
        RocType::Struct { name, .. }
        | RocType::TransparentWrapper { name, .. }
        | RocType::TagUnion(RocTagUnion::NonRecursive { name, .. })
        | RocType::TagUnion(RocTagUnion::Recursive { name, .. })
        | RocType::TagUnion(RocTagUnion::Enumeration { name, .. })
        | RocType::TagUnion(RocTagUnion::NullableWrapped { name, .. })
        | RocType::TagUnion(RocTagUnion::NullableUnwrapped { name, .. })
        | RocType::TagUnion(RocTagUnion::NonNullableUnwrapped { name, .. }) => name.clone(),
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
    architecture: Architecture,
    id: TypeId,
    null_tag: &str,
    non_null_tag: &str,
    non_null_payload: TypeId,
    null_represents_first_tag: bool,
    types: &Types,
    impls: &mut Impls,
) {
    let mut tag_names = vec![null_tag.to_string(), non_null_tag.to_string()];

    tag_names.sort();

    let discriminant_name = add_discriminant(name, architecture, tag_names, types, impls);
    let payload_type = types.get(non_null_payload);
    let payload_type_name = type_name(non_null_payload, types);
    let has_pointer = payload_type.has_pointer(types);
    let (wrapped_payload_type_name, init_payload, ref_if_needed) = if has_pointer {
        (
            format!("core::mem::ManuallyDrop<{payload_type_name}>"),
            "core::mem::ManuallyDrop::new(payload)",
            "&",
        )
    } else {
        (payload_type_name.clone(), "payload", "")
    };

    // The opaque struct for the tag union
    {
        // These need their own Clone impl because they have
        // a refcount to bump
        let derive_extras = if types.get(id).has_float(types) {
            ""
        } else {
            ", Eq, Ord, Hash"
        };
        let body = format!(
            r#"#[repr(C)]
#[derive(PartialEq, PartialOrd{derive_extras})]
pub struct {name} {{
    pointer: *mut {wrapped_payload_type_name},
}}"#
        );

        add_decl(impls, None, architecture, body);
    }

    // The impl for the tag union
    {
        let opt_impl = Some(format!("impl {name}"));

        add_decl(
            impls,
            opt_impl.clone(),
            architecture,
            format!(
                r#"#[inline(always)]
    fn storage(&self) -> Option<&core::cell::Cell<roc_std::Storage>> {{
        if self.pointer.is_null() {{
            None
        }} else {{
            unsafe {{
                Some(&*self.pointer.cast::<core::cell::Cell<roc_std::Storage>>().sub(1))
            }}
        }}
    }}"#
            ),
        );

        add_decl(
            impls,
            opt_impl.clone(),
            architecture,
            format!(
                r#"{VARIANT_DOC_COMMENT}
    pub fn variant(&self) -> {discriminant_name} {{
        if self.pointer.is_null() {{
            {discriminant_name}::{null_tag}
        }} else {{
            {discriminant_name}::{non_null_tag}
        }}
    }}"#
            ),
        );

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
            architecture,
            format!(
                r#"/// Construct a tag named {non_null_tag}, with the appropriate payload
    pub fn {non_null_tag}(payload: {payload_type_name}) -> Self {{
        let payload_align = core::mem::align_of::<{payload_type_name}>();
        let self_align = core::mem::align_of::<Self>();
        let size = self_align + core::mem::size_of::<{payload_type_name}>();

        unsafe {{
            // Store the payload at `self_align` bytes after the allocation,
            // to leave room for the refcount.
            let alloc_ptr = crate::roc_alloc(size, payload_align as u32);
            let payload_ptr = alloc_ptr.cast::<u8>().add(self_align).cast::<{wrapped_payload_type_name}>();

            *payload_ptr = {init_payload};

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
                architecture,
                format!(
                    r#"/// Unsafely assume the given {name} has a .variant() of {non_null_tag} and convert it to {non_null_tag}'s payload.
    /// (Always examine .variant() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .variant() doesn't return {non_null_tag}.
    pub unsafe fn into_{non_null_tag}(self) -> {payload_type_name} {{
        debug_assert_eq!(self.variant(), {discriminant_name}::{non_null_tag});

        let payload = {assign_payload};

        core::mem::drop(self);

        payload
    }}"#,
                ),
            );
        }

        add_decl(
            impls,
            opt_impl.clone(),
            architecture,
            format!(
                r#"/// Unsafely assume the given {name} has a .variant() of {non_null_tag} and return its payload.
    /// (Always examine .variant() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .variant() doesn't return {non_null_tag}.
    pub unsafe fn as_{non_null_tag}(&self) -> {ref_if_needed}{payload_type_name} {{
        debug_assert_eq!(self.variant(), {discriminant_name}::{non_null_tag});
        {ref_if_needed}*self.pointer
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
            architecture,
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
            architecture,
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
            architecture,
            format!(
                r#"/// Other `as` methods return a payload, but since the {null_tag} tag
    /// has no payload, this does nothing and is only here for completeness.
    pub unsafe fn as_{null_tag}(&self) {{
        ()
    }}"#,
            ),
        );
    }

    // The roc_std::ReferenceCount impl for the tag union
    {
        let opt_impl = Some(format!("unsafe impl roc_std::ReferenceCount for {name}"));

        add_decl(
            impls,
            opt_impl.clone(),
            architecture,
            format!(
                r#"fn increment(&self) {{
        if let Some(storage) = self.storage() {{
            let mut copy = storage.get();
            if !copy.is_readonly() {{
                copy.increment_reference_count();
                storage.set(copy);
            }}
        }}
    }}"#
            ),
        );

        add_decl(
            impls,
            opt_impl,
            architecture,
            format!(
                r#"unsafe fn decrement(wrapper_ptr: *const Self) {{
        let wrapper = &*wrapper_ptr;

        if let Some(storage) = Self::storage(wrapper) {{
            // Decrement the refcount and return early if no dealloc is needed
            {{
                let mut copy = storage.get();
                let needs_dealloc = copy.decrease();

                if !needs_dealloc {{
                    if !copy.is_readonly() {{
                        // Write the storage back.
                        storage.set(copy);
                    }}

                    return;
                }}
            }}

            // Dealloc the memory
            let alignment = core::mem::align_of::<Self>().max(core::mem::align_of::<roc_std::Storage>());
            let alloc_ptr = wrapper.pointer.cast::<u8>().sub(alignment);

            crate::roc_dealloc(
                alloc_ptr as *mut core::ffi::c_void,
                alignment as u32,
            );
        }}
    }}"#
            ),
        );
    }

    // The Clone impl for the tag union
    {
        // Note that these never have Copy because they always contain a pointer.
        let opt_impl = Some(format!("impl Clone for {name}"));

        // Recursive tag unions need a custom Clone which bumps refcount.
        let body = r#"fn clone(&self) -> Self {
        roc_std::ReferenceCount::increment(self);

        Self {
            pointer: self.pointer
        }
    }
"#
        .to_string();

        add_decl(impls, opt_impl, architecture, body);
    }

    // The Drop impl for the tag union
    {
        let opt_impl = Some(format!("impl Drop for {name}"));

        add_decl(
            impls,
            opt_impl,
            architecture,
            format!(
                r#"fn drop(&mut self) {{
        if !self.pointer.is_null() {{
            unsafe {{
                // Drop the payload before dropping its wrapper.
                core::mem::drop(core::mem::ManuallyDrop::take(&mut *self.pointer));

                roc_std::ReferenceCount::decrement(self as *const Self);
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

        let body = format!(
            r#"fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
        if self.pointer.is_null() {{
            f.write_str("{name}::{null_tag}")
        }} else {{
            f.write_str("{name}::")?;

            unsafe {{ f.debug_tuple("{non_null_tag}").field(&*{extra_deref}self.pointer).finish() }}
        }}
    }}"#
        );

        add_decl(impls, opt_impl, architecture, body);
    }
}

fn arch_to_str(architecture: &Architecture) -> &'static str {
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
