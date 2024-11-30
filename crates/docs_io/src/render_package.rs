use bumpalo::collections::string::String;
use core::fmt::{Debug, Write};

pub struct RecordField<'a, Type> {
    pub field_name: &'a str,
    pub value_type: Type,
    pub is_required: bool,
}

#[derive(Debug)]
pub struct BodyEntry<'a, Type> {
    pub entry_name: &'a str,
    pub type_vars_names: &'a [&'a str],
    pub type_annotation: Type,
    pub docs: Option<&'a str>,
}

#[derive(Debug)]
pub struct AbilityMember<'a, Type> {
    pub entry_name: &'a str,
    pub type_annotation: Type,
    pub docs: Option<&'a str>,
}

// ability: |ability, buf| self.render_ability_decl(arena, ability, buf),
// type_alias: |type_var_names: StrIter, alias, buf| {
//     self.render_type_alias_decl(arena, type_var_names, alias, buf)
// },
// opaque_type: |type_var_names: StrIter, abilities: AbilityIter, buf| {
//     self.render_opaque_type_decl(arena, type_var_names, abilities, buf)
// },
// value: |val, buf| self.render_val_decl(arena, val, buf),

// pub struct TypeAnnVisitor<
//     'a,
//     VisitAbility: Fn(AbilityMemberIter, &'a mut String<'a>),
//     VisitAlias: Fn(AliasVarNames, &'a Self, &'a mut String<'a>),
//     VisitOpaque: Fn(OpaqueVarNames, AbilityIter, &'a mut String<'a>),
//     VisitValue: Fn(&'a Self, &'a mut String<'a>),
// > {
//     pub ability: VisitAbility,
//     pub type_alias: VisitAlias,
//     pub opaque_type: VisitOpaque,
//     pub value: VisitValue,
// }

pub fn render_package_name_link(name: &str, buf: &mut String<'_>) {
    let _ = write!(buf, "<h1 class='pkg-full-name'><a href='/'>{name}</a></h1>");
}

// fn is_multiline(_first: &Type) -> bool {
//     let todo = ();

//     true
// }

fn advance_past<'a>(needle: &'static str, src: &'a str, buf: &mut String<'_>) -> &'a str {
    if let Some(start_index) = src.find(needle) {
        // Copy over everything up to this point.
        buf.push_str(&src[..start_index]);

        // Advance past the end of this string.
        &src[(start_index + needle.len())..]
    } else {
        unreachable!( // TODO replace this with a panic in debug builds and a much more concise crash in release
            "Compiler bug in docs generation code: could not find doc template section {:?} in the template - this should never happen!\n\nNOTE: advance_past must be called on each template section in the order they appear in the template! This improves performance, but means that working on sections out of order can lead to this error.\n\nAt this point, the remaining template was:\n\n{src}",
            needle
        );
    }
}

fn write_base_url(user_specified_base_url: Option<impl AsRef<str>>, buf: &mut String) {
    // e.g. "builtins/" in "https://roc-lang.org/builtins/Str"
    match user_specified_base_url {
        Some(root_builtins_path) => {
            let root_builtins_path = root_builtins_path.as_ref();

            if !root_builtins_path.starts_with('/') {
                buf.push('/');
            }

            buf.push_str(&root_builtins_path);

            if !root_builtins_path.ends_with('/') {
                buf.push('/');
            }
        }
        None => {
            buf.push('/');
        }
    }
}

// fn render_type_inner<
//     'a,
//     ModuleId: PartialEq + Debug + 'a,
//     IdentId: PartialEq + Debug,
//     ModuleNames: Iterator<Item = &'a (ModuleId, &'a str)>,
//     Sidebar: Iterator<Item = SidebarEntry<'a, StrIter>>,
//     StrIter: Iterator<Item = &'a str>,
//     Type: Debug,
//     Alias,
//     BodyEntries: Iterator<Item = BodyEntry<'a, Type, IdentId>>,
//     TypeVisitor: roc_docs_types::TypeVisitor<&'a str, StrIter, VisitType, VisitAbleVars>,
//     VisitType: Fn(&mut TypeVisitor),
//     VisitAbleVars: Iterator<Item = (&'a str, TypesIter)>,
//     TypesIter: Iterator<Item = Type>,
// >(
//     capture: &impl Docs<
//         'a,
//         ModuleId,
//         IdentId,
//         ModuleNames,
//         Sidebar,
//         StrIter,
//         Type,
//         Alias,
//         BodyEntries,
//         TypeVisitor,
//         VisitType,
//         VisitAbleVars,
//         TypesIter,
//     >,
//     typ: Type,
// ) {
//     capture.visit_type(typ)
// }

///////////////////////////////////////////////////////

//     if matches!(type_ann, Type::Ability { .. }) {
//         // Ability declarations don't have ":" after the name, just `implements`
//         // buf.push_str(" <span class='kw'>implements</span>");
//         // let todo = (); // TODO render ability declaration here
//         todo!();
//     } else if let Some(mut alias) = self.opt_alias(module_id, ident_id) {
//         // This is a type entry (either a type alias or an opaque type)

//         // Print all the variables in the type right after the name,
//         // separated by spaces - e.g. the `ok` and `err` in:
//         //
//         //     Result ok err :
//         for loc_alias_var in alias.type_variables.iter() {
//             let _ = write!(buf, " {}", loc_alias_var.value.name.as_str());
//         }

//         // Resolve as many aliases as necessary
//         loop {
//             match alias.kind {
//                 AliasKind::Structural => {
//                     // If this is an alias of another alias, inline the other alias so you can
//                     // see what the actual underlying type is.
//                     //
//                     // DESIGN NOTE: in the future, we might want to do this only when
//                     // this alias resolves to another alias which is in an unexposed module,
//                     // e.g. the alias Http.Request is exposed, but it's an alias to
//                     // InternalHttp.Request, which is not exposed because the InternalHttp
//                     // module is not exposed. (In that case, it's very important that we
//                     // inline the annotation because otherwise you just don't see anything,
//                     // and you can't tell what the alias is aliasing without inducing a type
//                     // mismatch, reading the source code, asking editor tooling to infer it, etc.)
//                     if let Type::DelayedAlias(alias_common) = &alias.typ {
//                         if let Some(new_alias) = self.opt_alias(
//                             alias_common.symbol.module_id(),
//                             alias_common.symbol.ident_id(),
//                         ) {
//                             alias = new_alias;
//                             continue;
//                         }
//                     }

//                     match type_ann {
//                         TypeAnnotation::NoTypeAnn => {
//                             let todo = (); // TODO if this turns out to be an alias of an internal opaque type (after expansion), do the AliasKind::Opaque logic instead (including not printing ":")

//                             dbg!("alias", name, &alias.typ);
//                         }
//                         _ann => {
//                             dbg!("body", name, &_ann);
//                             let todo = (); // TODO this is the body; actually render each of the other types into HTML here!
//                         }
//                     }
//                     buf.push_str(" <span class='kw'>:</span>");

//                     let todo = (); // TODO render the alias body, including the logic for expanding InternalPath etc.
//                 }
//                 AliasKind::Opaque => {
//                     // We print `:` for type aliases, but print nothing for opaque types
//                     // because we don't expose the internal structure of opaque types.

//                     let todo = (); // TODO print `implements` for this opaque type, if it implements any abilities
//                 }
//             }

//             // By default, break here. We only `continue` earlier on if we need to continue resolving an alias.
//             break;
//         }

//         // If we have any ability restrictions on the type alias variables, print them at the end.
//         let num_bound_vars = alias.type_variables.iter().fold(0, |count, loc_alias_var| {
//             count + loc_alias_var.value.opt_bound_abilities.is_some() as usize
//         });

//         if num_bound_vars > 0 {
//             let _ = write!(buf, "{indent}<span class='kw'>where</span>");

//             // if there are multiple variables, print each variable on its own line.
//             let is_multiline = num_bound_vars > 1;

//             let indent = if is_multiline {
//                 indent.increment()
//             } else {
//                 indent
//             };

//             for loc_alias_var in alias.type_variables.iter() {
//                 if let Some(ability_set) = &loc_alias_var.value.opt_bound_abilities {
//                     let type_var = loc_alias_var.value.name.as_str();

//                     if is_multiline {
//                         let _ = write!(buf, "{indent}");
//                     } else {
//                         buf.push_str(" ");
//                     };

//                     let _ = write!(
//                         buf,
//                         "<span class='type-var'>{type_var}</span> <span class='kw'>implements</span> "
//                     );

//                     for (index, symbol) in ability_set.sorted_iter().enumerate() {
//                         if index > 0 {
//                             buf.push_str("&amp; ");
//                         }

//                         let ident_id = symbol.ident_id();
//                         let module_id = symbol.module_id();
//                         let (ability_name, todo) = ("<todo>", ()); // TODO get IdentIds for this module_id and use that to print the ident_id

//                         let todo = (); // TODO make this <a> link to the Ability's docs
//                         let _ = write!(
//                             buf,
//                             "<a class='ability' href='#todo'>{ability_name}</a>",
//                         );
//                     }

//                     // Put trailing commas at the end of each `implements` line
//                     if is_multiline {
//                         buf.push_str(",");
//                     }
//                 }
//             }
//         }
//     } else if let Some(ann_result) = self.opt_type(module_id, ident_id) {
//         buf.push_str(" <span class='kw'>:</span>");

//         // This is a value entry (either a function or a non-function constant)
//         match ann_result {
//             Ok(ann) => {
//                 // dbg!("decl ann", &ann.signature);
//             }
//             Err(var) => {
//                 // dbg!("decl var", var);
//             }
//         }
//     } else {
//         // We should always have a variable, but if we don't, then in release builds
//         // we gracefully recover by not rendering a type. In debug builds, we panic.
//         #[cfg(debug_assertions)]
//         {
//             unreachable!("Tried to render docs for IdentId ({:?}) in module {module_name} which had no corresponding Variable. This should never happen!", ident_id);
//         }
//     }

//     buf.push_str("</h3>");

//     if let Some(doc_str) = entry.docs {
//         let todo = (); // TODO render markdown
//         buf.push_str(doc_str);
//     }

//     buf.push_str("</section>");
// }

// // for (
// //     var,
// // ) in exposed
// // {
// //     let _ = write!(
// //         buf,
// //         "<section><h3 id='{name}' class='entry-name'><a href='{module_name}#{name}'>{name}</a> :"
// //     );

// //     // match typ {
// //     //     Type::Alias {
// //     //         kind: AliasKind::Opaque,
// //     //         ..
// //     //     } => {
// //     //         buf.push_str(":= ");
// //     //         self.render_type(buf, Indentation::default(), typ, WrapInParens::Unnecessary)
// //     //     }
// //     //     // If this decl is just type alais to a type from another module (commonly `Foo : InternalFoo`),
// //     //     // then render the actual type rather than linking to the other module's alias.
// //     //     //
// //     //     // We could make the rule be "only do this if the linked type is not exposed" but that's more
// //     //     // complicated than the simpler rule of "if it's an alias of another type, always render the
// //     //     // final type it aliases to," and we can always adjust later if there turns out to be some
// //     //     // use case where that's not the behavior we want.
// //     //     Type::Alias {
// //     //         kind: AliasKind::Structural,
// //     //         symbol,
// //     //         actual,
// //     //         ..
// //     //     } if symbol.module_id() != self.home => {
// //     //         buf.push_str(": ");
// //     //         self.render_type(
// //     //             buf,
// //     //             Indentation::default(),
// //     //             actual,
// //     //             WrapInParens::Unnecessary,
// //     //         )
// //     //     }
// //     //     typ => {
// //     //         buf.push_str(": ");
// //     //         self.render_type(buf, Indentation::default(), typ, WrapInParens::Unnecessary)
// //     //     }
// //     // }

// //     buf.push_str("</section>");
// // }
