use const_format::formatcp;
use core::{fmt, iter};
use roc_collections::{VecMap, VecSet};
use roc_module::symbol::{IdentId, IdentIds, Interns, ModuleId, Symbol};
use roc_types::types::{AliasKind, Type};

pub struct ModuleDocs<'a, I: Iterator<Item = SidebarEntry<'a>> + Clone> {
    pub module_name: &'a str,
    pub home: ModuleId,
    pub home_ident_ids: &'a IdentIds,
    pub interns: &'a Interns,
    pub package_sidebar_entries: I,
    pub base_urls: VecMap<ModuleId, &'a str>,
}

pub struct RecordField<'a> {
    field_name: &'a str,
    value_type: &'a Type,
    is_required: bool,
}

#[derive(Clone, Copy, Debug, Default)]
struct Indentation {
    level: u32,
}

#[derive(Clone, Copy)]
enum WrapInParens {
    Unnecessary,
    NeededIfWhitespace,
}

impl Indentation {
    const INDENT_STR: &str = "    ";

    pub fn increment(self) -> Self {
        Self {
            level: self.level.saturating_add(1),
        }
    }

    pub fn decrement(self) -> Self {
        Self {
            level: self.level.saturating_sub(1),
        }
    }
}

impl fmt::Display for Indentation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Always start with a newline before indenting
        write!(f, "\n")?;

        for _ in 0..self.level {
            f.write_str(Self::INDENT_STR)?;
        }

        Ok(())
    }
}

/// A named heading in the sidebar, with some number of
/// entries beneath it.
pub struct SidebarEntry<'a> {
    /// In the source code, this will appear in a module's `exposes` list like:
    ///
    /// [
    ///     Foo,
    ///     Bar,
    ///     ## Heading
    ///     Baz,
    ///     Blah,
    /// ]
    pub link_text: &'a str,

    /// These doc comments get interpreted as flat strings; Markdown is not allowed
    /// in them, because they will be rendered in the sidebar as plain text.
    pub doc_comment: Option<&'a str>,
}

impl<'a, I: Iterator<Item = SidebarEntry<'a>> + Clone> ModuleDocs<'a, I> {
    pub fn render_decl(
        &mut self,
        ident: &str,
        typ: &'a Type,
        buf: &mut impl fmt::Write,
    ) -> fmt::Result {
        let module_name = self.module_name;

        write!(
            buf,
            "<h3 id='{ident}' class='entry-name'><a href='{module_name}#{ident}'>{LINK_ICON_SVG}</a><b>{ident}</b> "
        )?;

        match typ {
            Type::Alias {
                kind: AliasKind::Opaque,
                ..
            } => {
                buf.write_str(":= ")?;
                self.render_type(buf, Indentation::default(), typ, WrapInParens::Unnecessary)
            }
            // If this decl is just type alais to a type from another module (commonly `Foo : InternalFoo`),
            // then render the actual type rather than linking to the other module's alias.
            //
            // We could make the rule be "only do this if the linked type is not exposed" but that's more
            // complicated than the simpler rule of "if it's an alias of another type, always render the
            // final type it aliases to," and we can always adjust later if there turns out to be some
            // use case where that's not the behavior we want.
            Type::Alias {
                kind: AliasKind::Structural,
                symbol,
                actual,
                ..
            } if symbol.module_id() != self.home => {
                buf.write_str(": ")?;
                self.render_type(
                    buf,
                    Indentation::default(),
                    actual,
                    WrapInParens::Unnecessary,
                )
            }
            typ => {
                buf.write_str(": ")?;
                self.render_type(buf, Indentation::default(), typ, WrapInParens::Unnecessary)
            }
        }
    }

    #[allow(dead_code)]
    pub fn render_sidebar(&self, buf: &mut impl fmt::Write) -> fmt::Result {
        for SidebarEntry {
            link_text: module_name,
            doc_comment,
        } in self.package_sidebar_entries.clone()
        {
            if let Some(heading) = doc_comment {
                write!(buf, "\t<h3 class=\"sidebar-heading\">{heading}</a>\n")?;
            }

            // Sidebar entries should all be relative URLs and unqualified names
            buf.write_str("<a href=\"")?;
            self.render_relative_url(module_name, buf)?;
            write!(buf, "\">{}</a>", module_name)?;
        }

        Ok(())
    }

    fn render_relative_url(&self, text: &str, buf: &mut impl fmt::Write) -> fmt::Result {
        let module_name = self.module_name;

        // e.g. "Str#isEmpty"
        write!(buf, "{module_name}#{text}",)
    }

    fn render_type(
        &self,
        buf: &mut impl fmt::Write,
        indent: Indentation,
        typ: &'a Type,
        // Whether the type needs to be wrapped in parens (only matters if the rendered type contains spaces,
        // e.g. function application or tags with payloads)
        _wrap_in_parens: WrapInParens,
    ) -> fmt::Result {
        use Type::*;

        let todo = (); // TODO use wrap_in_parens

        match typ {
            EmptyRec => self.render_record_type(buf, indent, iter::empty()),
            EmptyTagUnion => self.render_tag_union_type(buf, indent, iter::empty()),
            Function(args, _closure_size, ret) => {
                if is_multiline(typ) {
                    buf.write_str("(\n")?;
                    self.render_function_type(buf, indent.increment(), args.iter(), &*ret)?;
                    write!(buf, "{indent}(\n")
                } else {
                    buf.write_char('(')?;
                    self.render_function_type(buf, indent, args.iter(), &*ret)?;
                    buf.write_char(')')
                }
            }
            Record(fields, _ext) => self.render_record_type(
                buf,
                indent,
                fields.iter().map(|(field_name, field)| {
                    use roc_types::types::RecordField::*;

                    match field {
                        Required(typ) | RigidRequired(typ) | Demanded(typ) => RecordField {
                            field_name: field_name.as_str(),
                            value_type: typ,
                            is_required: true,
                        },
                        Optional(typ) | RigidOptional(typ) => RecordField {
                            field_name: field_name.as_str(),
                            value_type: typ,
                            is_required: false,
                        },
                    }
                }),
            ),
            Tuple(_, _) => todo!(),
            TagUnion(tags, _ext) => self.render_tag_union_type(
                buf,
                indent,
                tags.iter()
                    .map(|(tag_name, payloads)| (tag_name.0.as_str(), payloads.as_slice())),
            ),
            FunctionOrTagUnion(_, _, _) => todo!(),
            ClosureTag {
                name: _,
                captures: _,
                ambient_function: _,
            } => todo!(),
            UnspecializedLambdaSet { unspecialized: _ } => todo!(),
            DelayedAlias(_) => todo!(),
            Alias {
                symbol: _,
                type_arguments: _,
                lambda_set_variables: _,
                infer_ext_in_output_types: _,
                actual: _,
                kind: _,
            } => todo!(),
            RecursiveTagUnion(_, _, _) => todo!(),
            Apply(_, _, _) => todo!(),
            Variable(_) => todo!(),
            RangedNumber(_) => todo!(),
            Error => todo!(),
        }
    }

    fn render_record_type(
        &self,
        buf: &mut impl fmt::Write,
        indent: Indentation,
        mut fields: impl ExactSizeIterator<Item = RecordField<'a>>,
    ) -> fmt::Result {
        const BRACES_CLASS_NAME: &str = "literal";
        const OPEN_BRACE_HTML: &str = formatcp!("<span class='{BRACES_CLASS_NAME}'>{{</span>");
        const CLOSE_BRACE_HTML: &str = formatcp!("<span class='{BRACES_CLASS_NAME}'>}}</span>");

        match fields.next() {
            None => {
                // Empty records are just "{}"
                write!(buf, "<span class='{BRACES_CLASS_NAME}'>{{}}</span>")
            }
            Some(RecordField {
                field_name,
                value_type,
                is_required,
            }) if fields.len() == 1 && !is_multiline(value_type) => {
                let colon_or_question_mark = if is_required { ":" } else { "?" };

                // If the record has one field, and that field's value is single-line,
                // then we print the record on one line with spaces inside the braces
                write!(
                    buf,
                    "{OPEN_BRACE_HTML} {field_name} {colon_or_question_mark} "
                )?;
                self.render_type(buf, indent, value_type, WrapInParens::Unnecessary)?;
                write!(buf, " {CLOSE_BRACE_HTML}")
            }
            Some(first) => {
                // Multi-field records are on multiple lines, with each line indented and ending in a trailing comma
                write!(buf, "{indent}{OPEN_BRACE_HTML}")?;

                {
                    // Indent one extra level while we're inside the braces.
                    let indent = indent.increment();

                    for RecordField {
                        field_name,
                        value_type,
                        is_required,
                    } in iter::once(first).chain(fields)
                    {
                        let colon_or_question_mark = if is_required { ":" } else { "?" };

                        write!(buf, "{indent}{field_name} {colon_or_question_mark} ")?;

                        if is_multiline(value_type) {
                            buf.write_char('\n')?;
                        } else {
                            buf.write_char(' ')?;
                        }

                        self.render_type(buf, indent, value_type, WrapInParens::Unnecessary)?;

                        // Put a trailing comma at the end of each line.
                        buf.write_char(',')?;
                    }
                }

                // The closing brace goes on its own line, indented.
                write!(buf, "{indent}{CLOSE_BRACE_HTML}")
            }
        }
    }

    fn render_tag_union_type(
        &self,
        buf: &mut impl fmt::Write,
        indent: Indentation,
        mut tags: impl ExactSizeIterator<Item = (&'a str, &'a [Type])>,
    ) -> fmt::Result {
        const BRACES_CLASS_NAME: &str = "literal";
        const TAG_CLASS_NAME: &str = "literal";
        const OPEN_BRACE_HTML: &str = formatcp!("<span class='{BRACES_CLASS_NAME}'>[</span>");
        const CLOSE_BRACE_HTML: &str = formatcp!("<span class='{BRACES_CLASS_NAME}'>]</span>");

        match tags.next() {
            None => {
                // Empty tag unions are just "[]"
                write!(buf, "<span class='{BRACES_CLASS_NAME}'>[]</span>")
            }
            Some((tag, payloads)) if tags.len() == 1 && !payloads.iter().any(is_multiline) => {
                // Single-line tag unions don't have spaces inside the braces
                write!(
                    buf,
                    "{OPEN_BRACE_HTML}<span class='{TAG_CLASS_NAME}'>{tag}</span>"
                )?;

                for typ in payloads.iter() {
                    buf.write_char(' ')?;
                    self.render_type(buf, indent, typ, WrapInParens::NeededIfWhitespace)?;
                }

                buf.write_str(CLOSE_BRACE_HTML)
            }
            Some(first) => {
                // Multi-tag unions are on multiple lines, with each line indented and ending in a trailing comma
                write!(buf, "{indent}{OPEN_BRACE_HTML}")?;

                {
                    // Indent one extra level while we're inside the braces.
                    let indent = indent.increment();

                    for (tag, payloads) in iter::once(first).chain(tags) {
                        write!(buf, "{indent}<span class='{TAG_CLASS_NAME}'>{tag}</span>")?;

                        for typ in payloads.iter() {
                            buf.write_char(' ')?;
                            self.render_type(buf, indent, typ, WrapInParens::NeededIfWhitespace)?;
                        }

                        // Put a trailing comma at the end of each line.
                        buf.write_char(',')?;
                    }
                }

                // The closing brace goes on its own line, indented.
                write!(buf, "{indent}{CLOSE_BRACE_HTML}")
            }
        }
    }

    fn render_function_type(
        &self,
        buf: &mut impl fmt::Write,
        indent: Indentation,
        mut args: impl ExactSizeIterator<Item = &'a Type>,
        ret: &'a Type,
    ) -> fmt::Result {
        let args_len = args.len();

        // Render args as multiline if the function has more than 3 args, or if any args are multiline
        if args_len > 3 || args.any(is_multiline) {
            let indent = indent.increment();

            for (index, arg) in args.enumerate() {
                write!(buf, "\n{indent}")?;

                self.render_type(buf, indent, arg, WrapInParens::Unnecessary)?;

                if index < args_len - 1 {
                    // Put a comma at the end of each line except the last one,
                    // because the -> is next.
                    buf.write_char(',')?;
                }
            }

            write!(buf, "\n{indent}->")?;
        } else {
            for (index, arg) in args.enumerate() {
                self.render_type(buf, indent, arg, WrapInParens::Unnecessary)?;

                if index < args_len - 1 {
                    // Put a comma at the end of each line except the last one,
                    // because the -> is next.
                    buf.write_str(", ")?;
                }
            }

            buf.write_str(" ->")?;
        }

        let indent = if is_multiline(ret) {
            write!(buf, "\n{indent}")?;

            indent.increment()
        } else {
            buf.write_char(' ')?;

            indent
        };

        self.render_type(buf, indent, ret, WrapInParens::Unnecessary)
    }

    fn render_absolute_url(
        &self,
        ident_id: IdentId,
        module_id: ModuleId,
        buf: &mut impl fmt::Write,
    ) -> fmt::Result {
        match self.base_urls.get(&module_id) {
            Some(base_url) => {
                write!(
                    buf,
                    // e.g. "https://example.com/Str#isEmpty"
                    "{base_url}{}#{}",
                    self.interns.module_name(module_id),
                    self.unqualified_name(ident_id)
                )
            }
            None => {
                if cfg!(debug_assertions) {
                    unreachable!("docs generation tried to get a base URL for ModuleId {:?} but it was not found in base_urls. This should never happen!", module_id);
                }

                // In release builds, don't panic, just gracefully continue.
                // It'll be a bug, but at least
                // it won't block the user from seeing *some* docs rendered.
                Ok(())
            }
        }
    }

    fn unqualified_name(&self, ident_id: IdentId) -> &str {
        self.name_from_ident_id(ident_id, &self.home_ident_ids)
    }

    fn name_from_ident_id(&self, ident_id: IdentId, ident_ids: &'a IdentIds) -> &'a str {
        ident_ids.get_name(ident_id).unwrap_or_else(|| {
            if cfg!(debug_assertions) {
                unreachable!("docs generation tried to render a relative URL for IdentId {:?} but it was not found in home_identids, which should never happen!", ident_id);
            }

            // In release builds, don't panic, just gracefully continue
            // by not writing the url. It'll be a bug, but at least
            // it won't block the user from seeing *some* docs rendered.
            ""
        })
    }

    // TODO render version as well
    pub fn render_package_name_link(
        &self,
        name: &str,
        base_url: &str,
        buf: &mut impl fmt::Write,
    ) -> fmt::Result {
        write!(
            buf,
            "<h1 class='pkg-full-name'><a href='{base_url}'>{name}</a></h1>"
        )
    }

    /// The list items containing module links
    pub fn render_package_index(
        &self,
        entries: impl Iterator<Item = SidebarEntry<'a>>,
        buf: &mut impl fmt::Write,
    ) -> fmt::Result {
        // The HTML for the index page
        write!(buf, "<h2 class='module-name'>Modules</h2>\n")?;

        for SidebarEntry {
            link_text: module_name,
            doc_comment,
        } in entries
        {
            if let Some(heading) = doc_comment {
                write!(buf, "\t<h3 class=\"sidebar-heading\">{heading}</a>\n")?;
            }

            // The anchor tag containing the module link
            write!(
                buf,
                "<a class='index-module-link' href='{module_name}'>{module_name}</a>\n"
            )?;
        }

        Ok(())
    }

    pub fn render_module(
        &self,
        _all_exposed_symbols: &VecSet<Symbol>,
        buf: &mut impl fmt::Write,
    ) -> fmt::Result {
        todo!()
    }
}

fn is_multiline(_first: &Type) -> bool {
    let todo = todo!();
}

const LINK_ICON_SVG: &str = include_str!("static/link.svg");
