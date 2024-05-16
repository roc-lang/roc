use const_format::formatcp;
use core::{fmt, iter};
use roc_collections::VecMap;
use roc_module::symbol::{IdentId, IdentIds, Interns, ModuleId};
use roc_types::types::{AliasKind, Type};

pub struct ModuleDocs<'a> {
    module_name: &'a str,
    home: ModuleId,
    home_ident_ids: IdentIds,
    interns: Interns,
    headings: &'a [Heading<'a>],
    entries: &'a [IdentId],
    base_urls: VecMap<ModuleId, &'a str>,
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
pub struct Heading<'a> {
    /// In the source code, this will appear in a module's `exposes` list like:
    ///
    /// [
    ///     Foo,
    ///     Bar,
    ///     ## Heading
    ///     Baz,
    ///     Blah,
    /// ]
    ///
    /// It gets interpreted as a flat string; Markdown is not allowed in these headers,
    /// because they will be rendered in the sidebar as text only.
    text: &'a str,

    /// The number of entries that get rendered before this header.
    /// It's expressed this way because there can be entries before the first
    /// header, so this tells us how many such entries to render before
    /// rendering the first header.
    entries_before: u32,
}

impl<'a> ModuleDocs<'a> {
    pub fn render_decl(
        &mut self,
        ident: &str,
        typ: &Type,
        buf: &mut impl fmt::Write,
    ) -> fmt::Result {
        let module_name = self.module_name;

        // TODO next: wire up file loading to this, so I can verify that if I have InternalHttp it actually gets expanded!
        //            I'm gonna need all that eventually, so it's best to do it right now before investing more in
        //            this canonicalized approach. It's critical that I verify that the original problem I'm trying
        //            to solve is actually going to be solved! Also, remember to do solving too, so we can show
        //            inferred type annotations for un-annotated things. Just add a `roc docs --infer` flag for now
        //            which does this. Later, can switch over to just always use --infer
        let todo = todo!();

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
    pub fn render_sidebar(&mut self, buf: &mut impl fmt::Write) -> fmt::Result {
        let mut entries = self.entries.iter().copied();

        for heading in self.headings {
            for _ in 0..heading.entries_before {
                match entries.next() {
                    Some(entry) => {
                        self.render_sidebar_entry(buf, entry)?;
                    }
                    None => {
                        if cfg!(debug_assertions) {
                            unreachable!("docs generation tried to render an entry inside a heading, but there were no more entries! This means that entries_before was inaccurately high in that Heading.");
                        }

                        // In release builds, don't panic, just gracefully continue
                        // by not rendering the headings. It'll be a bug, but at least
                        // it won't block the user from seeing *some* docs rendered.
                    }
                }
            }

            self.render_sidebar_heading(buf, heading.text)?;
        }

        while let Some(entry) = entries.next() {
            self.render_sidebar_entry(buf, entry)?;
        }

        Ok(())
    }

    fn render_sidebar_heading(
        &mut self,
        buf: &mut impl fmt::Write,
        heading: &'a str,
    ) -> fmt::Result {
        write!(buf, "\t<h3 class=\"sidebar-heading\">{heading}</a>\n")
    }

    fn render_sidebar_entry(&mut self, buf: &mut impl fmt::Write, entry: IdentId) -> fmt::Result {
        // Sidebar entries should all be relative URLs and unqualified names
        buf.write_str("<a href=\"")?;
        self.render_relative_url(entry, buf)?;
        write!(buf, "\">{}</a>", self.unqualified_name(entry))
    }

    fn render_relative_url(&self, ident_id: IdentId, buf: &mut impl fmt::Write) -> fmt::Result {
        write!(
            buf,
            // e.g. "Str#isEmpty"
            "{}#{}",
            self.module_name,
            self.unqualified_name(ident_id)
        )
    }

    fn render_type(
        &self,
        buf: &mut impl fmt::Write,
        indent: Indentation,
        typ: &Type,
        // Whether the type needs to be wrapped in parens (only matters if the rendered type contains spaces,
        // e.g. function application or tags with payloads)
        wrap_in_parens: WrapInParens,
    ) -> fmt::Result {
        use Type::*;

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
                name,
                captures,
                ambient_function,
            } => todo!(),
            UnspecializedLambdaSet { unspecialized } => todo!(),
            DelayedAlias(_) => todo!(),
            Alias {
                symbol,
                type_arguments,
                lambda_set_variables,
                infer_ext_in_output_types,
                actual,
                kind,
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

    fn render_function_type<'b>(
        &self,
        buf: &mut impl fmt::Write,
        indent: Indentation,
        mut args: impl ExactSizeIterator<Item = &'b Type>,
        ret: &Type,
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
}
fn is_multiline(first: &Type) -> bool {
    let todo = todo!();
}

const LINK_ICON_SVG: &str = r#"
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 512" fill="currentColor"><!--!
	Font Awesome Free 6.3.0 by @fontawesome - https://fontawesome.com License -
	https://fontawesome.com/license/free (Icons: CC BY 4.0, Fonts: SIL OFL 1.1, Code: MIT License)
	Copyright 2023 Fonticons, Inc. -->
	<path d="M562.8 267.7c56.5-56.5 56.5-148 0-204.5c-50-50-128.8-56.5-186.3-15.4l-1.6 1.1c-14.4 10.3-17.7 30.3-7.4 44.6s30.3 17.7 44.6 7.4l1.6-1.1c32.1-22.9 76-19.3 103.8 8.6c31.5 31.5 31.5 82.5 0 114L405.3 334.8c-31.5 31.5-82.5 31.5-114 0c-27.9-27.9-31.5-71.8-8.6-103.8l1.1-1.6c10.3-14.4 6.9-34.4-7.4-44.6s-34.4-6.9-44.6 7.4l-1.1 1.6C189.5 251.2 196 330 246 380c56.5 56.5 148 56.5 204.5 0L562.8 267.7zM43.2 244.3c-56.5 56.5-56.5 148 0 204.5c50 50 128.8 56.5 186.3 15.4l1.6-1.1c14.4-10.3 17.7-30.3 7.4-44.6s-30.3-17.7-44.6-7.4l-1.6 1.1c-32.1 22.9-76 19.3-103.8-8.6C57 372 57 321 88.5 289.5L200.7 177.2c31.5-31.5 82.5-31.5 114 0c27.9 27.9 31.5 71.8 8.6 103.9l-1.1 1.6c-10.3 14.4-6.9 34.4 7.4 44.6s34.4 6.9 44.6-7.4l1.1-1.6C416.5 260.8 410 182 360 132c-56.5-56.5-148-56.5-204.5 0L43.2 244.3z"></path>
</svg>
"#;
