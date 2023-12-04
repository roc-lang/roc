use crate::annotation::{is_collection_multiline, Formattable, Newlines, Parens};
use crate::collection::{fmt_collection, Braces};
use crate::pattern::fmt_pattern;
use crate::spaces::{fmt_default_newline, fmt_default_spaces, fmt_spaces, INDENT};
use crate::Buf;
use roc_parse::ast::{
    AbilityMember, Collection, CommentOrNewline, Defs, Expr, ExtractSpaces, Pattern, Spaced,
    Spaces, StrLiteral, TypeAnnotation, TypeDef, TypeHeader, ValueDef,
};
use roc_parse::header::{ExposedName, ModuleName};
use roc_region::all::Loc;

/// A Located formattable value is also formattable

impl<'a> Formattable for Defs<'a> {
    fn is_multiline(&self) -> bool {
        !self.tags.is_empty()
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        let mut prev_spaces = true;

        for (index, def) in self.defs().enumerate() {
            let spaces_before = &self.spaces[self.space_before[index].indices()];
            let spaces_after = &self.spaces[self.space_after[index].indices()];

            if prev_spaces {
                fmt_spaces(buf, spaces_before.iter(), indent);
            } else {
                fmt_default_newline(buf, spaces_before, indent);
            }

            match def {
                Ok(type_def) => type_def.format(buf, indent),
                Err(value_def) => value_def.format(buf, indent),
            }

            fmt_spaces(buf, spaces_after.iter(), indent);

            prev_spaces = !spaces_after.is_empty();
        }
    }
}

impl<'a> Formattable for TypeDef<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::TypeDef::*;

        match self {
            Alias { ann, .. } => ann.is_multiline(),
            Opaque { typ, .. } => typ.is_multiline(),
            Ability { members, .. } => members.iter().any(|d| d.is_multiline()),
        }
    }

    fn format_with_options(&self, buf: &mut Buf, _parens: Parens, newlines: Newlines, indent: u16) {
        use roc_parse::ast::TypeDef::*;

        match self {
            Alias {
                header: TypeHeader { name, vars },
                ann,
            } => {
                buf.indent(indent);
                buf.push_str(name.value);

                for var in *vars {
                    buf.spaces(1);

                    let need_parens = matches!(var.value, Pattern::Apply(..));

                    if need_parens {
                        buf.push_str("(");
                    }

                    fmt_pattern(buf, &var.value, indent, Parens::NotNeeded);
                    buf.indent(indent);

                    if need_parens {
                        buf.push_str(")");
                    }
                }

                buf.push_str(" :");
                buf.spaces(1);

                ann.format(buf, indent)
            }
            Opaque {
                header,
                typ: ann,
                derived: has_abilities,
            } => {
                let ann_is_where_clause =
                    matches!(ann.extract_spaces().item, TypeAnnotation::Where(..));

                // Always put the has-abilities clause on a newline if the opaque annotation
                // contains a where-has clause.
                let has_abilities_multiline = if let Some(has_abilities) = has_abilities {
                    !has_abilities.value.is_empty() && ann_is_where_clause
                } else {
                    false
                };

                let make_multiline = ann.is_multiline() || has_abilities_multiline;

                fmt_general_def(header, buf, indent, ":=", &ann.value, newlines);

                if let Some(has_abilities) = has_abilities {
                    buf.spaces(1);

                    has_abilities.format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::from_bool(make_multiline),
                        indent + INDENT,
                    );
                }
            }
            Ability {
                header: TypeHeader { name, vars },
                loc_implements: _,
                members,
            } => {
                buf.indent(indent);
                buf.push_str(name.value);
                for var in *vars {
                    buf.spaces(1);
                    fmt_pattern(buf, &var.value, indent, Parens::NotNeeded);
                    buf.indent(indent);
                }
                buf.spaces(1);
                buf.push_str(roc_parse::keyword::IMPLEMENTS);

                if !self.is_multiline() {
                    debug_assert_eq!(members.len(), 1);
                    buf.spaces(1);
                    members[0].format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::No,
                        indent + INDENT,
                    );
                } else {
                    for member in members.iter() {
                        member.format_with_options(
                            buf,
                            Parens::NotNeeded,
                            Newlines::Yes,
                            indent + INDENT,
                        );
                    }
                }
            }
        }
    }
}

impl<'a> Formattable for TypeHeader<'a> {
    fn is_multiline(&self) -> bool {
        self.vars.iter().any(|v| v.is_multiline())
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        buf.indent(indent);
        buf.push_str(self.name.value);

        for var in self.vars.iter() {
            buf.spaces(1);
            fmt_pattern(buf, &var.value, indent, Parens::NotNeeded);
            buf.indent(indent);
        }
    }
}

impl<'a> Formattable for ValueDef<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::ValueDef::*;

        match self {
            Annotation(loc_pattern, loc_annotation) => {
                loc_pattern.is_multiline() || loc_annotation.is_multiline()
            }
            Body(loc_pattern, loc_expr) => loc_pattern.is_multiline() || loc_expr.is_multiline(),
            AnnotatedBody { .. } => true,
            Expect { condition, .. } => condition.is_multiline(),
            ExpectFx { condition, .. } => condition.is_multiline(),
            Dbg { condition, .. } => condition.is_multiline(),
            ModuleImport {
                name,
                alias,
                exposed,
            } => {
                name.is_multiline()
                    || alias.map_or(false, |a| a.is_multiline())
                    || exposed.map_or(false, |(_, exp)| is_collection_multiline(&exp))
            }
        }
    }

    fn format_with_options(&self, buf: &mut Buf, _parens: Parens, newlines: Newlines, indent: u16) {
        use roc_parse::ast::ValueDef::*;
        match self {
            Annotation(loc_pattern, loc_annotation) => {
                fmt_general_def(
                    loc_pattern,
                    buf,
                    indent,
                    ":",
                    &loc_annotation.value,
                    newlines,
                );
            }
            Body(loc_pattern, loc_expr) => {
                fmt_body(buf, &loc_pattern.value, &loc_expr.value, indent);
            }
            Dbg { condition, .. } => fmt_dbg_in_def(buf, condition, self.is_multiline(), indent),
            Expect { condition, .. } => fmt_expect(buf, condition, self.is_multiline(), indent),
            ExpectFx { condition, .. } => {
                fmt_expect_fx(buf, condition, self.is_multiline(), indent)
            }
            AnnotatedBody {
                ann_pattern,
                ann_type,
                comment,
                body_pattern,
                body_expr,
            } => {
                fmt_general_def(ann_pattern, buf, indent, ":", &ann_type.value, newlines);

                if let Some(comment_str) = comment {
                    buf.push_str(" #");
                    buf.spaces(1);
                    buf.push_str(comment_str.trim());
                }

                buf.newline();
                fmt_body(buf, &body_pattern.value, &body_expr.value, indent);
            }
            ModuleImport {
                name,
                alias,
                exposed,
            } => {
                buf.indent(indent);
                buf.push_str("import");
                fmt_import_body(buf, &name, &alias, &exposed, indent + INDENT);
            }
        }
    }
}

fn fmt_import_body<'a>(
    buf: &mut Buf,
    name: &'a Loc<Spaced<'a, ModuleName>>,
    alias: &'a Option<Loc<Spaced<'a, ModuleName>>>,
    exposed: &'a Option<(
        &[CommentOrNewline<'a>],
        Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    )>,
    indent: u16,
) -> () {
    let name_spaces = name.extract_spaces();
    fmt_default_spaces(buf, name_spaces.before, indent);

    buf.indent(indent);
    buf.push_str(name.value.item().as_str());

    fmt_default_spaces(buf, name_spaces.after, indent);

    if let Some(alias_name) = alias {
        let alias_spaces = alias_name.extract_spaces();

        if !alias_spaces.before.is_empty() {
            buf.ensure_ends_with_newline();
        }

        buf.indent(indent);
        buf.push_str("as");

        fmt_default_spaces(buf, alias_spaces.before, indent + INDENT);

        buf.indent(indent + INDENT);
        buf.push_str(alias_name.value.item().as_str());

        fmt_default_spaces(buf, alias_spaces.after, indent);

        if alias_name.is_multiline() {
            buf.ensure_ends_with_newline();
        }
    }

    if let Some((spaces_before_list, exposed_list)) = exposed {
        let content_indent = if spaces_before_list.is_empty() {
            if buf.ends_with_newline() {
                indent
            } else {
                // Align list's closing brace with import keyword if we are in the same line
                indent - INDENT
            }
        } else {
            buf.ensure_ends_with_newline();
            indent + INDENT
        };

        buf.indent(indent);
        buf.push_str("exposing");

        fmt_default_spaces(buf, spaces_before_list, content_indent);
        fmt_collection(
            buf,
            content_indent,
            Braces::Square,
            *exposed_list,
            Newlines::No,
        );
    }
}

fn fmt_general_def<L: Formattable>(
    lhs: L,
    buf: &mut Buf,
    indent: u16,
    sep: &str,
    rhs: &TypeAnnotation,
    newlines: Newlines,
) {
    lhs.format(buf, indent);
    buf.indent(indent);

    if rhs.is_multiline() {
        buf.spaces(1);
        buf.push_str(sep);
        buf.spaces(1);

        let should_outdent = should_outdent(rhs);

        if should_outdent {
            match rhs {
                TypeAnnotation::SpaceBefore(sub_def, _) => {
                    sub_def.format_with_options(buf, Parens::NotNeeded, Newlines::No, indent);
                }
                _ => {
                    rhs.format_with_options(buf, Parens::NotNeeded, Newlines::No, indent);
                }
            }
        } else {
            rhs.format_with_options(buf, Parens::NotNeeded, newlines, indent + INDENT);
        }
    } else {
        buf.spaces(1);
        buf.push_str(sep);
        buf.spaces(1);
        rhs.format_with_options(buf, Parens::NotNeeded, Newlines::No, indent);
    }
}

fn should_outdent(mut rhs: &TypeAnnotation) -> bool {
    loop {
        match rhs {
            TypeAnnotation::SpaceBefore(sub_def, spaces) => {
                let is_only_newlines = spaces.iter().all(|s| s.is_newline());
                if !is_only_newlines || !sub_def.is_multiline() {
                    return false;
                }
                rhs = sub_def;
            }
            TypeAnnotation::Where(ann, _clauses) => {
                if !ann.is_multiline() {
                    return false;
                }
                rhs = &ann.value;
            }
            TypeAnnotation::Record { .. } | TypeAnnotation::TagUnion { .. } => return true,
            _ => return false,
        }
    }
}

fn fmt_dbg_in_def<'a>(buf: &mut Buf, condition: &'a Loc<Expr<'a>>, _: bool, indent: u16) {
    buf.ensure_ends_with_newline();
    buf.indent(indent);
    buf.push_str("dbg");

    buf.spaces(1);

    condition.format(buf, indent);
}

fn fmt_expect<'a>(buf: &mut Buf, condition: &'a Loc<Expr<'a>>, is_multiline: bool, indent: u16) {
    buf.ensure_ends_with_newline();
    buf.indent(indent);
    buf.push_str("expect");

    let return_indent = if is_multiline {
        buf.newline();
        indent + INDENT
    } else {
        buf.spaces(1);
        indent
    };

    condition.format(buf, return_indent);
}

fn fmt_expect_fx<'a>(buf: &mut Buf, condition: &'a Loc<Expr<'a>>, is_multiline: bool, indent: u16) {
    buf.ensure_ends_with_newline();
    buf.indent(indent);
    buf.push_str("expect-fx");

    let return_indent = if is_multiline {
        buf.newline();
        indent + INDENT
    } else {
        buf.spaces(1);
        indent
    };

    condition.format(buf, return_indent);
}

pub fn fmt_value_def(buf: &mut Buf, def: &roc_parse::ast::ValueDef, indent: u16) {
    def.format(buf, indent);
}

pub fn fmt_type_def(buf: &mut Buf, def: &roc_parse::ast::TypeDef, indent: u16) {
    def.format(buf, indent);
}

pub fn fmt_defs(buf: &mut Buf, defs: &Defs, indent: u16) {
    defs.format(buf, indent);
}

pub fn fmt_body<'a>(buf: &mut Buf, pattern: &'a Pattern<'a>, body: &'a Expr<'a>, indent: u16) {
    pattern.format_with_options(buf, Parens::InApply, Newlines::No, indent);
    buf.indent(indent);
    buf.push_str(" =");

    if body.is_multiline() {
        match body {
            Expr::SpaceBefore(sub_def, spaces) => {
                let should_outdent = match sub_def {
                    Expr::Record { .. } | Expr::List { .. } => {
                        let is_only_newlines = spaces.iter().all(|s| s.is_newline());
                        is_only_newlines && sub_def.is_multiline()
                    }
                    _ => false,
                };

                if should_outdent {
                    buf.spaces(1);
                    sub_def.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                } else {
                    body.format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::Yes,
                        indent + INDENT,
                    );
                }
            }
            Expr::Defs(..) | Expr::BinOps(_, _) | Expr::Backpassing(..) => {
                // Binop chains always get a newline. Otherwise you can have things like:
                //
                //     something = foo
                //        |> bar baz
                //
                // By always inserting a newline, this becomes:
                //
                //     something =
                //         foo
                //         |> bar baz
                //
                // This makes it clear what the binop is applying to!
                buf.newline();
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
            }
            Expr::When(..) | Expr::Str(StrLiteral::Block(_)) => {
                buf.ensure_ends_with_newline();
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
            }
            _ => {
                buf.spaces(1);
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
            }
        }
    } else {
        buf.spaces(1);
        body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
    }
}

impl<'a> Formattable for AbilityMember<'a> {
    fn is_multiline(&self) -> bool {
        self.name.value.is_multiline() || self.typ.is_multiline()
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        let Spaces { before, item, .. } = self.name.value.extract_spaces();
        fmt_spaces(buf, before.iter(), indent);

        buf.indent(indent);
        buf.push_str(item);
        buf.spaces(1);
        buf.push(':');
        buf.spaces(1);
        self.typ.value.format(buf, indent + INDENT);
    }
}
