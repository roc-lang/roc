use crate::annotation::{is_collection_multiline, Formattable, Newlines, Parens};
use crate::collection::{fmt_collection, Braces};
use crate::expr::{fmt_str_literal, pattern_needs_parens_when_backpassing};
use crate::pattern::fmt_pattern;
use crate::spaces::{fmt_default_newline, fmt_default_spaces, fmt_spaces, INDENT};
use crate::Buf;
use roc_error_macros::internal_error;
use roc_parse::ast::{
    AbilityMember, Defs, Expr, ExtractSpaces, ImportAlias, ImportAsKeyword, ImportExposingKeyword,
    ImportedModuleName, IngestedFileAnnotation, IngestedFileImport, ModuleImport,
    ModuleImportParams, Pattern, Spaces, Stmt, StrLiteral, TopLevelDefs, TypeAnnotation, TypeDef,
    TypeHeader, ValueDef,
};
use roc_parse::header::Keyword;
use roc_region::all::Loc;

/// A Located formattable value is also formattable

pub fn fmt_stmts(buf: &mut Buf<'_>, stmts: &TopLevelDefs<'_>, indent: u16) {
    let mut last_ann = None;

    let mut first = true;

    for stmt in stmts.item {
        let mut stmt = *stmt;

        if let Some((was_first, before_ann, ann_pattern, ann_type)) = last_ann.take() {
            if let Stmt::ValueDef(ValueDef::Body(body_pattern, body_expr)) = stmt.item.value {
                let spaces_middle = std::mem::replace(&mut stmt.before, before_ann);
                let arena = buf.text.bump();
                stmt.item.value = Stmt::ValueDef(ValueDef::AnnotatedBody {
                    ann_pattern: arena.alloc(ann_pattern),
                    ann_type: arena.alloc(ann_type),
                    lines_between: spaces_middle,
                    body_pattern,
                    body_expr,
                });
                first = was_first;
            } else {
                if was_first {
                    fmt_spaces(buf, before_ann.iter(), indent);
                } else {
                    fmt_default_newline(buf, before_ann, indent);
                }
                ValueDef::Annotation(ann_pattern, ann_type).format(buf, indent)
            }
        }

        if let Stmt::ValueDef(ValueDef::Annotation(header, ann_ty)) = stmt.item.value {
            last_ann = Some((first, stmt.before, header, ann_ty));
            continue;
        }

        if first {
            fmt_spaces(buf, stmt.before.iter(), indent);
        } else {
            fmt_default_newline(buf, stmt.before, indent);
        }

        first = false;

        match stmt.item.value {
            Stmt::Expr(expr) => expr.format(buf, indent),
            Stmt::Backpassing(loc_patterns, loc_body) => {
                use self::Expr::*;

                let arguments_are_multiline = loc_patterns
                    .iter()
                    .any(|loc_pattern| loc_pattern.is_multiline());

                // If the arguments are multiline, go down a line and indent.
                let indent = if arguments_are_multiline {
                    indent + INDENT
                } else {
                    indent
                };

                let mut it = loc_patterns.iter().peekable();

                while let Some(loc_pattern) = it.next() {
                    let needs_parens = if pattern_needs_parens_when_backpassing(&loc_pattern.value)
                    {
                        Parens::InApply
                    } else {
                        Parens::NotNeeded
                    };

                    loc_pattern.format_with_options(buf, needs_parens, Newlines::No, indent);

                    if it.peek().is_some() {
                        if arguments_are_multiline {
                            buf.push(',');
                            buf.newline();
                        } else {
                            buf.push_str(",");
                            buf.spaces(1);
                        }
                    }
                }

                if arguments_are_multiline {
                    buf.newline();
                    buf.indent(indent);
                } else {
                    buf.spaces(1);
                }

                buf.push_str("<-");

                let body_indent = if loc_body.is_multiline() {
                    indent + INDENT
                } else {
                    indent
                };

                // the body of the Backpass can be on the same line, or
                // on a new line. If it's on the same line, insert a space.

                match &loc_body.value {
                    SpaceBefore(_, _) => {
                        // the body starts with (first comment and then) a newline
                        // do nothing
                    }
                    _ => {
                        // add a space after the `<-`
                        buf.spaces(1);
                    }
                };

                loc_body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, body_indent);
            }
            Stmt::TypeDef(type_def) => type_def.format(buf, indent),
            Stmt::ValueDef(value_def) => value_def.format(buf, indent),
        }
    }

    if let Some((first, before_ann, header, ann_ty)) = last_ann {
        if first {
            fmt_spaces(buf, before_ann.iter(), indent);
        } else {
            fmt_default_newline(buf, before_ann, indent);
        }
        ValueDef::Annotation(header, ann_ty).format(buf, indent)
    }

    fmt_spaces(buf, stmts.after.iter(), indent);
}

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

impl<'a> Formattable for ModuleImport<'a> {
    fn is_multiline(&self) -> bool {
        let Self {
            before_name,
            name,
            params,
            alias,
            exposed,
        } = self;

        !before_name.is_empty()
            || name.is_multiline()
            || params.is_multiline()
            || alias.is_multiline()
            || match exposed {
                Some(a) => a.keyword.is_multiline() || is_collection_multiline(&a.item),
                None => false,
            }
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        let Self {
            before_name,
            name,
            params,
            alias,
            exposed,
        } = self;

        buf.indent(indent);
        buf.push_str("import");

        let indent = if !before_name.is_empty()
            || (params.is_multiline() && exposed.is_some())
            || alias.is_multiline()
            || exposed.map_or(false, |e| e.keyword.is_multiline())
        {
            indent + INDENT
        } else {
            indent
        };

        fmt_default_spaces(buf, before_name, indent);

        name.format(buf, indent);
        params.format(buf, indent);
        alias.format(buf, indent);

        if let Some(exposed) = exposed {
            exposed.keyword.format(buf, indent);
            fmt_collection(buf, indent, Braces::Square, exposed.item, Newlines::No);
        }
    }
}

impl<'a> Formattable for ModuleImportParams<'a> {
    fn is_multiline(&self) -> bool {
        let ModuleImportParams { before, params } = self;

        !before.is_empty() || is_collection_multiline(&params.value)
    }

    fn format_with_options(&self, buf: &mut Buf, _parens: Parens, newlines: Newlines, indent: u16) {
        let ModuleImportParams { before, params } = self;

        fmt_default_spaces(buf, before, indent);
        fmt_collection(buf, indent, Braces::Curly, params.value, newlines);
    }
}

impl<'a> Formattable for IngestedFileImport<'a> {
    fn is_multiline(&self) -> bool {
        let Self {
            before_path,
            path: _,
            name,
            annotation,
        } = self;
        !before_path.is_empty() || name.keyword.is_multiline() || annotation.is_multiline()
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        let Self {
            before_path,
            path,
            name,
            annotation,
        } = self;

        buf.indent(indent);
        buf.push_str("import");

        let indent = indent + INDENT;

        fmt_default_spaces(buf, before_path, indent);
        fmt_str_literal(buf, path.value, indent);

        name.keyword.format(buf, indent);
        buf.push_str(name.item.value);

        annotation.format(buf, indent);
    }
}

impl<'a> Formattable for ImportedModuleName<'a> {
    fn is_multiline(&self) -> bool {
        // No newlines in module name itself.
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        buf.indent(indent);

        if let Some(package_shorthand) = self.package {
            buf.push_str(package_shorthand);
            buf.push_str(".");
        }

        self.name.format(buf, indent);
    }
}

impl<'a> Formattable for ImportAlias<'a> {
    fn is_multiline(&self) -> bool {
        // No newlines in alias itself.
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        buf.indent(indent);
        buf.push_str(self.as_str());
    }
}

impl Formattable for ImportAsKeyword {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf<'_>,
        _parens: crate::annotation::Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        buf.indent(indent);
        buf.push_str(ImportAsKeyword::KEYWORD);
    }
}

impl Formattable for ImportExposingKeyword {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf<'_>,
        _parens: crate::annotation::Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        buf.indent(indent);
        buf.push_str(ImportExposingKeyword::KEYWORD);
    }
}

impl<'a> Formattable for IngestedFileAnnotation<'a> {
    fn is_multiline(&self) -> bool {
        let Self {
            before_colon,
            annotation,
        } = self;
        !before_colon.is_empty() || annotation.is_multiline()
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        let Self {
            before_colon,
            annotation,
        } = self;

        fmt_default_spaces(buf, before_colon, indent);
        buf.push_str(":");
        buf.spaces(1);
        annotation.format(buf, indent);
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
            Dbg { condition, .. } => condition.is_multiline(),
            ModuleImport(module_import) => module_import.is_multiline(),
            IngestedFileImport(ingested_file_import) => ingested_file_import.is_multiline(),
            Stmt(loc_expr) => loc_expr.is_multiline(),
            StmtAfterExpr => internal_error!("shouldn't exist before can"),
        }
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
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
            AnnotatedBody {
                ann_pattern,
                ann_type,
                lines_between,
                body_pattern,
                body_expr,
            } => {
                fmt_general_def(ann_pattern, buf, indent, ":", &ann_type.value, newlines);

                fmt_annotated_body_comment(buf, indent, lines_between);

                buf.newline();
                fmt_body(buf, &body_pattern.value, &body_expr.value, indent);
            }
            ModuleImport(module_import) => module_import.format(buf, indent),
            IngestedFileImport(ingested_file_import) => ingested_file_import.format(buf, indent),
            Stmt(loc_expr) => loc_expr.format_with_options(buf, parens, newlines, indent),
            StmtAfterExpr => internal_error!("shouldn't exist before can"),
        }
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

pub fn fmt_value_def(buf: &mut Buf, def: &roc_parse::ast::ValueDef, indent: u16) {
    def.format(buf, indent);
}

pub fn fmt_type_def(buf: &mut Buf, def: &roc_parse::ast::TypeDef, indent: u16) {
    def.format(buf, indent);
}

pub fn fmt_defs(buf: &mut Buf, defs: &Defs, indent: u16) {
    defs.format(buf, indent);
}

pub fn fmt_annotated_body_comment<'a>(
    buf: &mut Buf,
    indent: u16,
    lines_between: &'a [roc_parse::ast::CommentOrNewline<'a>],
) {
    let mut comment_iter = lines_between.iter();
    if let Some(comment_first) = comment_iter.next() {
        match comment_first {
            roc_parse::ast::CommentOrNewline::Newline => (),
            roc_parse::ast::CommentOrNewline::DocComment(comment_str) => {
                buf.push_str(" # #");
                buf.spaces(1);
                buf.push_str(comment_str.trim());
            }
            roc_parse::ast::CommentOrNewline::LineComment(comment_str) => {
                buf.push_str(" #");
                buf.spaces(1);
                buf.push_str(comment_str.trim());
            }
        }

        for comment_or_newline in comment_iter {
            match comment_or_newline {
                roc_parse::ast::CommentOrNewline::Newline => (),
                roc_parse::ast::CommentOrNewline::DocComment(comment_str) => {
                    buf.newline();
                    buf.indent(indent);
                    buf.push_str("# #");
                    buf.spaces(1);
                    buf.push_str(comment_str.trim());
                }
                roc_parse::ast::CommentOrNewline::LineComment(comment_str) => {
                    buf.newline();
                    buf.indent(indent);
                    buf.push_str("#");
                    buf.spaces(1);
                    buf.push_str(comment_str.trim());
                }
            }
        }
    }
}

pub fn fmt_body<'a>(buf: &mut Buf, pattern: &'a Pattern<'a>, body: &'a Expr<'a>, indent: u16) {
    // Check if this is an assignment into the unit value
    let is_unit_assignment = if let Pattern::RecordDestructure(collection) = pattern {
        collection.is_empty()
    } else {
        false
    };

    // Don't format the `{} =` for defs with this pattern
    if !is_unit_assignment {
        pattern.format_with_options(buf, Parens::InApply, Newlines::No, indent);
        buf.indent(indent);
        buf.push_str(" =");
    }

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
