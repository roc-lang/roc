use crate::annotation::{
    ann_lift_spaces_after, is_collection_multiline, Formattable, Newlines, Parens,
};
use crate::collection::{fmt_collection, Braces};
use crate::expr::{
    expr_lift_and_lower, expr_lift_spaces, expr_lift_spaces_after, expr_lift_spaces_before,
    fmt_str_literal, is_str_multiline, merge_spaces_conservative, sub_expr_requests_parens,
};
use crate::node::Nodify;
use crate::pattern::{pattern_lift_spaces, pattern_lift_spaces_before};
use crate::spaces::{
    fmt_comments_only, fmt_default_newline, fmt_default_spaces, fmt_spaces, NewlineAt, INDENT,
};
use crate::Buf;
use bumpalo::Bump;
use roc_error_macros::internal_error;
use roc_parse::ast::Spaceable;
use roc_parse::ast::{
    AbilityMember, Defs, Expr, ExtractSpaces, ImportAlias, ImportAsKeyword, ImportExposingKeyword,
    ImportedModuleName, IngestedFileAnnotation, IngestedFileImport, ModuleImport,
    ModuleImportParams, Pattern, Spaces, SpacesBefore, StrLiteral, TypeAnnotation, TypeDef,
    ValueDef,
};
use roc_parse::expr::merge_spaces;
use roc_parse::header::Keyword;
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
        let arena = buf.text.bump();

        for (index, def) in self.defs().enumerate() {
            let spaces_before = &self.spaces[self.space_before[index].indices()];
            let spaces_after = &self.spaces[self.space_after[index].indices()];

            let def = def_lift_spaces(buf.text.bump(), def);

            let spaces_before = merge_spaces(arena, spaces_before, def.before);
            let spaces_after = merge_spaces(arena, def.after, spaces_after);

            if prev_spaces {
                fmt_spaces(buf, spaces_before.iter(), indent);
            } else {
                fmt_default_newline(buf, spaces_before, indent);
            }

            match def.item {
                Ok(type_def) => type_def.format(buf, indent),
                Err(value_def) => value_def.format(buf, indent),
            }

            fmt_spaces(buf, spaces_after.iter(), indent);

            prev_spaces = !spaces_after.is_empty();
        }
    }
}

pub fn def_lift_spaces<'a, 'b: 'a>(
    arena: &'a Bump,
    def: Result<&'a TypeDef<'b>, &'a ValueDef<'b>>,
) -> Spaces<'a, Result<TypeDef<'a>, ValueDef<'a>>> {
    match def {
        Ok(td) => {
            let td = tydef_lift_spaces(arena, *td);
            Spaces {
                before: td.before,
                item: Ok(td.item),
                after: td.after,
            }
        }
        Err(vd) => {
            let vd = valdef_lift_spaces(arena, *vd);
            Spaces {
                before: vd.before,
                item: Err(vd.item),
                after: vd.after,
            }
        }
    }
}

pub fn tydef_lift_spaces<'a, 'b: 'a>(arena: &'a Bump, def: TypeDef<'b>) -> Spaces<'a, TypeDef<'a>> {
    match def {
        TypeDef::Alias { header, ann } => {
            let ann_lifted = ann_lift_spaces_after(arena, &ann.value);

            Spaces {
                before: &[],
                item: TypeDef::Alias {
                    header,
                    ann: Loc::at(ann.region, ann_lifted.item),
                },
                after: ann_lifted.after,
            }
        }
        TypeDef::Opaque {
            header,
            typ,
            derived,
        } => {
            if derived.is_some() {
                // It's structurally impossible for a derived clause to have spaces after
                Spaces {
                    before: &[],
                    item: def,
                    after: &[],
                }
            } else {
                let typ_lifted = ann_lift_spaces_after(arena, &typ.value);

                Spaces {
                    before: &[],
                    item: TypeDef::Opaque {
                        header,
                        typ: Loc::at(typ.region, typ_lifted.item),
                        derived,
                    },
                    after: typ_lifted.after,
                }
            }
        }
        TypeDef::Ability {
            header,
            loc_implements,
            members,
        } => {
            let new_members = arena.alloc_slice_copy(members);
            if let Some(last) = new_members.last_mut() {
                let typ = ann_lift_spaces_after(arena, &last.typ.value);
                last.typ.value = typ.item;

                Spaces {
                    before: &[],
                    item: TypeDef::Ability {
                        header,
                        loc_implements,
                        members: new_members,
                    },
                    after: typ.after,
                }
            } else {
                Spaces {
                    before: &[],
                    item: def,
                    after: &[],
                }
            }
        }
    }
}

pub fn valdef_lift_spaces<'a, 'b: 'a>(
    arena: &'a Bump,
    def: ValueDef<'b>,
) -> Spaces<'a, ValueDef<'a>> {
    match def {
        ValueDef::Annotation(pat, ann) => {
            let pat_lifted = pattern_lift_spaces_before(arena, &pat.value);
            let ann_lifted = ann_lift_spaces_after(arena, &ann.value);

            Spaces {
                before: pat_lifted.before,
                item: ValueDef::Annotation(
                    Loc::at(pat.region, pat_lifted.item),
                    Loc::at(ann.region, ann_lifted.item),
                ),
                after: ann_lifted.after,
            }
        }
        ValueDef::Body(pat, expr) => {
            let pat_lifted = pattern_lift_spaces(arena, &pat.value);

            // Don't format the `{} =` for defs with this pattern
            if is_body_unit_assignment(&pat_lifted, &expr.extract_spaces()) {
                let lifted = expr_lift_spaces(Parens::NotNeeded, arena, &expr.value);
                Spaces {
                    before: lifted.before,
                    item: ValueDef::Stmt(arena.alloc(Loc::at_zero(lifted.item))),
                    after: lifted.after,
                }
            } else {
                let lifted = expr_lift_spaces(Parens::NotNeeded, arena, &expr.value);
                Spaces {
                    before: pat_lifted.before,
                    item: ValueDef::Body(
                        arena.alloc(Loc::at(
                            pat.region,
                            pat_lifted.item.maybe_after(arena, pat_lifted.after),
                        )),
                        expr,
                    ),
                    after: lifted.after,
                }
            }
        }
        ValueDef::AnnotatedBody {
            ann_pattern,
            ann_type,
            lines_between,
            body_pattern,
            body_expr,
        } => {
            let ann_pattern_lifted = pattern_lift_spaces_before(arena, &ann_pattern.value);
            let ann_type_lifted = ann_lift_spaces_after(arena, &ann_type.value);
            let body_pattern_lifted = pattern_lift_spaces_before(arena, &body_pattern.value);
            let body_expr_lifted =
                expr_lift_spaces_after(Parens::NotNeeded, arena, &body_expr.value);

            let lines_between = merge_spaces(
                arena,
                ann_type_lifted.after,
                merge_spaces(arena, lines_between, body_pattern_lifted.before),
            );

            Spaces {
                before: ann_pattern_lifted.before,
                item: ValueDef::AnnotatedBody {
                    ann_pattern: arena.alloc(Loc::at(ann_pattern.region, ann_pattern_lifted.item)),
                    ann_type: arena.alloc(Loc::at(ann_type.region, ann_type_lifted.item)),
                    lines_between,
                    body_pattern: arena
                        .alloc(Loc::at(body_pattern.region, body_pattern_lifted.item)),
                    body_expr: arena.alloc(Loc::at(body_expr.region, body_expr_lifted.item)),
                },
                after: body_expr_lifted.after,
            }
        }
        ValueDef::Dbg {
            condition,
            preceding_comment,
        } => {
            let condition_lifted =
                expr_lift_spaces_after(Parens::NotNeeded, arena, &condition.value);

            Spaces {
                before: &[],
                item: ValueDef::Dbg {
                    condition: arena.alloc(Loc::at(condition.region, condition_lifted.item)),
                    preceding_comment,
                },
                after: condition_lifted.after,
            }
        }
        ValueDef::Expect {
            condition,
            preceding_comment,
        } => {
            let condition_lifted =
                expr_lift_spaces_after(Parens::NotNeeded, arena, &condition.value);

            Spaces {
                before: &[],
                item: ValueDef::Expect {
                    condition: arena.alloc(Loc::at(condition.region, condition_lifted.item)),
                    preceding_comment,
                },
                after: condition_lifted.after,
            }
        }
        ValueDef::ModuleImport(module_import) => {
            // Module imports begin with 'import', and end with either a ImportAlias or Collection
            // No spaces in sight!
            Spaces {
                before: &[],
                item: ValueDef::ModuleImport(module_import),
                after: &[],
            }
        }
        ValueDef::IngestedFileImport(mut ingested_file_import) => {
            // Ingested file imports begin with 'import', but can end with a TypeAnnotation, which can have spaces
            let after = if let Some(ann) = &mut ingested_file_import.annotation {
                let lifted = ann_lift_spaces_after(arena, &ann.annotation.value);
                ann.annotation.value = lifted.item;
                lifted.after
            } else {
                &[]
            };
            Spaces {
                before: &[],
                item: ValueDef::IngestedFileImport(ingested_file_import),
                after,
            }
        }
        ValueDef::Stmt(expr) => {
            let expr_lifted = expr_lift_spaces(Parens::NotNeeded, arena, &expr.value);
            Spaces {
                before: expr_lifted.before,
                item: ValueDef::Stmt(arena.alloc(Loc::at(expr.region, expr_lifted.item))),
                after: expr_lifted.after,
            }
        }
        ValueDef::StmtAfterExpr => Spaces {
            before: &[],
            item: ValueDef::StmtAfterExpr,
            after: &[],
        },
    }
}

pub fn valdef_lift_spaces_before<'a, 'b: 'a>(
    arena: &'a Bump,
    def: ValueDef<'b>,
) -> SpacesBefore<'a, ValueDef<'a>> {
    match def {
        ValueDef::Annotation(pat, ann) => {
            let pat_lifted = pattern_lift_spaces_before(arena, &pat.value);

            SpacesBefore {
                before: pat_lifted.before,
                item: ValueDef::Annotation(Loc::at(pat.region, pat_lifted.item), ann),
            }
        }
        ValueDef::Body(pat, expr) => {
            let pat_lifted = pattern_lift_spaces(arena, &pat.value);

            // Don't format the `{} =` for defs with this pattern
            if is_body_unit_assignment(&pat_lifted, &expr.extract_spaces()) {
                let lifted = expr_lift_spaces_before(Parens::NotNeeded, arena, &expr.value);
                SpacesBefore {
                    before: lifted.before,
                    item: ValueDef::Stmt(arena.alloc(Loc::at_zero(lifted.item))),
                }
            } else {
                SpacesBefore {
                    before: pat_lifted.before,
                    item: ValueDef::Body(
                        arena.alloc(Loc::at(
                            pat.region,
                            pat_lifted.item.maybe_after(arena, pat_lifted.after),
                        )),
                        expr,
                    ),
                }
            }
        }
        ValueDef::AnnotatedBody {
            ann_pattern,
            ann_type,
            lines_between,
            body_pattern,
            body_expr,
        } => {
            let ann_pattern_lifted = pattern_lift_spaces_before(arena, &ann_pattern.value);
            let ann_type_lifted = ann_lift_spaces_after(arena, &ann_type.value);
            let body_pattern_lifted = pattern_lift_spaces_before(arena, &body_pattern.value);

            let lines_between = merge_spaces(
                arena,
                ann_type_lifted.after,
                merge_spaces(arena, lines_between, body_pattern_lifted.before),
            );

            SpacesBefore {
                before: ann_pattern_lifted.before,
                item: ValueDef::AnnotatedBody {
                    ann_pattern: arena.alloc(Loc::at(ann_pattern.region, ann_pattern_lifted.item)),
                    ann_type: arena.alloc(Loc::at(ann_type.region, ann_type_lifted.item)),
                    lines_between,
                    body_pattern: arena
                        .alloc(Loc::at(body_pattern.region, body_pattern_lifted.item)),
                    body_expr,
                },
            }
        }
        ValueDef::Dbg {
            condition,
            preceding_comment,
        } => SpacesBefore {
            before: &[],
            item: ValueDef::Dbg {
                condition,
                preceding_comment,
            },
        },
        ValueDef::Expect {
            condition,
            preceding_comment,
        } => SpacesBefore {
            before: &[],
            item: ValueDef::Expect {
                condition,
                preceding_comment,
            },
        },
        ValueDef::ModuleImport(module_import) => {
            // Module imports always start with 'import', no spaces
            SpacesBefore {
                before: &[],
                item: ValueDef::ModuleImport(module_import),
            }
        }
        ValueDef::IngestedFileImport(ingested_file_import) => {
            // Similarly, ingested file imports always start with 'import', no spaces
            SpacesBefore {
                before: &[],
                item: ValueDef::IngestedFileImport(ingested_file_import),
            }
        }
        ValueDef::Stmt(expr) => {
            let expr_lifted = expr_lift_spaces_before(Parens::NotNeeded, arena, &expr.value);
            SpacesBefore {
                before: expr_lifted.before,
                item: ValueDef::Stmt(arena.alloc(Loc::at(expr.region, expr_lifted.item))),
            }
        }
        ValueDef::StmtAfterExpr => SpacesBefore {
            before: &[],
            item: ValueDef::StmtAfterExpr,
        },
    }
}

fn is_body_unit_assignment(pat: &Spaces<'_, Pattern<'_>>, body: &Spaces<'_, Expr<'_>>) -> bool {
    if let Pattern::RecordDestructure(collection) = pat.item {
        collection.is_empty()
            && pat.before.iter().all(|s| s.is_newline())
            && pat.after.iter().all(|s| s.is_newline())
            && !matches!(body.item, Expr::Defs(..))
            && !matches!(body.item, Expr::Return(..))
            && !matches!(body.item, Expr::DbgStmt { .. })
            && !starts_with_expect_ident(&body.item)
    } else {
        false
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
            Alias { header, ann } => {
                fmt_general_def(
                    header,
                    Parens::NotNeeded,
                    buf,
                    indent,
                    ":",
                    &ann.value,
                    newlines,
                );
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
                    !has_abilities.item.value.is_empty() && ann_is_where_clause
                } else {
                    false
                };

                let make_multiline = ann.is_multiline() || has_abilities_multiline;

                fmt_general_def(
                    header,
                    Parens::NotNeeded,
                    buf,
                    indent,
                    ":=",
                    &ann.value,
                    newlines,
                );

                if let Some(has_abilities) = has_abilities {
                    buf.spaces(1);

                    (*has_abilities).format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::from_bool(make_multiline),
                        indent + INDENT,
                    );
                }
            }
            Ability {
                header,
                loc_implements,
                members,
            } => {
                let header_lifted = header.to_node(buf.text.bump(), buf.flags());
                header_lifted.node.format(buf, indent);
                let implements = loc_implements.value.extract_spaces();
                let before_implements = merge_spaces_conservative(
                    buf.text.bump(),
                    header_lifted.after,
                    implements.before,
                );
                buf.spaces(1);
                fmt_comments_only(
                    buf,
                    before_implements.iter(),
                    NewlineAt::Bottom,
                    indent + INDENT,
                );
                buf.indent(indent + INDENT);
                buf.push_str(roc_parse::keyword::IMPLEMENTS);
                fmt_comments_only(
                    buf,
                    implements.after.iter(),
                    NewlineAt::Bottom,
                    indent + INDENT,
                );
                buf.spaces(1);

                if !self.is_multiline() {
                    debug_assert_eq!(members.len(), 1);
                    members[0].format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::No,
                        indent + INDENT,
                    );
                } else {
                    for member in members.iter() {
                        let Spaces {
                            before,
                            item,
                            after,
                        } = member.name.value.extract_spaces();
                        fmt_spaces(buf, before.iter(), indent + INDENT);
                        buf.ensure_ends_with_newline();

                        buf.indent(indent + INDENT);
                        buf.push_str(item);
                        fmt_spaces(buf, after.iter(), indent + INDENT);
                        buf.spaces(1);
                        buf.push(':');
                        buf.spaces(1);

                        member.typ.value.format(buf, indent + 2 * INDENT);
                    }
                }
            }
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
            || params.map(|p| !p.before.is_empty()).unwrap_or(false)
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
        buf.indent(indent);
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
        buf.indent(indent);
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
                let pat_parens = if ann_pattern_needs_parens(&loc_pattern.value) {
                    Parens::InApply
                } else {
                    Parens::NotNeeded
                };
                fmt_general_def(
                    loc_pattern,
                    pat_parens,
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
                let pat_parens = if ann_pattern_needs_parens(&ann_pattern.value) {
                    Parens::InApply
                } else {
                    Parens::NotNeeded
                };
                fmt_general_def(
                    ann_pattern,
                    pat_parens,
                    buf,
                    indent,
                    ":",
                    &ann_type.value,
                    newlines,
                );

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

fn ann_pattern_needs_parens(value: &Pattern<'_>) -> bool {
    match value.extract_spaces().item {
        Pattern::Tag(_) => true,
        Pattern::Apply(func, _args) if matches!(func.extract_spaces().item, Pattern::Tag(..)) => {
            true
        }
        _ => false,
    }
}

fn fmt_general_def<L: Formattable>(
    lhs: L,
    lhs_parens: Parens,
    buf: &mut Buf,
    indent: u16,
    sep: &str,
    rhs: &TypeAnnotation,
    newlines: Newlines,
) {
    lhs.format_with_options(buf, lhs_parens, Newlines::Yes, indent);
    buf.indent(indent);

    buf.spaces(1);
    buf.push_str(sep);
    buf.spaces(1);

    let rhs = rhs.to_node(buf.text.bump(), buf.flags());

    if rhs.node.is_multiline() || !rhs.before.is_empty() || !rhs.after.is_empty() {
        if rhs.node.is_multiline() && !rhs.needs_indent && rhs.before.iter().all(|s| s.is_newline())
        {
            rhs.node
                .format_with_options(buf, Parens::NotNeeded, Newlines::No, indent);
        } else {
            buf.ensure_ends_with_newline();
            fmt_comments_only(buf, rhs.before.iter(), NewlineAt::Bottom, indent + INDENT);
            rhs.node
                .format_with_options(buf, Parens::NotNeeded, newlines, indent + INDENT);
        }
    } else {
        fmt_comments_only(buf, rhs.before.iter(), NewlineAt::Bottom, indent);
        rhs.node
            .format_with_options(buf, Parens::NotNeeded, Newlines::No, indent);
    }

    fmt_comments_only(buf, rhs.after.iter(), NewlineAt::Bottom, indent);
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
    pattern.format_with_options(buf, Parens::InApply, Newlines::No, indent);

    if pattern.is_multiline() {
        buf.ensure_ends_with_newline();
        buf.indent(indent);
    } else {
        buf.spaces(1);
    }
    let indent = buf.cur_line_indent();
    buf.push_str("=");

    let body = expr_lift_and_lower(Parens::NotNeeded, buf.text.bump(), body);

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
            Expr::Apply(
                Loc {
                    value: Expr::Str(StrLiteral::Block(..)),
                    ..
                },
                ..,
            )
            | Expr::PncApply(
                Loc {
                    value: Expr::Str(StrLiteral::Block(..)),
                    ..
                },
                ..,
            ) => {
                buf.spaces(1);
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
            }
            Expr::Str(s) => {
                if is_str_multiline(&s) {
                    buf.ensure_ends_with_newline();
                } else {
                    buf.spaces(1);
                }
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
            }
            _ if starts_with_block_string_literal(&body) => {
                buf.ensure_ends_with_newline();
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
            }
            Expr::When(..) => {
                buf.ensure_ends_with_newline();
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
            }
            Expr::Defs(..) | Expr::BinOps(_, _) => {
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
            Expr::ParensAround(&Expr::SpaceBefore(sub_def, _)) => {
                let needs_indent = !sub_expr_requests_parens(sub_def);
                let indent = if needs_indent {
                    indent + INDENT
                } else {
                    indent
                };
                buf.spaces(1);
                body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
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

fn starts_with_expect_ident(expr: &Expr<'_>) -> bool {
    // We need to be persnickety about not formatting `{}=expect foo` into `expect foo`,
    // because `expect` is treated as a keyword at the statement level but not at the expr level.
    // If we removed the `{}=` in this case, that would change the meaning
    match expr {
        Expr::Apply(inner, _, _) => starts_with_expect_ident(&inner.value),
        Expr::PncApply(inner, _) => starts_with_expect_ident(&inner.value),
        Expr::Var { module_name, ident } => {
            module_name.is_empty() && (*ident == "expect" || *ident == "expect!")
        }
        _ => false,
    }
}

pub fn starts_with_block_string_literal(expr: &Expr<'_>) -> bool {
    match expr {
        Expr::Str(s) => is_str_multiline(s),
        Expr::SpaceAfter(inner, _) | Expr::SpaceBefore(inner, _) => {
            starts_with_block_string_literal(inner)
        }
        Expr::Apply(inner, _, _) => starts_with_block_string_literal(&inner.value),
        Expr::RecordAccess(inner, _) => starts_with_block_string_literal(inner),
        Expr::TupleAccess(inner, _) => starts_with_block_string_literal(inner),
        Expr::PncApply(inner, _) => starts_with_block_string_literal(&inner.value),
        Expr::TrySuffix(inner) => starts_with_block_string_literal(inner),
        _ => false,
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
