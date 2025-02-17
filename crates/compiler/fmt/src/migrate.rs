use roc_module::called_via::UnaryOp;
use roc_parse::{
    ast::{
        AbilityImpls, AssignedField, Base, Collection, Defs, Expr, FunctionArrow, Header,
        ImplementsAbilities, ImplementsAbility, ImplementsClause, ImportAlias, ImportAsKeyword,
        ImportExposingKeyword, ImportedModuleName, IngestedFileAnnotation, IngestedFileImport,
        ModuleImport, ModuleImportParams, Pattern, Spaced, Spaces, SpacesBefore, Tag,
        TypeAnnotation, TypeDef, TypeHeader, TypeVar, ValueDef,
    },
    header::{
        AppHeader, ExposedName, HostedHeader, Keyword, KeywordItem, ModuleHeader, ModuleName,
        PackageHeader, PlatformHeader,
    },
    ident::Accessor,
};
use roc_region::all::Loc;

use crate::{
    annotation::{is_collection_multiline, Parens},
    collection::Braces,
    expr::{expr_lift_spaces, fmt_str_literal, format_sq_literal, push_op},
    header::{
        fmt_app_header, fmt_hosted_header, fmt_package_header, fmt_platform_header,
        fmt_spaces_with_outdent,
    },
    pattern::snakify_camel_ident,
    spaces::fmt_spaces,
    Buf,
};

#[derive(Copy, Clone, Debug)]
pub enum Suffix {
    None,
    Comma,
    OpenRound,
    Question,
}

trait Fmt {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError>;
}

impl Fmt for Expr<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        fmt_expr(buf, indent, self, suffix)
    }
}

impl Fmt for Pattern<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        fmt_pattern(buf, indent, self, suffix)
    }
}

impl<F: Fmt> Fmt for &F {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        (*self).fmt(buf, indent, suffix)
    }
}

impl<F: Fmt> Fmt for Loc<F> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        self.value.fmt(buf, indent, suffix)
    }
}

impl<F: Fmt> Fmt for AssignedField<'_, F> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            AssignedField::RequiredValue(name, comment_or_newlines, loc1) => {
                buf.indent(indent);
                buf.push_str(name.value);
                if !comment_or_newlines.is_empty() {
                    fmt_spaces(buf, comment_or_newlines.iter(), indent);
                }
                buf.indent(indent);
                buf.push_str(":");
                buf.spaces(1);
                loc1.fmt(buf, indent, Suffix::None)?;
            }
            AssignedField::OptionalValue(name, comment_or_newlines, loc1) => {
                buf.indent(indent);
                buf.push_str(name.value);
                if !comment_or_newlines.is_empty() {
                    fmt_spaces(buf, comment_or_newlines.iter(), indent);
                }
                buf.indent(indent);
                buf.push_str(":");
                buf.spaces(1);
                loc1.fmt(buf, indent, Suffix::None)?;
            }
            AssignedField::IgnoredValue(name, comment_or_newlines, loc1) => {
                buf.indent(indent);
                buf.push('_');
                buf.push_str(name.value);
                if !comment_or_newlines.is_empty() {
                    fmt_spaces(buf, comment_or_newlines.iter(), indent);
                }
                buf.indent(indent);
                buf.push_str(":");
                buf.spaces(1);
                loc1.fmt(buf, indent, Suffix::None)?;
            }
            AssignedField::LabelOnly(name) => {
                buf.indent(indent);
                buf.push_str(name.value);
            }
            AssignedField::SpaceBefore(assigned_field, comment_or_newlines) => {
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
                assigned_field.fmt(buf, indent, suffix)?;
            }
            AssignedField::SpaceAfter(assigned_field, comment_or_newlines) => {
                assigned_field.fmt(buf, indent, suffix)?;
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
            }
        }
        Ok(())
    }
}

pub fn fmt_pattern(
    buf: &mut Buf,
    indent: u16,
    pat: &Pattern<'_>,
    suffix: Suffix,
) -> Result<(), MigrateError> {
    match pat {
        Pattern::Identifier { ident } => {
            buf.indent(indent);
            buf.push_str(ident);
        }
        Pattern::QualifiedIdentifier { module_name, ident } => {
            buf.indent(indent);
            buf.push_str(module_name);
            buf.push('.');
            buf.push_str(ident);
        }
        Pattern::Tag(tag) => {
            buf.indent(indent);
            buf.push_str(tag);
        }
        Pattern::OpaqueRef(opaque_ref) => {
            buf.indent(indent);
            buf.push_str(opaque_ref);
        }
        Pattern::Apply(loc, locs) => {
            fmt_pattern(buf, indent, &loc.value, Suffix::OpenRound)?;
            for loc in locs.iter() {
                fmt_pattern(buf, indent, &loc.value, Suffix::Comma)?;
            }
            buf.indent(indent);
            buf.push(')');
        }
        Pattern::PncApply(loc, collection) => {
            fmt_pattern(buf, indent, &loc.value, Suffix::OpenRound)?;
            for loc in collection.iter() {
                fmt_pattern(buf, indent, &loc.value, Suffix::Comma)?;
            }
            if !collection.final_comments().is_empty() {
                fmt_spaces(buf, collection.final_comments().iter(), indent);
            }
            buf.indent(indent);
            buf.push(')');
        }
        Pattern::RecordDestructure(collection) => {
            fmt_collection(buf, indent, Braces::Curly, None, *collection, None)?;
        }
        Pattern::RequiredField(name, loc) => {
            buf.indent(indent);
            buf.push_str(name);
            buf.push_str(":");
            buf.spaces(1);
            fmt_pattern(buf, indent, &loc.value, Suffix::None)?;
        }
        Pattern::OptionalField(name, loc) => {
            buf.indent(indent);
            buf.push_str(name);
            buf.push_str("?");
            buf.spaces(1);
            fmt_expr(buf, indent, &loc.value, Suffix::Question)?;
        }
        Pattern::NumLiteral(num_literal) => {
            buf.indent(indent);
            buf.push_str(num_literal);
        }
        Pattern::NonBase10Literal {
            string,
            base,
            is_negative,
        } => {
            buf.indent(indent);
            if *is_negative {
                buf.push('-');
            }
            match base {
                Base::Hex => buf.push_str("0x"),
                Base::Octal => buf.push_str("0o"),
                Base::Binary => buf.push_str("0b"),
                Base::Decimal => { /* nothing */ }
            }
            buf.push_str(string);
        }
        Pattern::FloatLiteral(float_literal) => {
            buf.indent(indent);
            buf.push_str(float_literal);
        }
        Pattern::StrLiteral(str_literal) => {
            buf.indent(indent);
            fmt_str_literal(buf, *str_literal, indent);
        }
        Pattern::Underscore(name) => {
            buf.indent(indent);
            buf.push('_');
            buf.push_str(name);
        }
        Pattern::SingleQuote(single_quote) => {
            buf.indent(indent);
            buf.push('\'');
            buf.push_str(single_quote);
            buf.push('\'');
        }
        Pattern::Tuple(collection) => {
            fmt_collection(buf, indent, Braces::Round, None, *collection, None)?;
        }
        Pattern::List(collection) => {
            fmt_collection(buf, indent, Braces::Square, None, *collection, None)?;
        }
        Pattern::ListRest(rest) => {
            buf.indent(indent);
            buf.push_str("..");
            if let Some(rest) = rest {
                fmt_spaces(buf, rest.0.iter(), indent);
                buf.indent(indent);
                buf.ensure_ends_with_whitespace();
                buf.push_str("as");
                fmt_spaces(buf, rest.1.spaces_before.iter(), indent);
                buf.ensure_ends_with_whitespace();
                buf.indent(indent);
                buf.push_str(rest.1.identifier.value);
            }
        }
        Pattern::As(loc, pattern_as) => {
            fmt_pattern(buf, indent, &loc.value, Suffix::None)?;
            buf.indent(indent);
            buf.push_str(" as");
            fmt_spaces(buf, pattern_as.spaces_before.iter(), indent);
            buf.ensure_ends_with_whitespace();
            buf.indent(indent);
            buf.push_str(pattern_as.identifier.value);
        }
        Pattern::SpaceBefore(pattern, comment_or_newlines) => {
            fmt_spaces(buf, comment_or_newlines.iter(), indent);
            fmt_pattern(buf, indent, pattern, suffix)?;
        }
        Pattern::SpaceAfter(pattern, comment_or_newlines) => {
            fmt_pattern(buf, indent, pattern, suffix)?;
            fmt_spaces(buf, comment_or_newlines.iter(), indent);
        }
        Pattern::Malformed(_malformed) => {
            return Err(MigrateError::MalformedPatternNotSupported);
        }
        Pattern::MalformedIdent(_, _bad_ident) => {
            return Err(MigrateError::MalformedPatternIdentNotSupported);
        }
        Pattern::MalformedExpr(_expr) => {
            return Err(MigrateError::MalformedPatternAsExprNotSupported);
        }
    }

    if !matches!(pat, Pattern::SpaceAfter(..) | Pattern::SpaceBefore(..)) {
        fmt_suffix(buf, indent, suffix);
    }

    Ok(())
}

pub fn fmt_expr(
    buf: &mut Buf,
    indent: u16,
    expr: &Expr<'_>,
    suffix: Suffix,
) -> Result<(), MigrateError> {
    match expr {
        Expr::Float(string) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        Expr::Num(string) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        Expr::Tag(string) | Expr::OpaqueRef(string) => {
            buf.indent(indent);
            buf.push_str(string)
        }
        Expr::SingleQuote(string) => {
            buf.indent(indent);
            format_sq_literal(buf, string);
        }
        Expr::NonBase10Int {
            base,
            string,
            is_negative,
        } => {
            buf.indent(indent);
            if *is_negative {
                buf.push('-');
            }

            match base {
                Base::Hex => buf.push_str("0x"),
                Base::Octal => buf.push_str("0o"),
                Base::Binary => buf.push_str("0b"),
                Base::Decimal => { /* nothing */ }
            }

            buf.push_str(string);
        }
        Expr::Str(literal) => {
            fmt_str_literal(buf, *literal, indent);
        }
        Expr::Var { module_name, ident } => {
            buf.indent(indent);
            if !module_name.is_empty() {
                buf.push_str(module_name);
                buf.push('.');
            }
            if buf.flags().snakify {
                snakify_camel_ident(buf, ident);
            } else {
                buf.push_str(ident);
            }
        }
        Expr::Underscore(name) => {
            buf.indent(indent);
            buf.push('_');
            buf.push_str(name);
        }
        Expr::Crash => {
            buf.indent(indent);
            buf.push_str("crash");
        }
        Expr::Dbg => {
            buf.indent(indent);
            buf.push_str("dbg");
        }
        Expr::Try => {
            buf.indent(indent);
            buf.push_str("try");
        }
        Expr::RecordAccess(expr, name) => {
            buf.indent(indent);
            fmt_expr(buf, indent, expr, Suffix::None)?;
            buf.push_str(".");
            buf.push_str(name);
        }
        Expr::TupleAccess(expr, index) => {
            buf.indent(indent);
            fmt_expr(buf, indent, expr, Suffix::None)?;
            buf.push_str(".");
            buf.push_str(index);
        }
        Expr::AccessorFunction(Accessor::TupleIndex(string))
        | Expr::AccessorFunction(Accessor::RecordField(string)) => {
            buf.indent(indent);
            buf.push_str(".");
            buf.push_str(string);
        }
        Expr::RecordUpdater(name) => {
            buf.indent(indent);
            buf.push_str("&");
            buf.push_str(name);
        }
        Expr::TrySuffix(expr) => {
            fmt_expr(buf, indent, expr, Suffix::Question)?;
        }
        Expr::List(collection) => {
            fmt_collection(buf, indent, Braces::Square, None, *collection, None)?;
        }
        Expr::RecordUpdate { update, fields } => {
            fmt_collection(
                buf,
                indent,
                Braces::Curly,
                Some(CollectionHead::Update(update)),
                *fields,
                None,
            )?;
        }
        Expr::Record(collection) => {
            fmt_collection(buf, indent, Braces::Curly, None, *collection, None)?;
        }
        Expr::Tuple(collection) => {
            fmt_collection(buf, indent, Braces::Round, None, *collection, None)?;
        }
        Expr::RecordBuilder { mapper, fields } => {
            fmt_collection(
                buf,
                indent,
                Braces::Curly,
                Some(CollectionHead::Mapper(mapper)),
                *fields,
                None,
            )?;
        }
        Expr::Closure(args, body) => {
            buf.indent(indent);
            buf.push('|');
            for arg in *args {
                fmt_pattern(buf, indent, &arg.value, Suffix::None)?;
            }
            buf.indent(indent);
            buf.push_str("|");
            buf.spaces(1);
            fmt_expr(buf, indent, &body.value, Suffix::None)?;
        }
        Expr::Defs(defs, final_expr) => {
            buf.indent(indent);
            buf.push('{');
            buf.ensure_ends_with_newline();

            for (index, def) in defs.defs().enumerate() {
                let spaces_before = &defs.spaces[defs.space_before[index].indices()];
                let spaces_after = &defs.spaces[defs.space_after[index].indices()];

                if !spaces_before.is_empty() {
                    fmt_spaces(buf, spaces_before.iter(), indent + 4);
                }

                match def {
                    Ok(type_def) => type_def.fmt(buf, indent + 4, Suffix::None)?,
                    Err(value_def) => value_def.fmt(buf, indent + 4, Suffix::None)?,
                }

                if !spaces_after.is_empty() {
                    fmt_spaces(buf, spaces_after.iter(), indent + 4);
                }
            }

            fmt_expr(buf, indent + 4, &final_expr.value, Suffix::None)?;

            buf.ensure_ends_with_newline();
            buf.indent(indent);
            buf.push('}');
        }
        Expr::DbgStmt {
            first,
            extra_args,
            continuation,
            pnc_style: _,
        } => {
            buf.indent(indent);
            buf.push_str("dbg");
            buf.spaces(1);
            fmt_expr(buf, indent, &first.value, Suffix::None)?;
            for arg in *extra_args {
                buf.push_str(",");
                buf.spaces(1);
                fmt_expr(buf, indent, &arg.value, Suffix::None)?;
            }
            buf.ensure_ends_with_newline();
            fmt_expr(buf, indent, &continuation.value, Suffix::None)?;
        }
        Expr::LowLevelTry(..) => todo!(),
        Expr::LowLevelDbg(..) => todo!(),
        Expr::Apply(func, args, _) => {
            fmt_expr(buf, indent, &func.value, Suffix::OpenRound)?;
            for arg in *args {
                // TODO: make the suffix depend on whether we're multiline
                fmt_expr(buf, indent, &arg.value, Suffix::Comma)?;
            }
            buf.indent(indent);
            buf.push(')');
        }
        Expr::PncApply(func, collection) => {
            fmt_expr(buf, indent, &func.value, Suffix::OpenRound)?;
            for arg in collection.iter() {
                // TODO: make the suffix depend on whether we're multiline
                fmt_expr(buf, indent, &arg.value, Suffix::Comma)?;
            }
            if !collection.final_comments().is_empty() {
                fmt_spaces(buf, collection.final_comments().iter(), indent);
            }
            buf.indent(indent);
            buf.push(')');
        }
        Expr::BinOps(expr_op_pairs, last_expr) => {
            for (expr, op) in *expr_op_pairs {
                fmt_expr(buf, indent, &expr.value, Suffix::None)?;
                buf.spaces(1);
                buf.indent(indent);
                push_op(buf, op.value);
                buf.spaces(1);
            }
            fmt_expr(buf, indent, &last_expr.value, Suffix::None)?;
        }
        Expr::UnaryOp(expr, op) => {
            buf.indent(indent);
            let ch = match op.value {
                UnaryOp::Negate => "-",
                UnaryOp::Not => "!",
            };
            buf.push_str(ch);
            fmt_expr(buf, indent, &expr.value, Suffix::None)?;
        }
        Expr::If {
            if_thens,
            final_else,
            indented_else: _,
        } => {
            for (i, (cond, then)) in if_thens.iter().enumerate() {
                buf.indent(indent);
                if i == 0 {
                    buf.push_str("if");
                } else {
                    buf.push_str("else if");
                }
                buf.spaces(1);
                fmt_expr(buf, indent, &cond.value, Suffix::None)?;
                buf.spaces(1);
                fmt_expr_ensure_block(buf, indent, &then.value)?;
            }
            buf.indent(indent);
            buf.spaces(1);
            buf.push_str("else");
            buf.spaces(1);
            fmt_expr_ensure_block(buf, indent, &final_else.value)?;
        }
        Expr::When(cond, when_branchs) => {
            buf.indent(indent);
            buf.push_str("when");
            buf.spaces(1);
            fmt_expr(buf, indent, &cond.value, Suffix::None)?;
            buf.indent(indent);
            buf.push('{');
            buf.ensure_ends_with_newline();
            for branch in when_branchs.iter() {
                for (i, pat) in branch.patterns.iter().enumerate() {
                    if i != 0 {
                        buf.indent(indent);
                        buf.push_str(" |");
                        buf.spaces(1);
                    }
                    fmt_pattern(buf, indent + 4, &pat.value, Suffix::None)?;
                }
                if let Some(guard) = &branch.guard {
                    buf.indent(indent);
                    buf.push_str(" if");
                    buf.spaces(1);
                    fmt_expr(buf, indent + 4, &guard.value, Suffix::None)?;
                }
                buf.indent(indent);
                buf.push_str("->");
                fmt_expr(buf, indent + 4, &branch.value.value, Suffix::None)?;
            }
            buf.indent(indent);
            buf.push('}');
        }
        Expr::Return(value, cont) => {
            buf.indent(indent);
            buf.push_str("return");
            buf.spaces(1);
            fmt_expr(buf, indent, &value.value, Suffix::None)?;
            buf.ensure_ends_with_newline();
            if let Some(cont) = cont {
                fmt_expr(buf, indent, &cont.value, Suffix::None)?;
            }
        }
        Expr::SpaceBefore(expr, comment_or_newlines) => {
            fmt_spaces(buf, comment_or_newlines.iter(), indent);
            fmt_expr(buf, indent, expr, suffix)?;
        }
        Expr::SpaceAfter(expr, comment_or_newlines) => {
            fmt_expr(buf, indent, expr, suffix)?;
            fmt_spaces(buf, comment_or_newlines.iter(), indent);
        }
        Expr::ParensAround(expr) => {
            buf.indent(indent);
            buf.push('(');
            fmt_expr(buf, indent, expr, Suffix::None)?;
            buf.indent(indent);
            buf.push(')');
        }
        Expr::MalformedIdent(_name, _bad_ident) => {
            return Err(MigrateError::MalformedIdentNotSupported)
        }
        Expr::PrecedenceConflict(_precedence_conflict) => {
            return Err(MigrateError::PrecedenceConflictNotSupported)
        }
        Expr::EmptyRecordBuilder(_inner) => todo!(),
        Expr::SingleFieldRecordBuilder(_loc) => todo!(),
        Expr::OptionalFieldInRecordBuilder(_loc, _loc1) => todo!(),
    }

    if !matches!(expr, Expr::SpaceAfter(..) | Expr::SpaceBefore(..)) {
        fmt_suffix(buf, indent, suffix);
    }

    Ok(())
}

pub fn fmt_expr_top_level(
    buf: &mut Buf<'_>,
    indent: u16,
    expr: &Expr<'_>,
) -> Result<(), MigrateError> {
    let expr = expr_lift_spaces(Parens::NotNeeded, buf.text.bump(), expr);
    if !expr.before.is_empty() {
        fmt_spaces(buf, expr.before.iter(), indent);
    }
    match expr.item {
        Expr::Defs(defs, final_expr) => {
            fmt_defs(buf, defs)?;
            fmt_expr(buf, indent, &final_expr.value, Suffix::None)?;
        }
        _ => {
            fmt_expr(buf, indent, &expr.item, Suffix::None)?;
        }
    }
    if !expr.after.is_empty() {
        fmt_spaces(buf, expr.after.iter(), indent);
    }
    Ok(())
}

fn fmt_expr_ensure_block(
    buf: &mut Buf<'_>,
    indent: u16,
    expr: &Expr<'_>,
) -> Result<(), MigrateError> {
    if matches!(expr, Expr::Defs(..)) {
        fmt_expr(buf, indent, expr, Suffix::None)?;
    } else {
        buf.indent(indent);
        buf.push('{');
        buf.ensure_ends_with_newline();
        fmt_expr(buf, indent + 4, expr, Suffix::None)?;
        buf.ensure_ends_with_newline();
        buf.indent(indent);
        buf.push('}');
    }
    Ok(())
}

fn fmt_suffix(buf: &mut Buf, indent: u16, suffix: Suffix) {
    buf.indent(indent);
    match suffix {
        Suffix::None => {}
        Suffix::Comma => buf.push(','),
        Suffix::OpenRound => buf.push('('),
        Suffix::Question => buf.push('?'),
    }
}

enum CollectionHead<'a> {
    Mapper(&'a Loc<Expr<'a>>),
    Update(&'a Loc<Expr<'a>>),
}

enum CollectionTail<'a> {
    Ext(TypeAnnotation<'a>),
}

fn fmt_collection<F: Fmt>(
    buf: &mut Buf<'_>,
    indent: u16,
    braces: Braces,
    head: Option<CollectionHead<'_>>,
    collection: Collection<'_, F>,
    tail: Option<CollectionTail<'_>>,
) -> Result<(), MigrateError> {
    buf.indent(indent);
    buf.push(braces.start());
    if collection.is_empty() && collection.final_comments().is_empty() && head.is_none() {
        buf.push(braces.end());
        return Ok(());
    }

    if let Some(head) = head {
        match head {
            CollectionHead::Mapper(mapper) => {
                fmt_expr(buf, indent, &mapper.value, Suffix::None)?;
                buf.indent(indent);
                buf.push_str("<-");
            }
            CollectionHead::Update(update) => {
                fmt_expr(buf, indent, &update.value, Suffix::None)?;
                buf.indent(indent);
                buf.push_str("&");
            }
        }
    }

    for item in collection.iter() {
        item.fmt(buf, indent + 4, Suffix::Comma)?;
    }

    if let Some(tail) = tail {
        match tail {
            CollectionTail::Ext(ext) => {
                buf.indent(indent);
                buf.push_str("..");
                ext.fmt(buf, indent, Suffix::None)?;
            }
        }
    }

    if !collection.final_comments().is_empty() {
        fmt_spaces(buf, collection.final_comments().iter(), indent);
    }

    buf.indent(indent);
    buf.push(braces.end());

    Ok(())
}
impl Fmt for TypeVar<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            TypeVar::Identifier(name) => {
                buf.indent(indent);
                buf.push_str(name);

                fmt_suffix(buf, indent, suffix);
            }
            TypeVar::SpaceBefore(type_var, comment_or_newlines) => {
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
                type_var.fmt(buf, indent, suffix)?;
            }
            TypeVar::SpaceAfter(type_var, comment_or_newlines) => {
                type_var.fmt(buf, indent, suffix)?;
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
            }
            TypeVar::Malformed(expr) => {
                fmt_expr(buf, indent, expr, suffix)?;
            }
        }
        Ok(())
    }
}

impl Fmt for TypeHeader<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        buf.indent(indent);
        buf.push_str(self.name.value);
        buf.push('(');
        for arg in self.vars {
            arg.fmt(buf, indent, Suffix::Comma)?;
        }
        buf.indent(indent);
        buf.push(')');
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for Tag<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            Tag::Apply { name, args } => {
                buf.indent(indent);
                buf.push_str(name.value);
                if !args.is_empty() {
                    buf.push('(');
                    for arg in args.iter() {
                        arg.fmt(buf, indent, Suffix::Comma)?;
                    }
                    buf.indent(indent);
                    buf.push(')');
                }

                fmt_suffix(buf, indent, suffix);
            }
            Tag::SpaceBefore(tag, comment_or_newlines) => {
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
                tag.fmt(buf, indent, suffix)?;
            }
            Tag::SpaceAfter(tag, comment_or_newlines) => {
                tag.fmt(buf, indent, suffix)?;
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
            }
        }
        Ok(())
    }
}

impl Fmt for TypeAnnotation<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            TypeAnnotation::Function(args, function_arrow, res) => {
                for arg in args.iter() {
                    arg.fmt(buf, indent, Suffix::Comma)?;
                }
                match function_arrow {
                    FunctionArrow::Pure => {
                        buf.push_str(" ->");
                    }
                    FunctionArrow::Effectful => {
                        buf.push_str(" =>");
                    }
                }
                buf.spaces(1);
                res.fmt(buf, indent, Suffix::None)?;
            }
            TypeAnnotation::Apply(module, func, locs) => {
                buf.indent(indent);
                if !module.is_empty() {
                    buf.push_str(module);
                    buf.push('.');
                }
                buf.push_str(func);
                if !locs.is_empty() {
                    buf.push('(');
                    for loc in locs.iter() {
                        loc.fmt(buf, indent, Suffix::Comma)?;
                    }
                    buf.indent(indent);
                    buf.push(')');
                }
            }
            TypeAnnotation::BoundVariable(name) => {
                buf.indent(indent);
                buf.push_str(name);
            }
            TypeAnnotation::As(lhs, comment_or_newlines, type_header) => {
                lhs.fmt(buf, indent, Suffix::None)?;
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
                buf.indent(indent);
                buf.push_str("as");
                buf.spaces(1);
                type_header.fmt(buf, indent, Suffix::None)?;
            }
            TypeAnnotation::Record { fields, ext } => {
                fmt_collection(
                    buf,
                    indent,
                    Braces::Curly,
                    None,
                    *fields,
                    ext.map(|ext| CollectionTail::Ext(ext.value)),
                )?;
            }
            TypeAnnotation::Tuple { elems, ext } => {
                fmt_collection(
                    buf,
                    indent,
                    Braces::Round,
                    None,
                    *elems,
                    ext.map(|ext| CollectionTail::Ext(ext.value)),
                )?;
            }
            TypeAnnotation::TagUnion { ext, tags } => {
                fmt_collection(
                    buf,
                    indent,
                    Braces::Square,
                    None,
                    *tags,
                    ext.map(|ext| CollectionTail::Ext(ext.value)),
                )?;
            }
            TypeAnnotation::Inferred => {
                buf.indent(indent);
                buf.push_str("_");
            }
            TypeAnnotation::Wildcard => {
                buf.indent(indent);
                buf.push_str("*");
            }
            TypeAnnotation::Where(left, clauses) => {
                left.fmt(buf, indent, Suffix::None)?;
                for clause in clauses.iter() {
                    buf.indent(indent);
                    buf.push_str(" where");
                    buf.spaces(1);
                    clause.value.fmt(buf, indent, Suffix::None)?;
                }
            }
            TypeAnnotation::SpaceBefore(type_annotation, comment_or_newlines) => {
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
                type_annotation.fmt(buf, indent, suffix)?;
            }
            TypeAnnotation::SpaceAfter(type_annotation, comment_or_newlines) => {
                type_annotation.fmt(buf, indent, suffix)?;
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
            }
            TypeAnnotation::Malformed(_) => todo!(),
        }

        if !matches!(
            self,
            TypeAnnotation::SpaceAfter(..) | TypeAnnotation::SpaceBefore(..)
        ) {
            fmt_suffix(buf, indent, suffix);
        }
        Ok(())
    }
}

impl<F: Fmt> Fmt for Spaced<'_, F> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            Spaced::Item(value) => value.fmt(buf, indent, suffix)?,
            Spaced::SpaceBefore(spaced, comment_or_newlines) => {
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
                spaced.fmt(buf, indent, suffix)?;
            }
            Spaced::SpaceAfter(spaced, comment_or_newlines) => {
                spaced.fmt(buf, indent, suffix)?;
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
            }
        }
        Ok(())
    }
}

impl Fmt for &str {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        buf.indent(indent);
        buf.push_str(self);
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for ImplementsClause<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        self.var.value.fmt(buf, indent, Suffix::None)?;
        buf.indent(indent);
        buf.push_str(" implements");
        buf.spaces(1);
        for (i, var) in self.abilities.iter().enumerate() {
            if i > 0 {
                buf.indent(indent);
                buf.spaces(1);
                buf.push_str("&");
                buf.spaces(1);
            }
            var.fmt(buf, indent, Suffix::None)?;
        }
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for ImplementsAbilities<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        if !self.before_implements_kw.is_empty() {
            buf.newline();
            buf.indent(indent);
            fmt_spaces(buf, self.before_implements_kw.iter(), indent);
        }
        buf.ensure_ends_with_newline();
        buf.indent(indent);
        buf.push_str(roc_parse::keyword::IMPLEMENTS);
        if !self.after_implements_kw.is_empty() {
            fmt_spaces(buf, self.after_implements_kw.iter(), indent);
        }
        buf.ensure_ends_with_whitespace();
        fmt_collection(buf, indent, Braces::Square, None, self.item.value, None)?;
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for AbilityImpls<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            AbilityImpls::AbilityImpls(impls) => {
                fmt_collection(buf, indent, Braces::Curly, None, *impls, None)?;
                fmt_suffix(buf, indent, suffix);
            }
            AbilityImpls::SpaceBefore(impls, spaces) => {
                fmt_spaces(buf, spaces.iter(), indent);
                impls.fmt(buf, indent, suffix)?;
            }
            AbilityImpls::SpaceAfter(impls, spaces) => {
                impls.fmt(buf, indent, suffix)?;
                fmt_spaces(buf, spaces.iter(), indent);
            }
        }
        Ok(())
    }
}

impl Fmt for ImplementsAbility<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            ImplementsAbility::ImplementsAbility { ability, impls } => {
                ability.fmt(buf, indent, Suffix::None)?;
                if let Some(impls) = impls {
                    buf.push_str(" implements");
                    buf.spaces(1);
                    impls.fmt(buf, indent, Suffix::None)?;
                }
                fmt_suffix(buf, indent, suffix);
            }
            ImplementsAbility::SpaceBefore(implements_ability, comment_or_newlines) => {
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
                implements_ability.fmt(buf, indent, suffix)?;
            }
            ImplementsAbility::SpaceAfter(implements_ability, comment_or_newlines) => {
                implements_ability.fmt(buf, indent, suffix)?;
                fmt_spaces(buf, comment_or_newlines.iter(), indent);
            }
        }
        Ok(())
    }
}
impl Fmt for TypeDef<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            TypeDef::Alias { header, ann } => {
                header.fmt(buf, indent, Suffix::None)?;
                buf.push_str(" :");
                buf.spaces(1);
                ann.fmt(buf, indent, Suffix::None)?;
            }
            TypeDef::Opaque {
                header,
                typ,
                derived,
            } => {
                header.fmt(buf, indent, Suffix::None)?;
                buf.push_str(" :=");
                buf.spaces(1);
                typ.fmt(buf, indent, Suffix::None)?;
                if let Some(derived) = derived {
                    buf.push_str(" implements");
                    buf.spaces(1);
                    derived.fmt(buf, indent, Suffix::None)?;
                }
            }
            TypeDef::Ability {
                header: _,
                loc_implements: _,
                members: _,
            } => return Err(MigrateError::AbilitiesNotSupported),
        }
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

#[derive(Debug)]
pub enum MigrateError {
    AbilitiesNotSupported,
    MalformedIdentNotSupported,
    MalformedPatternNotSupported,
    MalformedPatternIdentNotSupported,
    MalformedPatternAsExprNotSupported,
    PrecedenceConflictNotSupported,
}

impl Fmt for ValueDef<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            ValueDef::Annotation(pat, ty) => {
                fmt_pattern(buf, indent, &pat.value, Suffix::None)?;
                buf.indent(indent);
                buf.push_str(":");
                buf.spaces(1);
                ty.fmt(buf, indent, Suffix::None)?;
            }
            ValueDef::Body(pat, body) => {
                fmt_pattern(buf, indent, &pat.value, Suffix::None)?;
                buf.indent(indent);
                buf.push_str(" =");
                buf.spaces(1);
                body.fmt(buf, indent, Suffix::None)?;
            }
            ValueDef::AnnotatedBody {
                ann_pattern,
                ann_type,
                lines_between,
                body_pattern,
                body_expr,
            } => {
                ann_pattern.fmt(buf, indent, Suffix::None)?;
                buf.indent(indent);
                buf.push_str(":");
                buf.spaces(1);
                ann_type.fmt(buf, indent, Suffix::None)?;
                fmt_spaces(buf, lines_between.iter(), indent);
                body_pattern.fmt(buf, indent, Suffix::None)?;
                buf.indent(indent);
                buf.push_str(" =");
                buf.spaces(1);
                body_expr.fmt(buf, indent, Suffix::None)?;
            }
            ValueDef::Dbg {
                condition,
                preceding_comment: _,
            } => {
                buf.indent(indent);
                buf.push_str("dbg");
                buf.spaces(1);
                fmt_expr(buf, indent, &condition.value, Suffix::None)?;
            }
            ValueDef::Expect {
                condition,
                preceding_comment: _,
            } => {
                buf.indent(indent);
                buf.push_str("expect");
                buf.spaces(1);
                fmt_expr(buf, indent, &condition.value, Suffix::None)?;
            }
            ValueDef::ModuleImport(module_import) => {
                module_import.fmt(buf, indent, Suffix::None)?;
            }
            ValueDef::IngestedFileImport(ingested_file_import) => {
                ingested_file_import.fmt(buf, indent, Suffix::None)?;
            }
            ValueDef::Stmt(loc) => {
                fmt_expr(buf, indent, &loc.value, suffix)?;
            }
            ValueDef::StmtAfterExpr => todo!(),
        }
        Ok(())
    }
}

impl Fmt for IngestedFileImport<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        let Self {
            before_path,
            path,
            name,
            annotation,
        } = self;

        buf.indent(indent);
        buf.push_str("import");

        let indent = indent + 4;

        if !before_path.is_empty() {
            fmt_spaces(buf, before_path.iter(), indent);
        }
        fmt_str_literal(buf, path.value, indent);

        name.keyword.fmt(buf, indent, Suffix::None)?;
        buf.indent(indent);
        buf.push_str(name.item.value);

        if let Some(annotation) = annotation {
            annotation.fmt(buf, indent, Suffix::None)?;
        }
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for IngestedFileAnnotation<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        if !self.before_colon.is_empty() {
            fmt_spaces(buf, self.before_colon.iter(), indent);
        }
        buf.indent(indent);
        buf.push_str(":");
        buf.spaces(1);
        self.annotation.fmt(buf, indent, Suffix::None)?;
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for ImportedModuleName<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        buf.indent(indent);

        if let Some(package_shorthand) = self.package {
            buf.push_str(package_shorthand);
            buf.push_str(".");
        }

        self.name.fmt(buf, indent, Suffix::None)?;
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl<F: Fmt> Fmt for Spaces<'_, F> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        if !self.before.is_empty() {
            fmt_spaces(buf, self.before.iter(), indent);
        }
        self.item.fmt(buf, indent, Suffix::None)?;
        if !self.after.is_empty() {
            fmt_spaces(buf, self.after.iter(), indent);
        }
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl<F: Fmt> Fmt for SpacesBefore<'_, F> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        if !self.before.is_empty() {
            fmt_spaces(buf, self.before.iter(), indent);
        }
        self.item.fmt(buf, indent, suffix)?;
        Ok(())
    }
}

impl Fmt for ExposedName<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        buf.indent(indent);
        buf.push_str(self.as_str());
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for ModuleName<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        buf.indent(indent);
        buf.push_str(self.as_str());
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for ModuleImport<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        let Self {
            before_name,
            name,
            params,
            alias,
            exposed,
        } = self;

        buf.indent(indent);
        buf.push_str("import");

        if !before_name.is_empty() {
            fmt_spaces(buf, before_name.iter(), indent);
        }

        name.fmt(buf, indent, Suffix::None)?;
        if let Some(params) = params {
            params.fmt(buf, indent, Suffix::None)?;
        }
        if let Some(alias) = alias {
            alias.fmt(buf, indent, Suffix::None)?;
        }

        if let Some(exposed) = exposed {
            exposed.keyword.fmt(buf, indent, Suffix::None)?;
            fmt_collection(buf, indent, Braces::Square, None, exposed.item, None)?;
        }
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}
macro_rules! impl_fmt_for_keywords {
    ($($t:ty),*) => {
        $(
            impl Fmt for $t {
                fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
                    buf.indent(indent);
                    buf.push_str(<$t as Keyword>::KEYWORD);
                    fmt_suffix(buf, indent, suffix);
                    Ok(())
                }
            }
        )*
    };
}

impl_fmt_for_keywords!(ImportExposingKeyword, ImportAsKeyword);

impl Fmt for ImportAlias<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        buf.indent(indent);
        buf.push_str(self.as_str());
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for ModuleImportParams<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        if !self.before.is_empty() {
            fmt_spaces(buf, self.before.iter(), indent);
        }
        fmt_collection(buf, indent, Braces::Curly, None, self.params.value, None)?;
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl<K: Keyword, V: Fmt> Fmt for KeywordItem<'_, K, V> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        buf.indent(indent);
        buf.push_str(K::KEYWORD);
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for ModuleHeader<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        buf.indent(indent);
        buf.push_str("module");

        let mut indent = fmt_spaces_with_outdent(buf, self.after_keyword, 0);

        if let Some(params) = &self.params {
            if is_collection_multiline(&params.pattern.value) {
                indent = 4;
            }

            fmt_collection(buf, indent, Braces::Curly, None, params.pattern.value, None)?;

            indent = fmt_spaces_with_outdent(buf, params.before_arrow, indent);
            buf.push_str("->");
            indent = fmt_spaces_with_outdent(buf, params.after_arrow, indent);
        }

        fmt_collection(buf, indent, Braces::Square, None, self.exposes, None)?;
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for AppHeader<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        fmt_app_header(buf, self);
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for PackageHeader<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        fmt_package_header(buf, self);
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for PlatformHeader<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        fmt_platform_header(buf, self);
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for HostedHeader<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        fmt_hosted_header(buf, self);
        fmt_suffix(buf, indent, suffix);
        Ok(())
    }
}

impl Fmt for Header<'_> {
    fn fmt(&self, buf: &mut Buf, indent: u16, suffix: Suffix) -> Result<(), MigrateError> {
        match self {
            Header::Module(module_header) => module_header.fmt(buf, indent, suffix),
            Header::App(app_header) => app_header.fmt(buf, indent, suffix),
            Header::Package(package_header) => package_header.fmt(buf, indent, suffix),
            Header::Platform(platform_header) => platform_header.fmt(buf, indent, suffix),
            Header::Hosted(hosted_header) => hosted_header.fmt(buf, indent, suffix),
        }
    }
}

pub fn fmt_header(
    buf: &mut Buf<'_>,
    header: &SpacesBefore<'_, Header<'_>>,
) -> Result<(), MigrateError> {
    header.fmt(buf, 0, Suffix::None)
}

pub fn fmt_defs(buf: &mut Buf<'_>, defs: &Defs<'_>) -> Result<(), MigrateError> {
    for (index, def) in defs.defs().enumerate() {
        let spaces_before = &defs.spaces[defs.space_before[index].indices()];
        let spaces_after = &defs.spaces[defs.space_after[index].indices()];

        if !spaces_before.is_empty() {
            fmt_spaces(buf, spaces_before.iter(), 0);
        }

        match def {
            Ok(type_def) => type_def.fmt(buf, 0, Suffix::None)?,
            Err(value_def) => value_def.fmt(buf, 0, Suffix::None)?,
        }

        if !spaces_after.is_empty() {
            fmt_spaces(buf, spaces_after.iter(), 0);
        }
    }
    Ok(())
}
