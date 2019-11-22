use parse::ast::{AssignedField, Expr, Pattern};

pub fn is_multiline_expr<'a>(expr: &'a Expr<'a>) -> bool {
    use parse::ast::Expr::*;
    // TODO cache these answers using a Map<Pointer, bool>, so
    // we don't have to traverse subexpressions repeatedly

    match expr {
        // Return whether these spaces contain any Newlines
        SpaceBefore(_, spaces) | SpaceAfter(_, spaces) => {
            spaces.iter().any(|space| space.contains_newline())
        }

        // These expressions never have newlines
        Float(_)
        | Int(_)
        | HexInt(_)
        | OctalInt(_)
        | BinaryInt(_)
        | Str(_)
        | Field(_, _)
        | QualifiedField(_, _)
        | AccessorFunction(_)
        | Var(_, _)
        | MalformedIdent(_)
        | MalformedClosure
        | Variant(_, _) => false,

        // These expressions always have newlines
        Defs(_, _) | Case(_, _) => true,

        List(elems) => elems
            .iter()
            .any(|loc_expr| is_multiline_expr(&loc_expr.value)),

        BlockStr(lines) => lines.len() > 1,
        Apply(loc_expr, args, _) => {
            is_multiline_expr(&loc_expr.value)
                || args.iter().any(|loc_arg| is_multiline_expr(&loc_arg.value))
        }

        If((loc_cond, loc_if_true, loc_if_false)) => {
            is_multiline_expr(&loc_cond.value)
                || is_multiline_expr(&loc_if_true.value)
                || is_multiline_expr(&loc_if_false.value)
        }

        BinOp((loc_left, _, loc_right)) => {
            is_multiline_expr(&loc_left.value) || is_multiline_expr(&loc_right.value)
        }

        UnaryOp(loc_subexpr, _) | PrecedenceConflict(_, _, loc_subexpr) => {
            is_multiline_expr(&loc_subexpr.value)
        }

        ParensAround(subexpr) => is_multiline_expr(&subexpr),

        Closure(loc_patterns, loc_body) => {
            // check the body first because it's more likely to be multiline
            is_multiline_expr(&loc_body.value)
                || loc_patterns
                    .iter()
                    .any(|loc_pattern| is_multiline_pattern(&loc_pattern.value))
        }

        Record(loc_fields) => loc_fields
            .iter()
            .any(|loc_field| is_multiline_field(&loc_field.value)),
    }
}

pub fn is_multiline_field<'a, Val>(field: &'a AssignedField<'a, Val>) -> bool {
    use self::AssignedField::*;

    match field {
        LabeledValue(_, spaces, _) => !spaces.is_empty(),
        LabelOnly(_) => false,
        AssignedField::SpaceBefore(_, _) | AssignedField::SpaceAfter(_, _) => true,
        Malformed(text) => text.chars().any(|c| c == '\n'),
    }
}

pub fn is_multiline_pattern<'a>(_pattern: &'a Pattern<'a>) -> bool {
    panic!("TODO return iff there are any newlines")
}
