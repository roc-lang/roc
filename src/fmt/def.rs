use crate::fmt::expr::fmt_expr;
use crate::fmt::pattern::fmt_pattern;
use crate::fmt::spaces::fmt_spaces;
use crate::parse::ast::Def;
use bumpalo::collections::String;

pub fn fmt_def<'a>(buf: &mut String<'a>, def: &'a Def<'a>, indent: u16) {
    use crate::parse::ast::Def::*;

    match def {
        Annotation(_, _) => panic!("TODO have format_def support Annotation"),
        Body(loc_pattern, loc_expr) => {
            fmt_pattern(buf, &loc_pattern.value, indent, true);
            buf.push_str(" = ");
            fmt_expr(buf, &loc_expr.value, indent, false);
        }
        SpaceBefore(sub_def, spaces) => {
            fmt_spaces(buf, spaces.iter(), indent);
            fmt_def(buf, sub_def, indent);
        }
        SpaceAfter(sub_def, spaces) => {
            fmt_def(buf, sub_def, indent);

            fmt_spaces(buf, spaces.iter(), indent);
        }
        Nested(def) => fmt_def(buf, def, indent),
    }
}
