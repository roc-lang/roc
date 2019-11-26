use fmt::expr::fmt_expr;
use fmt::pattern::fmt_pattern;
use fmt::spaces::fmt_spaces;

use bumpalo::collections::String;
use parse::ast::Def;

pub fn fmt_def<'a>(buf: &mut String<'a>, def: &'a Def<'a>, indent: u16) {
    match def {
        Def::Annotation(_, _) => panic!("TODO have format_def support Annotation"),
        Def::Body(loc_pattern, loc_expr) => {
            fmt_pattern(buf, &loc_pattern.value, indent, true);
            buf.push_str(" = ");
            fmt_expr(buf, &loc_expr.value, indent, false);
        }
        Def::CustomType(_, _) => panic!("TODO have format_def support CustomType"),
        Def::TypeAlias(_, _) => panic!("TODO have format_def support TypeAlias"),
        Def::SpaceBefore(sub_def, spaces) => {
            fmt_spaces(buf, spaces.iter(), indent);
            fmt_def(buf, sub_def, indent);
        }
        Def::SpaceAfter(sub_def, spaces) => {
            fmt_def(buf, sub_def, indent);

            fmt_spaces(buf, spaces.iter(), indent);
        }
    }
}
