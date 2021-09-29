use crate::html::ToHtml;
use roc_parse::ast::Def;

impl<'a> ToHtml<'a> for Def<'a> {
    fn css_class(&self) -> Option<&'a str> {
        match self {
            // Def::Annotation(_, _) => {}
            // Def::Alias { .. } => {}
            Def::Body(_, _) => None,
            // Def::AnnotatedBody { .. } => {}
            // Def::Expect(_) => {}
            Def::SpaceBefore(_, _) => None,
            Def::SpaceAfter(_, _) => None,
            // Def::NotYetImplemented(_) => {}
            _ => None,
        }
    }

    fn html_body(&self, buf: &mut bumpalo::collections::String<'a>) {
        match self {
            // Def::Annotation(_, _) => {}
            // Def::Alias { .. } => {}
            Def::Body(pattern, expr) => {
                pattern.html(buf);
                EqualSign.html(buf);
                expr.html(buf);
            }
            // Def::AnnotatedBody { .. } => {}
            // Def::Expect(_) => {}
            Def::SpaceBefore(sub_def, spaces) => {
                for space in spaces.iter() {
                    space.html(buf);
                }
                sub_def.html(buf);
            }
            Def::SpaceAfter(sub_def, spaces) => {
                sub_def.html(buf);
                for space in spaces.iter() {
                    space.html(buf);
                }
            }
            // Def::NotYetImplemented(_) => {}
            _ => {}
        }
    }
}

struct EqualSign;

impl<'a> ToHtml<'a> for EqualSign {
    fn css_class(&self) -> Option<&'a str> {
        Some("operator")
    }
    fn html_body(&self, buf: &mut bumpalo::collections::String<'a>) {
        buf.push_str(" = ")
    }
}
