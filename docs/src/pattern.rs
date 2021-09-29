use crate::html::ToHtml;
use roc_parse::ast::Pattern;

impl<'a> ToHtml<'a> for Pattern<'a> {
    fn css_class(&self) -> Option<&'a str> {
        match self {
            // Pattern::Identifier(_) => {}
            // Pattern::GlobalTag(_) => {}
            // Pattern::PrivateTag(_) => {}
            // Pattern::Apply(_, _) => {}
            // Pattern::RecordDestructure(_) => {}
            // Pattern::RequiredField(_, _) => {}
            // Pattern::OptionalField(_, _) => {}
            // Pattern::NumLiteral(_) => {}
            // Pattern::NonBase10Literal { .. } => {}
            // Pattern::FloatLiteral(_) => {}
            // Pattern::StrLiteral(_) => {}
            // Pattern::Underscore(_) => {}
            // Pattern::SpaceBefore(_, _) => {}
            // Pattern::SpaceAfter(_, _) => {}
            // Pattern::Malformed(_) => {}
            // Pattern::MalformedIdent(_, _) => {}
            // Pattern::QualifiedIdentifier { .. } => {}
            _ => None,
        }
    }

    fn html_body(&self, buf: &mut bumpalo::collections::String<'a>) {
        match self {
            Pattern::Identifier(str) => {
                buf.push_str(str);
            }
            // Pattern::GlobalTag(_) => {}
            // Pattern::PrivateTag(_) => {}
            // Pattern::Apply(_, _) => {}
            // Pattern::RecordDestructure(_) => {}
            // Pattern::RequiredField(_, _) => {}
            // Pattern::OptionalField(_, _) => {}
            // Pattern::NumLiteral(_) => {}
            // Pattern::NonBase10Literal { .. } => {}
            // Pattern::FloatLiteral(_) => {}
            // Pattern::StrLiteral(_) => {}
            // Pattern::Underscore(_) => {}
            // Pattern::SpaceBefore(_, _) => {}
            // Pattern::SpaceAfter(_, _) => {}
            // Pattern::Malformed(_) => {}
            // Pattern::MalformedIdent(_, _) => {}
            // Pattern::QualifiedIdentifier { .. } => {}
            _ => {}
        }
    }
}
