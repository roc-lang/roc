use crate::html::ToHtml;

struct EqualSign;

impl<'a> ToHtml<'a> for EqualSign {
    fn css_class(&self) -> Option<&'a str> {
        Some("operator")
    }
    fn html_body(&self, buf: &mut bumpalo::collections::String<'a>) {
        buf.push_str(" = ")
    }
}
