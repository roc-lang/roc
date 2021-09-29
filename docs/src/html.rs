use roc_region::all::Located;

pub trait ToHtml<'a> {
    fn css_class(&self) -> Option<&'a str>;
    fn html_body(&self, buf: &mut bumpalo::collections::String<'a>);
    fn html(&self, buf: &mut bumpalo::collections::String<'a>) {
        let maybe_css_class = self.css_class();

        if let Some(css_class) = maybe_css_class {
            let opening_tag: String = ["<span class=\"syntax-", css_class, "\">"].concat();

            buf.push_str(opening_tag.as_str());
        }

        self.html_body(buf);

        if let Some(_) = maybe_css_class {
            buf.push_str("</span>");
        }
    }
}

impl<'a, T> ToHtml<'a> for Located<T>
where
    T: ToHtml<'a>,
{
    fn css_class(&self) -> Option<&'a str> {
        None
    }

    fn html_body(&self, buf: &mut bumpalo::collections::String<'a>) {
        let start_col = self.region.start_col as usize;

        let last_newline_to_end = buf.len() - buf.rfind("\n").unwrap_or_else(|| buf.len());

        if start_col > last_newline_to_end {
            let new_spaces = start_col - last_newline_to_end;
            buf.push_str((0..new_spaces).map(|_| " ").collect::<String>().as_str());
        };

        self.value.html(buf);
    }
}
