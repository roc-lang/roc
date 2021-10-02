use roc_code_markup::{markup::nodes::MarkupNode, slow_pool::SlowPool};
use roc_region::all::Located;
use bumpalo::{collections::String as BumpString};

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


// determine appropriate css class for MarkupNode
pub fn mark_node_to_html<'a>(mark_node: &MarkupNode, mark_node_pool: &SlowPool, buf: &mut BumpString<'a>) {
    let additional_newlines:usize;
    
    match mark_node {
        MarkupNode::Nested { children_ids, newlines_at_end, .. } => {
            for &child_id in children_ids {
                mark_node_to_html(
                    mark_node_pool.get(child_id),
                    mark_node_pool,
                    buf
                )
            }

            additional_newlines = *newlines_at_end;
        }
        MarkupNode::Text { content, syn_high_style, newlines_at_end, .. } => {
            use roc_code_markup::syntax_highlight::HighlightStyle::*;

            let css_class = match syn_high_style {
                Operator => "operator",
                Comma => "comma",
                String => "string",
                FunctionName => "function_name",
                Type => "type",
                Bracket => "bracket",
                Number => "number",
                PackageRelated => "package-related",
                Variable => "variable",
                RecordField => "recordfield",
                Import => "import",
                Provides => "provides",
                Blank => "blank",
            };

            write_html_to_buf(content, css_class, buf);

            additional_newlines = *newlines_at_end;
        }
        MarkupNode::Blank { newlines_at_end, .. } => {
            let mut content_str = " ".to_string();

            for _ in 0..*newlines_at_end {
                content_str.push('\n');
            }

            write_html_to_buf(&content_str, "blank", buf);

            additional_newlines = *newlines_at_end;
        }
    }

    for _ in 0..additional_newlines {
        buf.push('\n')
    }
    
}

fn write_html_to_buf<'a>(content: &str, css_class: &'static str, buf: &mut BumpString<'a>) {
    
    let opening_tag: String = ["<span class=\"syntax-", css_class, "\">"].concat();

    buf.push_str(opening_tag.as_str());
    
    buf.push_str(content);

    buf.push_str("</span>");
}

