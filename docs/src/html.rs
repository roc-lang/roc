use bumpalo::collections::String as BumpString;
use roc_code_markup::{markup::nodes::MarkupNode, slow_pool::SlowPool};

// determine appropriate css class for MarkupNode
pub fn mark_node_to_html<'a>(
    mark_node: &MarkupNode,
    mark_node_pool: &SlowPool,
    buf: &mut BumpString<'a>,
) {
    let mut additional_newlines = 0;

    match mark_node {
        MarkupNode::Nested {
            children_ids,
            newlines_at_end,
            ..
        } => {
            for &child_id in children_ids {
                mark_node_to_html(mark_node_pool.get(child_id), mark_node_pool, buf)
            }

            additional_newlines = *newlines_at_end;
        }
        MarkupNode::Text {
            content,
            syn_high_style,
            newlines_at_end,
            ..
        } => {
            use roc_code_markup::syntax_highlight::HighlightStyle::*;

            let css_class = match syn_high_style {
                Operator => "operator",
                Comma => "comma",
                String => "string",
                FunctionName => "function-name",
                FunctionArgName => "function-arg-name",
                Type => "type",
                Bracket => "bracket",
                Number => "number",
                PackageRelated => "package-related",
                Value => "value",
                RecordField => "recordfield",
                Import => "import",
                Provides => "provides",
                Blank => "blank",
            };

            write_html_to_buf(content, css_class, buf);

            additional_newlines = *newlines_at_end;
        }
        MarkupNode::Blank {
            newlines_at_end, ..
        } => {
            let mut content_str = " ".to_string();

            for _ in 0..*newlines_at_end {
                content_str.push('\n');
            }

            write_html_to_buf(&content_str, "blank", buf);

            additional_newlines = *newlines_at_end;
        }
        MarkupNode::Indent { .. } => {
            let content_str = mark_node.get_content();

            write_html_to_buf(&content_str, "indent", buf);
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
