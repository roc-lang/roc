use roc_parse::highlight::Token;

pub fn highlight_roc_code(code: &str) -> String {
    let buf = highlight(code);

    format!("<pre><samp>{}</samp></pre>", buf.join(""))
}

pub fn highlight_roc_code_inline(code: &str) -> String {
    let buf = highlight(code);

    format!("<code>{}</code>", buf.join(""))
}

pub fn highlight(code: &str) -> Vec<String> {
    let mut buf: Vec<String> = Vec::new();
    let mut offset = 0;

    // Sometimes code snippets start with "»" in order to show that they're in the repl.
    // Special-case that even though it's normally not a valid highlight.
    const REPL_PROMPT: &str = "»";

    let code = if let Some(stripped) = code.strip_prefix(REPL_PROMPT) {
        buf = push_html_span(buf, REPL_PROMPT, "kw");

        stripped
    } else {
        code
    };

    for location in roc_parse::highlight::highlight(code) {
        let current_text = &code[offset..location.byte_range().end];

        match location.value {
            // Comments `#` and Documentation comments `##`
            Token::LineComment | Token::DocComment => {
                buf = push_html_span(buf, current_text, "comment");
            }
            // Number, String, Tag, Type literals
            Token::SingleQuote
            | Token::String
            | Token::UnicodeEscape
            | Token::EscapedChar
            | Token::Interpolated
            | Token::Number => {
                buf = push_html_span(buf, current_text, "literal");
            }
            // Keywords and punctuation
            Token::Keyword
            | Token::Equals
            | Token::Backslash
            | Token::Pizza
            | Token::Arrow
            | Token::BackArrow
            | Token::ColonEquals
            | Token::Colon
            | Token::And
            | Token::QuestionMark => {
                buf = push_html_span(buf, current_text, "kw");
            }
            // Operators
            Token::Percent
            | Token::Caret
            | Token::Bang
            | Token::BangEquals
            | Token::Slash
            | Token::DoubleSlash
            | Token::Pipe
            | Token::GreaterThan
            | Token::GreaterThanEquals
            | Token::Minus
            | Token::LessThan
            | Token::LessThanEquals
            | Token::DoubleEquals
            | Token::DoubleBar
            | Token::Multiply
            | Token::Plus
            | Token::DoubleAnd => {
                buf = push_html_span(buf, current_text, "op");
            }
            // Delimieters
            Token::Paren
            | Token::Bracket
            | Token::Brace
            | Token::Comma
            | Token::Bar
            | Token::Decimal => {
                buf = push_html_span(buf, current_text, "delimiter");
            }
            // Types, Tags, and Modules
            Token::UpperIdent | Token::AtSign => {
                buf = push_html_span(buf, current_text, "upperident");
            }
            // Variables modules and field names
            Token::LowerIdent | Token::Underscore => {
                buf = push_html_span(buf, current_text, "lowerident");
            }
            // Anyting else that wasn't tokenised
            Token::Error | Token::Other => {
                buf = push_html(buf, current_text);
            }
        }

        offset = location.byte_range().end;
    }

    buf
}

fn push_html_span(mut buf: Vec<String>, curr: &str, class: &str) -> Vec<String> {
    // html escape strings from source code
    let escaped = html_escape::encode_text(curr);

    buf.push(format!("<span class=\"{class}\">{escaped}</span>"));

    buf
}

fn push_html(mut buf: Vec<String>, curr: &str) -> Vec<String> {
    // html escape strings from source code
    let escaped = html_escape::encode_text(curr);

    buf.push(format!("{escaped}"));

    buf
}
