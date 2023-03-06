use roc_parse::highlight::Token;
use roc_region::all::Loc;

pub fn highlight_roc_code(code: &str) -> String {
    let locations: Vec<Loc<Token>> = roc_parse::highlight::highlight(code);
    let mut buf: Vec<String> = Vec::new();
    let mut offset = 0;

    for location in locations {
        let current_text = &code[offset..location.byte_range().end];

        match location.value {
            Token::LineComment | Token::DocComment => {
                buf = push_html_span(buf, current_text, "comment");
            }
            Token::SingleQuote
            | Token::String
            | Token::UnicodeEscape
            | Token::EscapedChar
            | Token::Interpolated => {
                buf = push_html_span(buf, current_text, "str");
            }
            Token::Keyword => {
                buf = push_html_span(buf, current_text, "kw");
            }
            Token::Number => {
                buf = push_html_span(buf, current_text, "number");
            }
            Token::Pizza => {
                buf = push_html_span(buf, current_text, "pipe");
            }
            Token::Arrow | Token::Backpass => {
                buf = push_html_span(buf, current_text, "arrow");
            }
            Token::Bar => {
                buf = push_html_span(buf, current_text, "bar");
            }
            Token::Backslash => {
                buf = push_html_span(buf, current_text, "backslash");
            }
            Token::Comma => {
                buf = push_html_span(buf, current_text, "comma");
            }
            Token::QuestionMark => {
                buf = push_html_span(buf, current_text, "qmark");
            }
            Token::ColonEquals | Token::Colon => {
                buf = push_html_span(buf, current_text, "colon");
            }
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
            | Token::Plus
            | Token::And
            | Token::DoubleAnd
            | Token::Equals => {
                buf = push_html_span(buf, current_text, "op");
            }
            Token::Paren => {
                buf = push_html_span(buf, current_text, "paren");
            }
            Token::Bracket => {
                buf = push_html_span(buf, current_text, "bracket");
            }
            Token::Brace => {
                buf = push_html_span(buf, current_text, "brace");
            }
            Token::UpperIdent => {
                buf = push_html_span(buf, current_text, "upperident");
            }
            Token::LowerIdent => {
                buf = push_html_span(buf, current_text, "lowerident");
            }
            Token::Error | Token::Other => {
                buf = push_html(buf, current_text);
            }
        }

        offset = location.byte_range().end;
    }

    format!("<pre><samp>{}</samp></pre>", buf.join(""))
}

fn push_html_span(mut buf: Vec<String>, curr: &str, class: &str) -> Vec<String> {
    // html escape strings from source code
    let escaped = html_escape::encode_text(curr);

    buf.push(format!("<span class=\"{}\">{}</span>", class, escaped));

    buf
}

fn push_html(mut buf: Vec<String>, curr: &str) -> Vec<String> {
    // html escape strings from source code
    let escaped = html_escape::encode_text(curr);

    buf.push(format!("{}", escaped));

    buf
}
