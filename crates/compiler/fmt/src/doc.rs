use std::fmt::Display;

use bumpalo::Bump;
use roc_parse::{
    ast::{Base, Expr},
    ident::Accessor,
};

use crate::Buf;

#[derive(Debug)]
pub enum Doc<'a> {
    Copy(&'a str),
    OptionalNewline,
    WhenMultiline(Box<Doc<'a>>),
    ForcedNewline,
    Space,
    Literal(&'static str),
    Comment(&'a str),
    Concat(Vec<Doc<'a>>),
    Group(Vec<Doc<'a>>),
    Indent(Box<Doc<'a>>),
}

enum Prec {
    TaskAwaitBang,
}

fn docify<'a>(expr: &Expr<'a>) -> Doc<'a> {
    match expr {
        Expr::Float(text) | Expr::Num(text) => Doc::Copy(text),
        Expr::NonBase10Int {
            string,
            base,
            is_negative,
        } => Doc::Concat(vec![
            Doc::Literal(if *is_negative { "-" } else { "" }),
            Doc::Literal(match base {
                Base::Hex => "0x",
                Base::Octal => "0o",
                Base::Binary => "0b",
                Base::Decimal => "",
            }),
            Doc::Copy(string),
        ]),

        Expr::Str(_) => todo!(),
        Expr::SingleQuote(_) => todo!(),

        Expr::RecordAccess(rec, field) | Expr::TupleAccess(rec, field) => {
            Doc::Concat(vec![docify(rec), Doc::Literal("."), Doc::Copy(field)])
        }
        Expr::AccessorFunction(Accessor::TupleIndex(name))
        | Expr::AccessorFunction(Accessor::RecordField(name)) => {
            Doc::Concat(vec![Doc::Literal("."), Doc::Copy(name)])
        }

        Expr::TaskAwaitBang(inner) => Doc::Concat(vec![
            docify_paren(Prec::TaskAwaitBang, inner),
            Doc::Literal(" !"),
        ]),

        Expr::List(items) => {
            let mut inner = Vec::with_capacity(items.len() * 2);

            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    inner.push(Doc::Literal(","));
                }
                inner.push(Doc::OptionalNewline);
                inner.push(docify(&item.value));
            }

            if !items.is_empty() {
                inner.push(Doc::WhenMultiline(Box::new(Doc::Literal(","))));
                inner.push(Doc::OptionalNewline);
            }

            let mut parts = Vec::with_capacity(4);
            parts.push(Doc::Literal("["));
            parts.push(Doc::Indent(Box::new(Doc::Concat(inner))));
            parts.push(Doc::Literal("]"));
            Doc::Group(parts)
        }

        Expr::RecordUpdate { update, fields } => todo!(),
        Expr::Record(_) => todo!(),
        Expr::Tuple(_) => todo!(),
        Expr::RecordBuilder(_) => todo!(),
        Expr::Var { module_name, ident } => todo!(),
        Expr::Underscore(_) => todo!(),
        Expr::Crash => todo!(),
        Expr::Tag(_) => todo!(),
        Expr::OpaqueRef(_) => todo!(),
        Expr::Closure(_, _) => todo!(),
        Expr::Defs(_, _) => todo!(),
        Expr::EmptyDefsFinal => todo!(),
        Expr::Backpassing(_, _, _) => todo!(),
        Expr::Expect(_, _) => todo!(),
        Expr::Dbg(_, _) => todo!(),
        Expr::LowLevelDbg(_, _, _) => todo!(),
        Expr::Apply(_, _, _) => todo!(),
        Expr::BinOps(_, _) => todo!(),
        Expr::UnaryOp(_, _) => todo!(),
        Expr::If(_, _) => todo!(),
        Expr::When(_, _) => todo!(),
        Expr::SpaceBefore(_, _) => todo!(),
        Expr::SpaceAfter(_, _) => todo!(),
        Expr::ParensAround(_) => todo!(),
        Expr::MalformedIdent(_, _) => todo!(),
        Expr::MalformedClosure => todo!(),
        Expr::MalformedSuffixed(_) => todo!(),
        Expr::PrecedenceConflict(_) => todo!(),
        Expr::MultipleRecordBuilders(_) => todo!(),
        Expr::UnappliedRecordBuilder(_) => todo!(),
    }
}

fn docify_paren<'a>(prec: Prec, expr: &Expr<'a>) -> Doc<'a> {
    todo!();
}

impl<'a> Doc<'a> {
    fn must_be_multiline(&self) -> bool {
        match self {
            Doc::OptionalNewline
            | Doc::Space
            | Doc::Copy(_)
            | Doc::Literal(_)
            | Doc::WhenMultiline(_) => false,
            Doc::ForcedNewline | Doc::Comment(_) => true,
            Doc::Concat(v) | Doc::Group(v) => v.iter().any(|d| d.must_be_multiline()),
            Doc::Indent(d) => d.must_be_multiline(),
        }
    }

    fn render(&self, max_width: usize) -> String {
        let arena = Bump::new();
        let mut buf = Buf::new_in(&arena);
        self.render_inner(0, max_width, true, &mut buf);
        buf.as_str().to_string()
    }

    fn render_inner(
        &self,
        indent: usize,
        max_width: usize,
        honor_newlines: bool,
        buf: &mut Buf<'_>,
    ) {
        match self {
            Doc::OptionalNewline => {
                if honor_newlines {
                    buf.newline();
                } else {
                    buf.spaces(1)
                }
            }
            Doc::WhenMultiline(s) => {
                if honor_newlines {
                    s.render_inner(indent, max_width, honor_newlines, buf);
                }
            }
            Doc::Space => buf.spaces(1),
            Doc::Copy(s) | Doc::Literal(s) => {
                buf.indent(indent as u16);
                buf.push_str(s)
            }
            Doc::ForcedNewline => {
                assert!(!honor_newlines);
                buf.newline()
            }
            Doc::Comment(s) => {
                assert!(!honor_newlines);
                buf.indent(indent as u16);
                buf.push_str(s)
            }
            Doc::Concat(v) => {
                for d in v {
                    d.render_inner(indent, max_width, honor_newlines, buf);
                }
            }
            Doc::Group(v) => {
                let mut honor_newlines = honor_newlines;
                if honor_newlines
                    && !self.must_be_multiline()
                    && self.width_without_newlines() <= max_width
                {
                    honor_newlines = false;
                }

                for d in v {
                    d.render_inner(indent, max_width, honor_newlines, buf);
                }
            }
            Doc::Indent(d) => {
                d.render_inner(indent + 4, max_width, honor_newlines, buf);
            }
        }
    }

    fn width_without_newlines(&self) -> usize {
        match self {
            Doc::OptionalNewline => 1,
            Doc::ForcedNewline => panic!("should not be called"),
            Doc::Space => 1,
            Doc::WhenMultiline(_) => 0,
            Doc::Copy(s) => s.len(),
            Doc::Literal(s) => s.len(),
            Doc::Comment(s) => s.len(),
            Doc::Concat(v) => v.iter().map(|d| d.width_without_newlines()).sum(),
            Doc::Group(v) => v.iter().map(|d| d.width_without_newlines()).sum(),
            Doc::Indent(d) => d.width_without_newlines(),
        }
    }
}

impl Display for Doc<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.render(10))
    }
}

#[test]
fn test_docify() {
    let arena = Bump::new();
    let expr = roc_parse::test_helpers::parse_expr_with(&arena, "[123, 456]").unwrap();
    let doc = docify(&expr);
    assert_eq!(doc.render(10), "[ 123, 456 ]");
    assert_eq!(doc.render(5), "[\n    123,\n    456,\n]");
}
