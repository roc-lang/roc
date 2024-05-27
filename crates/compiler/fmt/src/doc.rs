use std::{collections::VecDeque, fmt::Display};

use bumpalo::Bump;
use roc_parse::{
    ast::{Base, Expr},
    ident::Accessor,
};

use crate::Buf;

pub struct Doc<'a> {
    nodes: Vec<Node<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NodeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NodeRange {
    begin: NodeId,
    end: NodeId,
}

impl NodeRange {
    fn contains(&self, other: &NodeRange) -> bool {
        self.begin <= other.begin && other.end <= self.end
    }
}

#[derive(Debug, Copy, Clone)]
enum Node<'a> {
    Copy(&'a str),
    OptionalNewline,
    WhenMultiline(&'static str),
    ForcedNewline,
    Space,
    Literal(&'static str),
    Comment(&'a str),
    Group(NodeRange),
    Indent(NodeRange),
}

impl<'a> Doc<'a> {
    fn new() -> Doc<'a> {
        Doc { nodes: Vec::new() }
    }

    fn fully_contained(&self, range: NodeRange) -> bool {
        for i in range.begin.0..range.end.0 {
            match &self.nodes[i] {
                Node::Group(r) | Node::Indent(r) => {
                    if !range.contains(r) {
                        return false;
                    }
                }
                _ => {}
            }
        }
        true
    }

    fn push(&mut self, node: Node<'a>) -> NodeRange {
        let a = self.nodes.len();
        self.nodes.push(node);
        NodeRange {
            begin: NodeId(a),
            end: NodeId(a + 1),
        }
    }

    fn copy(&mut self, text: &'a str) -> NodeRange {
        self.push(Node::Copy(text))
    }

    fn literal(&mut self, text: &'static str) -> NodeRange {
        self.push(Node::Literal(text))
    }

    fn begin(&mut self) -> NodeId {
        NodeId(self.nodes.len())
    }

    fn indent_to(&mut self, begin: NodeId) -> NodeRange {
        let end = self.nodes.len();
        self.push(Node::Indent(NodeRange {
            begin,
            end: NodeId(end),
        }));
        let end = NodeId(self.nodes.len());
        let res = NodeRange { begin, end };
        debug_assert!(self.fully_contained(res));
        res
    }

    fn group_to(&mut self, begin: NodeId) -> NodeRange {
        let end = self.nodes.len();
        self.push(Node::Group(NodeRange {
            begin,
            end: NodeId(end),
        }));
        let end = NodeId(self.nodes.len());
        let res = NodeRange { begin, end };
        debug_assert!(self.fully_contained(res));
        res
    }

    fn to(&self, begin: NodeId) -> NodeRange {
        NodeRange {
            begin,
            end: NodeId(self.nodes.len()),
        }
    }
}

// #[derive(Debug)]
// pub enum Doc<'a> {
//     Copy(&'a str),
//     OptionalNewline,
//     WhenMultiline(Box<Doc<'a>>),
//     ForcedNewline,
//     Space,
//     Literal(&'static str),
//     Comment(&'a str),
//     Concat(Vec<Doc<'a>>),
//     Group(Vec<Doc<'a>>),
//     Indent(Box<Doc<'a>>),
// }

enum Prec {
    TaskAwaitBang,
}

fn docify<'a>(expr: &Expr<'a>, doc: &mut Doc<'a>) -> NodeRange {
    match expr {
        Expr::Float(text) | Expr::Num(text) => doc.copy(text),
        Expr::NonBase10Int {
            string,
            base,
            is_negative,
        } => {
            // Doc::Concat(vec![
            //     Doc::Literal(if *is_negative { "-" } else { "" }),
            //     Doc::Literal(match base {
            //         Base::Hex => "0x",
            //         Base::Octal => "0o",
            //         Base::Binary => "0b",
            //         Base::Decimal => "",
            //     }),
            //     Doc::Copy(string),
            // ])

            let begin = doc.begin();
            doc.push(Node::Literal(if *is_negative { "-" } else { "" }));
            doc.push(Node::Literal(match base {
                Base::Hex => "0x",
                Base::Octal => "0o",
                Base::Binary => "0b",
                Base::Decimal => "",
            }));
            doc.push(Node::Copy(string));
            doc.to(begin)
        }

        Expr::Str(_) => todo!(),
        Expr::SingleQuote(_) => todo!(),

        Expr::RecordAccess(rec, field) | Expr::TupleAccess(rec, field) => {
            // Doc::Concat(vec![docify(rec), Doc::Literal("."), Doc::Copy(field)])
            let begin = doc.begin();
            let rec = docify(rec, doc);
            doc.push(Node::Literal("."));
            let field = doc.copy(field);
            doc.to(begin)
        }
        Expr::AccessorFunction(Accessor::TupleIndex(name))
        | Expr::AccessorFunction(Accessor::RecordField(name)) => {
            // Doc::Concat(vec![Doc::Literal("."), Doc::Copy(name)])
            let begin = doc.begin();
            doc.push(Node::Literal("."));
            let name = doc.copy(name);
            doc.to(begin)
        }

        Expr::TaskAwaitBang(inner) => {
            // Doc::Concat(vec![
            //     docify_paren(Prec::TaskAwaitBang, inner),
            //     Doc::Literal(" !"),
            // ])

            let begin = doc.begin();
            let inner = docify(inner, doc);
            doc.push(Node::Literal(" !"));
            doc.to(begin)
        }

        Expr::List(items) => {
            // let mut inner = Vec::with_capacity(items.len() * 2);

            // for (i, item) in items.iter().enumerate() {
            //     if i > 0 {
            //         inner.push(Doc::Literal(","));
            //     }
            //     inner.push(Doc::OptionalNewline);
            //     inner.push(docify(&item.value));
            // }

            // if !items.is_empty() {
            //     inner.push(Doc::WhenMultiline(Box::new(Doc::Literal(","))));
            //     inner.push(Doc::OptionalNewline);
            // }

            // let mut parts = Vec::with_capacity(4);
            // parts.push(Doc::Literal("["));
            // parts.push(Doc::Indent(Box::new(Doc::Concat(inner))));
            // parts.push(Doc::Literal("]"));
            // Doc::Group(parts)

            let begin = doc.begin();
            doc.push(Node::Literal("["));

            let begin_indent = doc.begin();

            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    doc.push(Node::Literal(","));
                }
                doc.push(Node::OptionalNewline);
                docify(&item.value, doc);
            }

            if !items.is_empty() {
                doc.push(Node::WhenMultiline(","));
                doc.push(Node::OptionalNewline);
            }

            doc.indent_to(begin_indent);

            doc.push(Node::Literal("]"));
            doc.group_to(begin)
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
    fn bubble_up<T: Copy>(
        &self,
        map: impl Fn(usize, Node) -> T,
        aggregate: impl Fn(T, T) -> T,
    ) -> Vec<T> {
        let mut res = Vec::with_capacity(self.nodes.len());
        let mut stack = Vec::<(usize, T)>::new(); // todo: split into two parallel stacks

        for (i, node) in self.nodes.iter().enumerate() {
            let mut t = map(i, *node);

            match node {
                Node::Copy(_)
                | Node::Literal(_)
                | Node::Space
                | Node::OptionalNewline
                | Node::WhenMultiline(_)
                | Node::ForcedNewline
                | Node::Comment(_) => {}
                Node::Group(range) | Node::Indent(range) => {
                    while let Some((i, v)) = stack.pop() {
                        if i < range.begin.0 {
                            stack.push((i, v));
                            break;
                        }
                        t = aggregate(t, v);
                    }
                }
            };

            res.push(t);
            stack.push((i, t));
        }

        res
    }

    fn compute_must_be_multiline(&self) -> Vec<bool> {
        self.bubble_up(
            |_, node| match node {
                Node::Copy(_)
                | Node::Literal(_)
                | Node::Space
                | Node::OptionalNewline
                | Node::WhenMultiline(_)
                | Node::Group(_)
                | Node::Indent(_) => false,
                Node::ForcedNewline | Node::Comment(_) => true,
            },
            |a, b| a | b,
        )
    }

    fn compute_width_without_newlines(&self) -> Vec<usize> {
        self.bubble_up(
            |_, node| match node {
                Node::Copy(s) | Node::Literal(s) => s.len(),
                Node::Space | Node::OptionalNewline => 1,
                Node::WhenMultiline(_) => 0,
                Node::ForcedNewline => 0,
                Node::Comment(s) => s.len(),
                Node::Group(_) | Node::Indent(_) => 0,
            },
            |a, b| a + b,
        )
    }

    fn compute_indents(&self) -> VecDeque<u16> {
        let mut res = VecDeque::with_capacity(self.nodes.len());
        let mut stack = Vec::<usize>::new();

        for (i, node) in self.nodes.iter().enumerate().rev() {
            while let Some(top) = stack.pop() {
                if top <= i {
                    stack.push(top);
                    break;
                }
            }
            res.push_front(stack.len() as u16);
            if let Node::Indent(range) = node {
                stack.push(range.begin.0);
            }
        }

        res
    }

    fn compute_is_multiline(
        &self,
        must_be_multiline: &[bool],
        width_without_newlines: &[usize],
        _indents: &VecDeque<u16>, // TODO: use this in computing width?
        max_width: usize,
    ) -> Vec<bool> {
        self.bubble_up(
            |i, _| must_be_multiline[i] || width_without_newlines[i] > max_width,
            |a, b| a | b,
        )
    }

    fn compute_honor_newlines(&self, is_multiline: &[bool]) -> VecDeque<bool> {
        let mut stack = Vec::<(usize, bool)>::new();
        let mut res = VecDeque::with_capacity(self.nodes.len());
        let mut honor_newlines = false;

        for (i, node) in self.nodes.iter().enumerate().rev() {
            while let Some((top, hn)) = stack.pop() {
                if top <= i {
                    stack.push((top, hn));
                    break;
                }
                honor_newlines = hn;
            }
            res.push_front(honor_newlines);
            if let Node::Group(range) = node {
                stack.push((range.begin.0, is_multiline[i]));
                honor_newlines = is_multiline[i];
            }
        }

        res
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
        let must_be_multiline = self.compute_must_be_multiline();
        let width_without_newlines = self.compute_width_without_newlines();
        let indents = self.compute_indents();
        let is_multiline = self.compute_is_multiline(
            &must_be_multiline,
            &width_without_newlines,
            &indents,
            max_width,
        );
        let honor_newlines = self.compute_honor_newlines(&is_multiline);

        for i in 0..must_be_multiline.len() {
            // debug vis - print one per line, in columns (must_be_multiline, width_without_newlines, is_multiline, indent, node)
            println!(
                "{:5} {:5} {:5} {:5} {:5} {:?}",
                must_be_multiline[i],
                width_without_newlines[i],
                is_multiline[i],
                indents[i],
                honor_newlines[i],
                self.nodes[i]
            );
        }

        for ((node, honor_newlines), indent) in self
            .nodes
            .iter()
            .zip(honor_newlines.into_iter())
            .zip(indents.into_iter())
        {
            let indent = indent * 4;
            match node {
                Node::OptionalNewline => {
                    if honor_newlines {
                        buf.newline();
                    } else {
                        buf.spaces(1)
                    }
                }
                Node::WhenMultiline(s) => {
                    if honor_newlines {
                        buf.indent(indent);
                        buf.push_str(s)
                    }
                }
                Node::Space => buf.spaces(1),
                Node::Copy(s) | Node::Literal(s) => {
                    buf.indent(indent);
                    buf.push_str(s)
                }
                Node::ForcedNewline => {
                    assert!(!honor_newlines);
                    buf.newline()
                }
                Node::Comment(s) => {
                    buf.indent(indent);
                    buf.push_str(s)
                }
                Node::Group(_) | Node::Indent(_) => {}
            }
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
    let mut doc = Doc::new();
    docify(&expr, &mut doc);
    assert_eq!(doc.render(10), "[ 123, 456 ]");
    assert_eq!(doc.render(5), "[\n    123,\n    456,\n]");
}
