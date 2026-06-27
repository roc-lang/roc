# Repro: ARC borrow certifier ICE ("use of unbound refcounted local") when a
# function builds and consumes a moderately large / deeply nested value made of
# refcounted data (here: an HTML-document-shaped recursive `Node` tree rendered
# to a `Str`).
#
# - `roc check` reports no errors.
# - `roc build` panics in src/lir/arc_certify.zig:
#     ARC borrow certifier: proc=0 stmt=995: use of unbound refcounted local 150
#
# The same file with a SMALL tree (e.g. `div_([class("m")], [text("x")])`)
# compiles and runs fine, so the trigger is the cumulative size/nesting of the
# tree built in `main!` (proc=0), not any single construct. Reduced shapes
# (flat width up to 8, a single depth-6 chain) do NOT trigger it; the mixed
# head+body document below does.
app [main!] { pf: platform "./platform/main.roc" }
import pf.Stdout

Attr := [Attr(Str, Str)]
Node := [Text(Str), Element(Str, U64, List(Attr), List(Node)), UnclosedElem(Str, U64, List(Attr))]

node_size : Node -> U64
node_size = |node|
    match node {
        Text(content) => Str.count_utf8_bytes(content)
        Element(_, size, _, _) => size
        UnclosedElem(_, size, _) => size
    }

element : Str -> (List(Attr), List(Node) -> Node)
element = |tag_name|
    |attrs, children| {
        with_tag = 2 * (3 + Str.count_utf8_bytes(tag_name))
        with_attrs = List.fold(attrs, with_tag, |acc, Attr.Attr(name, val)|
            acc + Str.count_utf8_bytes(name) + Str.count_utf8_bytes(val) + 4)
        total_size = List.fold(children, with_attrs, |acc, child| acc + node_size(child))
        Element(tag_name, total_size, attrs, children)
    }

unclosed_elem : Str -> (List(Attr) -> Node)
unclosed_elem = |tag_name|
    |attrs| {
        with_tag = 2 * (3 + Str.count_utf8_bytes(tag_name))
        total_size = List.fold(attrs, with_tag, |acc, Attr.Attr(name, val)|
            acc + Str.count_utf8_bytes(name) + Str.count_utf8_bytes(val) + 4)
        UnclosedElem(tag_name, total_size, attrs)
    }

text : Str -> Node
text = |s| Text(s)
class : Str -> Attr
class = |v| Attr.Attr("class", v)

render_attr : Str, Attr -> Str
render_attr = |buffer, Attr.Attr(key, val)| "${buffer} ${key}=\"${val}\""

render_help : Str, Node -> Str
render_help = |buffer, node|
    match node {
        Text(content) => Str.concat(buffer, content)
        Element(tag_name, _, attrs, children) => {
            with_tag_name = "${buffer}<${tag_name}"
            with_attrs = if List.is_empty(attrs) { with_tag_name } else { List.fold(attrs, with_tag_name, render_attr) }
            with_tag = Str.concat(with_attrs, ">")
            with_children = List.fold(children, with_tag, render_help)
            "${with_children}</${tag_name}>"
        }
        UnclosedElem(tag_name, _, attrs) =>
            if List.is_empty(attrs) { "${buffer}<${tag_name}>" } else {
                folded = List.fold(attrs, "${buffer}<${tag_name}", render_attr)
                Str.concat(folded, ">")
            }
    }

render : Node -> Str
render = |node| {
    buffer = Str.reserve("<!DOCTYPE html>", node_size(node))
    render_help(buffer, node)
}

div_ = element("div")
meta_ = unclosed_elem("meta")
title_ = element("title")
link_ = unclosed_elem("link")
html_ = element("html")
head_ = element("head")
body_ = element("body")
ul_ = element("ul")
li_ = element("li")
a_ = element("a")

main! = || {
    tree = html_([class("en")], [
        head_([], [
            meta_([class("ct")]),
            title_([], [text("T")]),
            link_([class("ss")]),
        ]),
        body_([], [
            div_([class("main")], [
                div_([class("navbar")], [
                    ul_([], [
                        li_([class("nav-link")], [a_([class("x")], [text("X")])]),
                        li_([class("nav-link")], [a_([class("y")], [text("Y")])]),
                    ]),
                ]),
                div_([class("article")], [text("body")]),
            ]),
        ]),
    ])
    Stdout.line!(render(tree))
}
