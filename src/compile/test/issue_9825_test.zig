//! Regression test for issue #9825.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 9825: List.map-built recursive refcounted tree lowers to LIR" {
    // Repro for https://github.com/roc-lang/roc/issues/9825.
    // This should build and render the mapped child list without tripping the
    // ARC borrow certifier.
    try expectLowersToLir(
        \\Attr := [Attr(Str, Str)]
        \\Node := [Text(Str), Element(Str, U64, List(Attr), List(Node))]
        \\
        \\node_size : Node -> U64
        \\node_size = |node|
        \\    match node {
        \\        Text(content) => Str.count_utf8_bytes(content)
        \\        Element(_, size, _, _) => size
        \\    }
        \\
        \\element : Str -> (List(Attr), List(Node) -> Node)
        \\element = |tag_name|
        \\    |attrs, children| {
        \\        with_tag = 2 * (3 + Str.count_utf8_bytes(tag_name))
        \\        total_size = List.fold(children, with_tag, |acc, child| acc + node_size(child))
        \\        Element(tag_name, total_size, attrs, children)
        \\    }
        \\
        \\text : Str -> Node
        \\text = |s| Text(s)
        \\
        \\class : Str -> Attr
        \\class = |v| Attr.Attr("class", v)
        \\
        \\render_attr : Str, Attr -> Str
        \\render_attr = |buffer, Attr.Attr(key, val)| "${buffer} ${key}=\"${val}\""
        \\
        \\render_help : Str, Node -> Str
        \\render_help = |buffer, node|
        \\    match node {
        \\        Text(content) => Str.concat(buffer, content)
        \\        Element(tag_name, _, attrs, children) => {
        \\            with_tag_name = "${buffer}<${tag_name}"
        \\            with_attrs = if List.is_empty(attrs) { with_tag_name } else { List.fold(attrs, "${with_tag_name} ", render_attr) }
        \\            with_tag = Str.concat(with_attrs, ">")
        \\            with_children = List.fold(children, with_tag, render_help)
        \\            "${with_children}</${tag_name}>"
        \\        }
        \\    }
        \\
        \\ul_ = element("ul")
        \\li_ = element("li")
        \\
        \\main! = |_args| {
        \\    children = List.map(["a"], |s| li_([class("n")], [text(s)]))
        \\    echo!(render_help("", ul_([], children)))
        \\    Ok({})
        \\}
    );
}
