app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stdin

# A record that is a payload of the recursive type its own field holds stores
# that field in a recursion box. These functions are generic in the record's
# fields, so only the descriptor each call supplies says whether a field they
# move out leaves a box behind. A tag's only payload is stored the same way,
# and `collect` is generic in it.

Tree := [Node({ tag : Tree, items : List(Tree) }), Leaf(Str)]

push = |{ tag, items }, x| { tag, items: items.append(x) }

only_items = |{ tag: _, items }| items

only_tag = |{ tag, items: _ }| tag

Nest := [Deep([Wrap(Nest), Empty]), Flat(Str)]

collect = |value, found| match value {
    Deep(inner) => found.append(inner)
    Flat(_) => found
}

label : Tree -> Str
label = |tree| match tree {
    Leaf(text) => text
    Node({ tag, items }) => "${label(tag)}/${items.len().to_str()}"
}

main! = || {
    input = Stdin.line!()
    leaf = |suffix| Tree.Leaf("${input} ${suffix}")

    # The record dies in the call, which holds the only reference to its box.
    grown = push({ tag: leaf("tag"), items: [leaf("first")] }, leaf("second"))
    Stdout.line!("unique: ${label(grown.tag)} ${grown.items.len().to_str()}")

    # The record is read again afterwards, so its box has another holder.
    kept = { tag: leaf("kept tag"), items: [leaf("kept item")] }
    copied = push(kept, leaf("added"))
    Stdout.line!("shared: ${label(kept.tag)} ${kept.items.len().to_str()} ${label(copied.tag)} ${copied.items.len().to_str()}")

    # A field the call drops is released along with its box.
    Stdout.line!("items only: ${only_items({ tag: leaf("dropped tag"), items: [leaf("a"), leaf("b")] }).len().to_str()}")
    Stdout.line!("tag only: ${label(only_tag({ tag: leaf("taken tag"), items: [leaf("dropped item")] }))}")

    # A nested node moves through the same slot.
    nested = push({ tag: Tree.Node({ tag: leaf("inner"), items: [leaf("inner item")] }), items: [] }, leaf("outer item"))
    Stdout.line!("nested: ${label(nested.tag)} ${nested.items.len().to_str()}")

    # A tag's only payload moves out of its box alone, then with another holder.
    flat = |suffix| Nest.Flat("${input} ${suffix}")
    alone = collect(Nest.Deep(Wrap(flat("alone"))), [])
    held = Nest.Deep(Wrap(flat("held")))
    both = collect(held, collect(held, alone))
    Stdout.line!("payload: ${both.len().to_str()} ${collect(flat("none"), []).len().to_str()}")

    # The same functions at a record no recursion box reaches.
    plain = push({ tag: "${input} plain", items: [1.U64] }, 2)
    Stdout.line!("plain: ${plain.tag} ${plain.items.len().to_str()}")
}
