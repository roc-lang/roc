app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

run_primitive! : () => I64
run_primitive! = || {
    boxed = Host.boxed_add!(10)
    f = Box.unbox(boxed)

    f(32)
}

run_nested_record! : () => I64
run_nested_record! = || {
    boxed = Host.boxed_nested_record!("abcdef")
    f = Box.unbox(boxed)

    f(10)
}

run_recursive_tree! : () => I64
run_recursive_tree! = || {
    tree =
        Host.Tree.Node(
            Box.box(Host.Tree.Leaf(5)),
            Box.box(Host.Tree.Node(
                Box.box(Host.Tree.Leaf(7)),
                Box.box(Host.Tree.Leaf(11)),
            )),
        )

    boxed = Host.boxed_recursive_tree!(tree)
    f = Box.unbox(boxed)

    f(19)
}

main! = || {
    Host.reset_boxed_drop_report!()

    Stdout.line!("primitive: ${run_primitive!().to_str()}")
    Stdout.line!("nested record: ${run_nested_record!().to_str()}")
    Stdout.line!("recursive tree: ${run_recursive_tree!().to_str()}")
    Stdout.line!(Host.boxed_drop_report!())
}
