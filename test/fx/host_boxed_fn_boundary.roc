app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

sum_tree : Host.Tree -> I64
sum_tree = |tree|
    match tree {
        Host.Tree.Leaf(n) => n
        Host.Tree.Node(left, right) => sum_tree(Box.unbox(left)) + sum_tree(Box.unbox(right))
    }

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
    boxed = {
        tree =
            Host.Tree.Node(
                Box.box(Host.Tree.Leaf(5)),
                Box.box(Host.Tree.Node(
                    Box.box(Host.Tree.Leaf(7)),
                    Box.box(Host.Tree.Leaf(11)),
                )),
            )

        Host.boxed_recursive_tree!(tree)
    }
    f = Box.unbox(boxed)

    f(19)
}

run_host_returns_boxed_capture! : () => I64
run_host_returns_boxed_capture! = || {
    inner = Box.box(|x| x + 4)
    boxed = Host.boxed_with_boxed_capture!(inner, 5)
    f = Box.unbox(boxed)

    f(33)
}

run_host_consumes_primitive! : () => I64
run_host_consumes_primitive! = || {
    boxed = Box.box(|x| x + 5)

    Host.call_boxed!(boxed, 37)
}

run_host_consumes_nested_record! : () => I64
run_host_consumes_nested_record! = || {
    record = { label: "abcd", base: 6 }
    boxed = Box.box(|x| x + record.base + List.len(Str.to_utf8(record.label)).to_i64_wrap())

    Host.call_boxed!(boxed, 30)
}

run_host_consumes_recursive_tree! : () => I64
run_host_consumes_recursive_tree! = || {
    tree =
        Host.Tree.Node(
            Box.box(Host.Tree.Leaf(3)),
            Box.box(Host.Tree.Node(
                Box.box(Host.Tree.Leaf(8)),
                Box.box(Host.Tree.Leaf(12)),
            )),
        )

    boxed = Box.box(|x| x + sum_tree(tree))

    Host.call_boxed!(boxed, 20)
}

run_host_consumes_boxed_capture! : () => I64
run_host_consumes_boxed_capture! = || {
    inner = Box.box(|x| x + 2)
    outer = Box.box(|x| {
        f = Box.unbox(inner)

        f(x) + 3
    })

    Host.call_boxed!(outer, 10)
}

run_host_roundtrip! : () => I64
run_host_roundtrip! = || {
    boxed = Box.box(|x| x * 2)
    roundtripped = Host.roundtrip_boxed!(boxed)
    f = Box.unbox(roundtripped)

    f(21)
}

run_host_store! : () => I64
run_host_store! = || {
    {
        offset = 8
        boxed = Box.box(|x| x + offset)
        Host.store_boxed!(boxed)
    }

    result = Host.stored_boxed_call!(34)
    Host.release_stored_boxed!()

    result
}

main! = || {
    Host.reset_boxed_drop_report!()

    Stdout.line!("primitive: ${run_primitive!().to_str()}")
    Stdout.line!("nested record: ${run_nested_record!().to_str()}")
    Stdout.line!("recursive tree: ${run_recursive_tree!().to_str()}")
    Stdout.line!("host returns boxed capture: ${run_host_returns_boxed_capture!().to_str()}")
    Stdout.line!("host consumes primitive: ${run_host_consumes_primitive!().to_str()}")
    Stdout.line!("host consumes nested record: ${run_host_consumes_nested_record!().to_str()}")
    Stdout.line!("host consumes recursive tree: ${run_host_consumes_recursive_tree!().to_str()}")
    Stdout.line!("host consumes boxed capture: ${run_host_consumes_boxed_capture!().to_str()}")
    Stdout.line!("host roundtrip: ${run_host_roundtrip!().to_str()}")
    Stdout.line!("host store: ${run_host_store!().to_str()}")
    Stdout.line!(Host.boxed_drop_report!())
}
