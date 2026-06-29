# TODO(HARDEN_GLUE): Delete this wasm-specific platform file and use main.roc
# for wasm runtime contracts after wasm shared-output linking supports
# provided functions with Dec/I128/U128 natural arguments and returns. Today
# `roc build --target=wasm32` exits 1 without diagnostics when the type-catalog
# platform provides the scalar identity roots from main.roc. Compile-only glue
# still covers their generated ABI declarations, and the native runtime
# contract executes those scalar roots.
platform ""
    requires {} { fixture_seed : {} -> {} }
    exposes [A, B, Catalog, CatalogHost]
    packages {}
    provides {
        "roc_point": point_for_host,
        "roc_structural": structural_for_host,
        "roc_tree": tree_for_host,
        "roc_catalog_union": catalog_union_for_host,
        "roc_box_payload": box_payload_for_host,
        "roc_boxed_union": boxed_union_for_host,
        "roc_bang_record": bang_record_for_host,
        "roc_single_payload": single_payload_for_host,
        "roc_single_no_payload": single_no_payload_for_host,
        "roc_result_a": result_a_for_host,
        "roc_result_b": result_b_for_host,
    }
    hosted {
        "roc_catalog_roundtrip": CatalogHost.roundtrip!,
        "roc_catalog_single_payload_roundtrip": CatalogHost.single_payload_roundtrip!,
        "roc_catalog_single_no_payload": CatalogHost.single_no_payload!,
    }
    targets: {
        inputs_dir: "targets/",
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libunwind.a", "libc.a"], output: Exe },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libunwind.a", "libc.a"], output: Exe },
        wasm32: { inputs: ["host.wasm", app], output: Shared },
    }

import A
import B
import Catalog
import CatalogHost

point_for_host : {} -> Catalog.Point
point_for_host = |_| {
    { x: -17, y: 42 }
}

structural_for_host : {} -> { name : Str, count : U64, nested : { flag : Bool, byte : U8 } }
structural_for_host = |_| {
    { name: "catalog", count: 19, nested: { flag: True, byte: 7 } }
}

tree_for_host : {} -> Catalog.Tree
tree_for_host = |_| {
    Node(Box.box(Leaf("left")), Box.box(Leaf("right")))
}

catalog_union_for_host : {} -> Catalog.CatalogUnion
catalog_union_for_host = |_| {
    Payload({ id: 9, name: "payload", flags: [True, False, True] })
}

box_payload_for_host : {} -> Box(Catalog.PayloadAlias)
box_payload_for_host = |_| {
    Box.box({ id: 11, name: "boxed", flags: [False, True] })
}

boxed_union_for_host : {} -> Box(Catalog.CatalogUnion)
boxed_union_for_host = |_| {
    Box.box(Pair({ x: 1, y: 2 }, { x: 3, y: 4 }))
}

bang_record_for_host : {} -> { init! : Box(Catalog.PayloadAlias), render! : Str }
bang_record_for_host = |_| {
    { init!: Box.box({ id: 13, name: "bang", flags: [] }), render!: "rendered" }
}

dec_for_host : Dec -> Dec
dec_for_host = |value| {
    value
}

i128_for_host : I128 -> I128
i128_for_host = |value| {
    value
}

u128_for_host : U128 -> U128
u128_for_host = |value| {
    value
}

single_payload_for_host : {} -> Catalog.SinglePayload
single_payload_for_host = |_| {
    Only({ id: 15, name: "single", flags: [True] })
}

single_no_payload_for_host : {} -> Catalog.SingleNoPayload
single_no_payload_for_host = |_| {
    Ready
}

result_a_for_host : {} -> A.Result
result_a_for_host = |_| {
    Ok("alpha")
}

result_b_for_host : {} -> B.Result
result_b_for_host = |_| {
    Err({ code: 5, message: "bravo" })
}
