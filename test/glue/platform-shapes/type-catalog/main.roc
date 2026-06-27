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
        "roc_dec": dec_for_host,
        "roc_i128": i128_for_host,
        "roc_u128": u128_for_host,
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
    targets: {}

import A
import B
import Catalog
import CatalogHost

point_for_host : {} -> Catalog.Point
point_for_host = |_| {
    crash "glue fixture"
}

structural_for_host : {} -> { name : Str, count : U64, nested : { flag : Bool, byte : U8 } }
structural_for_host = |_| {
    crash "glue fixture"
}

tree_for_host : {} -> Catalog.Tree
tree_for_host = |_| {
    crash "glue fixture"
}

catalog_union_for_host : {} -> Catalog.CatalogUnion
catalog_union_for_host = |_| {
    crash "glue fixture"
}

box_payload_for_host : {} -> Box(Catalog.PayloadAlias)
box_payload_for_host = |_| {
    crash "glue fixture"
}

boxed_union_for_host : {} -> Box(Catalog.CatalogUnion)
boxed_union_for_host = |_| {
    crash "glue fixture"
}

bang_record_for_host : {} -> { init! : Box(Catalog.PayloadAlias), render! : Str }
bang_record_for_host = |_| {
    crash "glue fixture"
}

dec_for_host : Dec -> Dec
dec_for_host = |_| {
    crash "glue fixture"
}

i128_for_host : I128 -> I128
i128_for_host = |_| {
    crash "glue fixture"
}

u128_for_host : U128 -> U128
u128_for_host = |_| {
    crash "glue fixture"
}

single_payload_for_host : {} -> Catalog.SinglePayload
single_payload_for_host = |_| {
    crash "glue fixture"
}

single_no_payload_for_host : {} -> Catalog.SingleNoPayload
single_no_payload_for_host = |_| {
    crash "glue fixture"
}

result_a_for_host : {} -> A.Result
result_a_for_host = |_| {
    crash "glue fixture"
}

result_b_for_host : {} -> B.Result
result_b_for_host = |_| {
    crash "glue fixture"
}
