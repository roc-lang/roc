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
        "roc_result_a": result_a_for_host,
        "roc_result_b": result_b_for_host,
    }
    hosted {
        "roc_catalog_roundtrip": CatalogHost.roundtrip!,
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

result_a_for_host : {} -> A.Result
result_a_for_host = |_| {
    crash "glue fixture"
}

result_b_for_host : {} -> B.Result
result_b_for_host = |_| {
    crash "glue fixture"
}
