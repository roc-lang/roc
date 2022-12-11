app "rust-glue"
    packages { pf: "RocType.roc" }
    imports []
    provides [makeGlue] to pf

makeGlue = \types ->
    when List.first types is
        Ok arch ->
            lenStr = List.len arch.types |> Num.toStr
            Ok [
                {name: "it_works.txt", content: "This is the first file generated with roc glue!\nYay! This works with dynamically linked platforms!\nThere are \(lenStr) types to make.\n"}
            ]
        Err ListWasEmpty ->
            Err "No types provided"
