app "rust-glue"
    packages { pf: "RocType.roc" }
    imports []
    provides [makeGlue] to pf

makeGlue = \types ->
    when types |> List.map typesWithDict |> List.first is
        Ok arch ->
            lenStr = List.len arch.types |> Num.toStr
            Ok [
                {name: "it_works.txt", content: "This is the first file generated with roc glue!\nYay! This works with dynamically linked platforms!\nThere are \(lenStr) types to make.\n"}
            ]
        Err ListWasEmpty ->
            Err "No types provided"

# This is a temporary helper until roc_std::roc_dict is update.
# after that point, Dict will be passed in directly.
typesWithDict = \{types, sizes, aligns, typesByName, deps, target} ->
    {
        types,
        sizes,
        aligns,
        typesByName: Dict.fromList typesByName,
        deps: Dict.fromList deps,
        target,
    }
