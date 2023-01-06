platform "roc-lang/glue"
    requires {} { makeGlue : List Types -> Result (List { path : Str, content : List U8 }) Str }
    exposes [Target, OutputFile, RocType]
    packages {}
    imports [OutputFile.{ OutputFile }, RocType.{ Types, TypeId, RocType }, Target.{ Target }]
    provides [makeGlueForHost]

# This is a temporary workaround until roc_std::roc_dict is updated to the new internal forma,
# and the host can properly create a RocDict to pass in here instead of this list of tuples
# (the workaround).
TypesWithoutDict : {
    types : List RocType,
    sizes : List U32,
    aligns : List U32,
    typesByName : List [T Str TypeId],
    deps : List [T TypeId (List TypeId)],
    target : Target,
}

toTypesWithDict : TypesWithoutDict -> Types
toTypesWithDict = \{ types, sizes, aligns, typesByName, deps, target } -> {
    types,
    sizes,
    aligns,
    typesByName: Dict.fromList typesByName,
    deps: Dict.fromList deps,
    target,
}

makeGlueForHost : List TypesWithoutDict -> Result (List OutputFile) Str
makeGlueForHost = \types ->
    types
    |> List.map toTypesWithDict
    |> makeGlue