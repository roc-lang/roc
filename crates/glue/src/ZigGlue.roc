app "zig-glue"
    packages { pf: "../platform/main.roc" }
    imports [
        pf.Types.{ Types },
        pf.File.{ File },
        "../../compiler/builtins/bitcode/src/list.zig" as rocStdList : Str,
    ]
    provides [makeGlue] to pf

makeGlue : List Types -> Result (List File) Str
makeGlue = \typesByArch ->
    typesByArch
    |> List.map convertTypesToFile
    |> List.concat staticFiles
    |> Ok

## These are always included, and don't depend on the specifics of the app.
staticFiles : List File
staticFiles = [
    { name: "list.zig", content: rocStdList },
]

convertTypesToFile : Types -> File
convertTypesToFile = \_ -> {
    name: "glue.zig",
    content: "// Nothing to see yet",
}
