platform "roc-lang/glue"
    requires {} { makeGlue : List Types -> Result (List { path : Str, content : List U8 }) Str }
    exposes [Target, OutputFile, RocType]
    packages {}
    imports [OutputFile.{ OutputFile }, RocType.{ Types }]
    provides [makeGlueForHost]

makeGlueForHost : List Types -> Result (List OutputFile) Str
makeGlueForHost = \x -> makeGlue x