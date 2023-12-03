platform "roc-lang/glue"
    requires {} { makeGlue : List Types -> Result (List File) Str }
    exposes [Shape, File, Types, TypeId, Target]
    packages {}
    imports [Types.{ Types }, File.{ File }]
    provides [makeGlueForHost]

makeGlueForHost : List Types -> Result (List File) Str
makeGlueForHost = \types -> makeGlue types
