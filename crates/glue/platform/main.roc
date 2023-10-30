platform "roc-lang/glue"
    requires {} { makeGlue : List Module -> Result (List File) Str }
    exposes [Type, File, Module, TypeId, Target]
    packages {}
    imports [Module.{ Module }, File.{ File }]
    provides [makeGlueForHost]

makeGlueForHost : List Module -> Result (List File) Str
makeGlueForHost = \types -> makeGlue types
