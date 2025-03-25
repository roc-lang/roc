platform "roc-lang/glue"
    requires {} { make_glue : List Types -> Result (List File) Str }
    exposes [Shape, File, Types, TypeId, Target]
    packages {}
    imports [Types.{ Types }, File.{ File }]
    provides [make_glue_for_host]

make_glue_for_host : List Types -> Result (List File) Str
make_glue_for_host = \types -> make_glue(types)
