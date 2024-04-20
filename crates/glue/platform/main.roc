platform "roc-lang/glue"
    requires {} { makeGlue : List Types -> Result (List File) Str }
    exposes [Shape, File, Types, TypeId, Target]
    packages {}
    provides [makeGlueForHost]

import Types exposing [Types]
import File exposing [File]

makeGlueForHost : List Types -> Result (List File) Str
makeGlueForHost = \types -> makeGlue types
