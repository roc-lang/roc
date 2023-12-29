interface IngestedFile
    exposes [str]
    imports []

import "IngestedFile.roc" as foo : Str

str = foo
