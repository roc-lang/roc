interface IngestedFile
    exposes [str, nested]
    imports []

import "IngestedFile.roc" as foo : Str

str = foo

nested =
    import "Dep1.roc" as dep1 : Str
    dep1
