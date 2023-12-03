interface IngestedFile
    exposes [str]
    imports ["IngestedFile.roc" as foo : Str]

str = foo
