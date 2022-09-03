platform "static-site-gen"
    requires {} { transformFileContent : Str -> Str }
    exposes []
    packages {}
    imports []
    provides [transformFileContentForHost]

transformFileContentForHost : Str -> Str
transformFileContentForHost = \list -> transformFileContent list
