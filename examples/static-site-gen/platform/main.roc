platform "static-site-gen"
    requires {} { transformFileContent : Str -> Result Str Str }
    exposes []
    packages {}
    imports []
    provides [transformFileContentForHost]

transformFileContentForHost : Str -> Result Str Str
transformFileContentForHost = \list -> transformFileContent list
