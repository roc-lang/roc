platform "static-site-gen"
    requires {} { transformFileContent : List U8 -> Result (List U8) Str }
    exposes []
    packages {}
    imports []
    provides [transformFileContentForHost]

transformFileContentForHost : List U8 -> Result (List U8) Str
transformFileContentForHost = \list -> transformFileContent list
