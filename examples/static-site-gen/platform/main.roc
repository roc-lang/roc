platform "static-site-gen"
    requires {} { transformFileContent : Str, Str -> Str }
    exposes []
    packages {}
    imports []
    provides [transformFileContentForHost]

transformFileContentForHost : Box Str, Box Str -> Str
transformFileContentForHost = \relPath, htmlContent -> transformFileContent (Box.unbox relPath) (Box.unbox htmlContent)
