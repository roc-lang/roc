platform "static-site-gen"
    requires {} { transformFileContent : Str, Str -> Str }
    exposes []
    packages {}
    imports []
    provides [transformFileContentForHost]

transformFileContentForHost : Str, Str -> Str
transformFileContentForHost = \relPath, htmlContent -> transformFileContent relPath htmlContent
