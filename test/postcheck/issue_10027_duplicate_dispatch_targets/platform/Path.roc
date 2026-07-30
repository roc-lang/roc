import path.Path as PackagePath

Path := [].{
    display : PackagePath.Path -> Str
    display = |path| PackagePath.display(path)
}
