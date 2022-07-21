interface Path
    exposes [
        Path,
        fromStr,
        toStr
    ]
    imports []

Path := Str

fromStr : Str -> Path
fromStr = @Path

toStr : Path -> Str
toStr = \@Path str -> str
