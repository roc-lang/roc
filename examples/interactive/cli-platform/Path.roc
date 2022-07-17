interface Path
    exposes [
        Path,
        fromStr,
        toStr,
        display,
    ]
    imports []

Path := Str

fromStr : Str -> Path
fromStr = @Path

toStr : Path -> Str
toStr = \@Path str -> str

display : Path -> Str
display = \@Path str -> str
