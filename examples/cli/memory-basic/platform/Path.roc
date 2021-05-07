interface Path
    exposes [ Path, fromStr, toStr ]
    imports []


Path : [ @Path Str ]


fromStr : Str -> Result Path [ MalformedPath ]*
fromStr = \str ->
    # TODO actually validate the path - may want a Parser for this!
    Ok (@Path str)

toStr : Path -> Str
toStr = \@Path str ->
    str
