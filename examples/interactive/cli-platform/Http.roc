interface Http
    exposes [
        Error,
        Request,
        Header,
        header,
        Body,
        emptyBody,
        bytesBody,
        stringBody,
        jsonBody,
        multiPartBody,
    ]
    imports [pf.Effect, Task.{ Task }, Encode.Encoding, Json]

Error : [
    BadUrl Str,
    Timeout,
    NetworkError,
    BadStatus U16,
    BadBody Str,
]

Request a : {
    method : Str,
    headers : List Header,
    url : Str,
    body : Body,
    expect : Expect a, # TODO: rename this, it clashes with the test thingy
    timeout : Maybe Float,
    tracker : Maybe Str,
    allowCookiesFromOtherDomains : Bool
}



Header : [Header Str Str]

## An HTTP header for configuring requests. See a bunch of common headers
## [here](https://en.wikipedia.org/wiki/List_of_HTTP_header_fields).
##
header : Str, Str -> Header
header = Header



Body : [
    Body [MimeType Str] (List U8),
    EmptyBody
]

emptyBody : Body
emptyBody =
    EmptyBody

bytesBody : [MimeType Str], List U8 -> Body
bytesBody =
    Body

stringBody : [MimeType Str], Str -> Body
stringBody = \mimeType, str ->
    Body mimeType (Str.toUtf8 str)

jsonBody : a -> Body | a has Encoding
jsonBody = \val ->
    Body (MimeType "application/json") (Encode.toBytes val Json.format)

multiPartBody : List Part -> Body
multiPartBody parts =
    boundary = "7MA4YWxkTrZu0gW" # TODO, what's this exactly?
    beforeName = Str.toUtf8 "-- \(boundary)\r\nContent-Disposition: form-data; name=\""
    afterName = Str.toUtf8 "\"\r\n"
    bytes = List.walk parts [] \acc, (Part name bytes) ->
                acc
                |> List.concat beforeName
                |> List.concat name
                |> List.concat afterName
                |> List.concat bytes

    Body (MimeType "multipart/form-data;boundary=\"\(boundary)\"") bytes


Part : [Part Str (List U8)]

bytesPart : Str, List U8 -> Part
bytesPart =
    Part

stringPart : Str, Str -> Part
stringPart = \name, str ->
    Part name (Str.toUtf8 str)


