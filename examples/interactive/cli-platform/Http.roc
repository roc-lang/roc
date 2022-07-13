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
        stringPart,
        bytesPart,
        handleStringResponse,
        handleEncodedResponse,
    ]
    imports [Encode.{ Encoding }, Json]

TimeoutConfig : [WithTimeout F64, WithoutTimeout]
TrackerConfig : [WithTracker F64, WithoutTracker]

Request a : {
    method : Str,
    headers : List Header,
    url : Str,
    body : Body,
    responseHandler : ResponseHandler a,
    timeout : TimeoutConfig,
    tracker : TrackerConfig,
    allowCookiesFromOtherDomains : Bool,
}

Header : { name : Str, value : Str }

## An HTTP header for configuring requests. See a bunch of common headers
## [here](https://en.wikipedia.org/wiki/List_of_HTTP_header_fields).
##
header : Str, Str -> Header
header = \name, value ->
    { name, value }

Body : [
    Body [MimeType Str] (List U8),
    EmptyBody,
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
multiPartBody = \parts ->
    boundary = "7MA4YWxkTrZu0gW" # TODO: what's this exactly? a hash of all the part bodies?
    beforeName = Str.toUtf8 "-- \(boundary)\r\nContent-Disposition: form-data; name=\""
    afterName = Str.toUtf8 "\"\r\n"
    appendPart = \buffer, Part name partBytes ->
        buffer
            |> List.concat beforeName
            |> List.concat (Str.toUtf8 name)
            |> List.concat afterName
            |> List.concat partBytes
    bodyBytes = List.walk parts [] appendPart

    Body (MimeType "multipart/form-data;boundary=\"\(boundary)\"") bodyBytes

Part : [Part Str (List U8)]

bytesPart : Str, List U8 -> Part
bytesPart =
    Part

stringPart : Str, Str -> Part
stringPart = \name, str ->
    Part name (Str.toUtf8 str)

Error : [
    BadUrl Str,
    Timeout,
    NetworkError,
    BadStatus U16,
    BadBody Str,
]

Response body : [
    BadUrl Str,
    Timeout,
    NetworkError,
    BadStatus Metadata body,
    GoodStatus Metadata body,
]

Metadata : {
    url : Str,
    statusCode : U16,
    statusText : Str,
    headers : List Header,
}

ResponseHandler a : Response (List U8) -> Result a Error

handleEncodedResponse : (List U8 -> Result a Str) -> ResponseHandler a
handleEncodedResponse = \decoder ->
    \response ->
        when response is
            BadUrl url -> Err (BadUrl url)
            Timeout -> Err Timeout
            NetworkError -> Err NetworkError
            BadStatus metadata _ -> Err (BadStatus metadata.statusCode)
            GoodStatus _ bodyBytes -> decoder bodyBytes |> Result.mapErr BadBody

handleStringResponse : ResponseHandler Str
handleStringResponse = \response ->
    when response is
        BadUrl url -> Err (BadUrl url)
        Timeout -> Err Timeout
        NetworkError -> Err NetworkError
        BadStatus metadata _ -> Err (BadStatus metadata.statusCode)
        GoodStatus _ bodyBytes ->
            Str.fromUtf8 bodyBytes
                |> Result.mapErr
                    \BadUtf8 _ pos ->
                        position = Num.toStr pos

                        BadBody "Invalid UTF-8 at byte offset \(position)"
