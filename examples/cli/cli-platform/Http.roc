interface Http
    exposes [
        Request,
        Method,
        Header,
        TimeoutConfig,
        Body,
        Response,
        Metadata,
        Error,
        header,
        emptyBody,
        bytesBody,
        stringBody,
        handleStringResponse,
        defaultRequest,
        errorToString,
        send,
    ]
    imports [Effect, InternalTask, Task.{ Task }, InternalHttp]

Request : InternalHttp.Request
Method : InternalHttp.Method
Header : InternalHttp.Header
TimeoutConfig : InternalHttp.TimeoutConfig
Body : InternalHttp.Body
Response : InternalHttp.Response
Metadata : InternalHttp.Metadata
Error : InternalHttp.Error

defaultRequest : Request
defaultRequest = {
    method: Get,
    headers: [],
    url: "",
    body: Http.emptyBody,
    timeout: NoTimeout,
}

## An HTTP header for configuring requests. See a bunch of common headers
## [here](https://en.wikipedia.org/wiki/List_of_HTTP_header_fields).
##
header : Str, Str -> Header
header =
    Header

emptyBody : Body
emptyBody =
    EmptyBody

bytesBody : [MimeType Str], List U8 -> Body
bytesBody =
    Body

stringBody : [MimeType Str], Str -> Body
stringBody = \mimeType, str ->
    Body mimeType (Str.toUtf8 str)

# jsonBody : a -> Body | a has Encoding
# jsonBody = \val ->
#     Body (MimeType "application/json") (Encode.toBytes val Json.format)
#
# multiPartBody : List Part -> Body
# multiPartBody = \parts ->
#     boundary = "7MA4YWxkTrZu0gW" # TODO: what's this exactly? a hash of all the part bodies?
#     beforeName = Str.toUtf8 "-- \(boundary)\r\nContent-Disposition: form-data; name=\""
#     afterName = Str.toUtf8 "\"\r\n"
#     appendPart = \buffer, Part name partBytes ->
#         buffer
#         |> List.concat beforeName
#         |> List.concat (Str.toUtf8 name)
#         |> List.concat afterName
#         |> List.concat partBytes
#     bodyBytes = List.walk parts [] appendPart
#     Body (MimeType "multipart/form-data;boundary=\"\(boundary)\"") bodyBytes
# bytesPart : Str, List U8 -> Part
# bytesPart =
#     Part
# stringPart : Str, Str -> Part
# stringPart = \name, str ->
#     Part name (Str.toUtf8 str)
handleStringResponse : Response -> Result Str Error
handleStringResponse = \response ->
    when response is
        BadRequest err -> Err (BadRequest err)
        Timeout -> Err Timeout
        NetworkError -> Err NetworkError
        BadStatus metadata _ -> Err (BadStatus metadata.statusCode)
        GoodStatus _ bodyBytes ->
            Str.fromUtf8 bodyBytes
            |> Result.mapErr
                \BadUtf8 _ pos ->
                    position = Num.toStr pos

                    BadBody "Invalid UTF-8 at byte offset \(position)"

errorToString : Error -> Str
errorToString = \err ->
    when err is
        BadRequest e -> "Invalid Request: \(e)"
        Timeout -> "Request timed out"
        NetworkError -> "Network error"
        BadStatus code -> Str.concat "Request failed with status " (Num.toStr code)
        BadBody details -> Str.concat "Request failed. Invalid body. " details

send : Request -> Task Str Error
send = \req ->
    # TODO: Fix our C ABI codegen so that we don't this Box.box heap allocation
    Effect.sendRequest (Box.box req)
    |> Effect.map handleStringResponse
    |> InternalTask.fromEffect
