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
    handleBytesResponse response
    |> Result.try (\bytes -> Str.fromUtf8 bodyBytes)
    |> Result.mapErr \BadUtf8 _ pos ->
        position = Num.toStr pos

        BadBody "Invalid UTF-8 at byte offset \(position)"

handleBytesResponse : Response -> Result List U8 Error
handleBytesResponse = \response ->
    when response is
        BadRequest err -> Err (BadRequest err)
        Timeout -> Err Timeout
        NetworkError -> Err NetworkError
        BadStatus metadata _ -> Err (BadStatus metadata.statusCode)
        GoodStatus _ bodyBytes -> Ok bodyBytes


errorToString : Error -> Str
errorToString = \err ->
    when err is
        BadRequest e -> "Invalid Request: \(e)"
        Timeout -> "Request timed out"
        NetworkError -> "Network error"
        BadStatus code -> Str.concat "Request failed with status " (Num.toStr code)
        BadBody details -> Str.concat "Request failed. Invalid body. " details

send :
    {
        method : Str,
        url : Str,
        decode :
            {
                status : U16,
                headers : List [Header Str Str ],
                body : [Body { contentType : Str, content : List U8 }, NoBody],
            }
            -> Result ok err,
        headers ? List [Header Str Str],
        body ? [Body { contentType : Str, content : List U8 }, NoBody],
        timeout ? [TimeoutMilliseconds U64, NoTimeout],
    }
    -> Task ok (Error err) [Network [Http]*]*
send = \{ method, url, decode, headers ? [], body ? NoBody, timeout ? NoTimeout } ->
    req = { method, url, headers, body, timeout }
    # TODO: Fix our C ABI codegen so that we don't need this Box.box heap allocation
    Effect.sendRequest (Box.box req)
    |> Effect.map decode
    |> InternalTask.fromEffect


notify :
    Str
    -> {
        method : Str,
        url : Str,
        decode :
            {
                status : U16,
                headers : List [Header Str Str ],
                body : [Body { contentType : Str, content : List U8 }, NoBody],
            }
            -> Result ok err,
        headers ? List [Header Str Str],
        body : [Body { contentType : Str, content : List U8 }, NoBody],
        timeout ? [TimeoutMilliseconds U64, NoTimeout],
    }
notify = \str ->
    decode = \response ->
        handleBytesResponse response
        |> Result.try \bytes ->
            # Could do extra processing here and return an Err if the response
            # was syntactically valid JSON but the data inside was wrong somehow.
            Decode.decode bytes Json.fromUtf8

    # probably an authentication token would go here.
    headers = []

    {
        method: "POST",
        url: str,
        decode,
        headers,
        body: NoBody,
        # don't specify timeout because it's not important to Bugsnag;
        # defer to the platform's default!
    }

task : Task BugsnagResponse (Http.Error (BugsnagError *)) [Network [Http]*]*
task =
    notify "bugsnag info goes here"
    |> send