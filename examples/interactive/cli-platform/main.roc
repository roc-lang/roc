platform "cli"
    requires {} { main : _ }
    exposes []
    packages {}
    imports [Http.{
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
        Response,
        Metadata,
        handleStringResponse,
        handleEncodedResponse,
    }]
    provides [mainForHost]

BindGenTypes : {
    req : Request Str,
    res : Response (List U8),
}

mainForHost : BindGenTypes
mainForHost = { req, res }

req : Request Str
req = {
    method: "GET",
    headers: [{ name: "Expires", value: "0" }],
    url: "https://www.google.com",
    body: stringBody (MimeType "text") "banana",
    responseHandler: handleStringResponse,
    timeout: WithoutTimeout,
    tracker: WithoutTracker,
    allowCookiesFromOtherDomains: False,
}

metadata : Metadata
metadata = {
    url: "https://www.google.com/redirect",
    statusCode: 200,
    statusText: "OK",
    headers: [{ name: "All", value: "Good" }],
}

res : Response (List U8)
res = GoodStatus metadata [0x48, 0x69]

# Mistakes I make:
# square brackets for tags     body : stringBody [MimeType "text"] "banana",
