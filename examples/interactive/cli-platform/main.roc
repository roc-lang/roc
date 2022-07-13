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
        handleStringResponse,
        handleEncodedResponse,
    }]
    provides [mainForHost]

mainForHost : Request Str
mainForHost = {
    method: "GET",
    headers: [{ name: "Expires", value: "0" }],
    url: "https://www.google.com",
    body: stringBody (MimeType "text") "banana",
    responseHandler: handleStringResponse,
    timeout: WithoutTimeout,
    tracker: WithoutTracker,
    allowCookiesFromOtherDomains: False,
}

# Mistakes I make:
# square brackets for tags     body : stringBody [MimeType "text"] "banana",
