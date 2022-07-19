interface InternalHttp
    exposes [Request, Method, Header, Timeout, ProgressTracking, Part, Body, Response, Metadata, Error]
    imports []

Request : {
    method : Method,
    headers : List Header,
    url : Str,
    body : Body,
    timeout : Timeout,
    progressTracking : ProgressTracking,
    allowCookiesFromOtherDomains : Bool,
}

Method : [Options, Get, Post, Put, Delete, Head, Trace, Connect, Patch]

Header : [Header Str Str]

Timeout : [Timeout F64, NoTimeout]

ProgressTracking : [ProgressTrackingId Str, NoProgressTracking]

Part : [Part Str (List U8)]

Body : [
    Body [MimeType Str] (List U8),
    EmptyBody,
]

Response : [
    BadUrl Str,
    Timeout,
    NetworkError,
    BadStatus Metadata (List U8),
    GoodStatus Metadata (List U8),
]

Metadata : {
    url : Str,
    statusCode : U16,
    statusText : Str,
    headers : List Header,
}

Error : [
    BadUrl Str,
    Timeout,
    NetworkError,
    BadStatus U16,
    BadBody Str,
]
