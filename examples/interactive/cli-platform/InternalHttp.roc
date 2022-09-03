interface InternalHttp
    exposes [Request, RequestConfig, Method, Header, TimeoutConfig, Part, Body, Response, Metadata, Error, method]
    imports []

Request : {
    method : Str,
    url : Str,
    headers : List [Header Str Str],
    body : [Body { mimeType : Str, content : List U8 }, NoBody],
    timeout : [TimeoutMilliseconds U64, NoTimeout],
}

Part : [Part Str (List U8)]

Response : [
    BadRequest Str,
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

Error responseErr : [
    BadRequest Str,
    Timeout,
    NetworkError,
    BadStatus U16,
    BadBody Str,
    BadResponse responseErr,
]responseErr
