interface Stdout
    exposes [line]
    imports [pf.Effect, Task.{ Task }]

Error : [
    BadUrl Str,
    Timeout,
    NetworkError,
    BadStatus U16,
    BadBody Str,
]

# https://datatracker.ietf.org/doc/html/rfc7231#section-6
# statusTo
#    | 100  | Continue                      | Section 6.2.1            |
#    | 101  | Switching Protocols           | Section 6.2.2            |
#    | 200  | OK                            | Section 6.3.1            |
#    | 201  | Created                       | Section 6.3.2            |
#    | 202  | Accepted                      | Section 6.3.3            |
#    | 203  | Non-Authoritative Information | Section 6.3.4            |
#    | 204  | No Content                    | Section 6.3.5            |
#    | 205  | Reset Content                 | Section 6.3.6            |
#    | 206  | Partial Content               | Section 4.1 of [RFC7233] |
#    | 300  | Multiple Choices              | Section 6.4.1            |
#    | 301  | Moved Permanently             | Section 6.4.2            |
#    | 302  | Found                         | Section 6.4.3            |
#    | 303  | See Other                     | Section 6.4.4            |
#    | 304  | Not Modified                  | Section 4.1 of [RFC7232] |
#    | 305  | Use Proxy                     | Section 6.4.5            |
#    | 307  | Temporary Redirect            | Section 6.4.7            |
#    | 400  | Bad Request                   | Section 6.5.1            |
#    | 401  | Unauthorized                  | Section 3.1 of [RFC7235] |
#    | 402  | Payment Required              | Section 6.5.2            |
#    | 403  | Forbidden                     | Section 6.5.3            |
#    | 404  | Not Found                     | Section 6.5.4            |
#    | 405  | Method Not Allowed            | Section 6.5.5            |
#    | 406  | Not Acceptable                | Section 6.5.6            |
#    | 407  | Proxy Authentication Required | Section 3.2 of [RFC7235] |
#    | 408  | Request Timeout               | Section 6.5.7            |
#    | 409  | Conflict                      | Section 6.5.8            |
#    | 410  | Gone                          | Section 6.5.9            |
#    | 411  | Length Required               | Section 6.5.10           |
#    | 412  | Precondition Failed           | Section 4.2 of [RFC7232] |
#    | 413  | Payload Too Large             | Section 6.5.11           |
#    | 414  | URI Too Long                  | Section 6.5.12           |
#    | 415  | Unsupported Media Type        | Section 6.5.13           |
#    | 416  | Range Not Satisfiable         | Section 4.4 of [RFC7233] |
#    | 417  | Expectation Failed            | Section 6.5.14           |
#    | 426  | Upgrade Required              | Section 6.5.15           |
#    | 500  | Internal Server Error         | Section 6.6.1            |
#    | 501  | Not Implemented               | Section 6.6.2            |
#    | 502  | Bad Gateway                   | Section 6.6.3            |
#    | 503  | Service Unavailable           | Section 6.6.4            |
#    | 504  | Gateway Timeout               | Section 6.6.5            |
#    | 505  | HTTP Version Not Supported    | Section 6.6.6            |
StatusCode : [
    Continue,
    SwitchingProtocols,
    OK,
    Created,
    Accepted,
    NonAuthoritativeInformation,
    NoContent,
    ResetContent,
    PartialContent,
    MultipleChoices,
    MovedPermanently,
    Found,
    SeeOther,
    NotModified,
    UseProxy,
    TemporaryRedirect,
    BadRequest,
    Unauthorized,
    PaymentRequired,
    Forbidden,
    NotFound,
    MethodNotAllowed,
    NotAcceptable,
    ProxyAuthenticationRequired,
    RequestTimeout,
    Conflict,
    Gone,
    LengthRequired,
    PreconditionFailed,
    PayloadTooLarge,
    URITooLong,
    UnsupportedMediaType,
    RangeNotSatisfiable,
    ExpectationFailed,
    UpgradeRequired,
    InternalServerError,
    NotImplemented,
    BadGateway,
    ServiceUnavailable,
    GatewayTimeout,
    HTTPVersionNotSupported,
    CustomStatus U16,
]

StatusCategory : [
    Informational,
    Successful,
    Redirection,
    ClientError,
    ServerError,
    UnknownStatusCategory,
]

statusCategory : U16 -> StatusCategory
statusCategory = \code ->
    if code >= 600 then
        UnknownStatusCategory
    else if code >= 500 then
        ServerError
    else if code >= 400 then
        ClientError
    else if code >= 300 then
        Redirection
    else if code >= 200 then
        Successful
    else if code >= 100 then
        Informational
    else
        UnknownStatusCategory
