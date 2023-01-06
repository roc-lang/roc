interface Parser.Http
    exposes [
        Request,
        # Response,
        # parseRequest,
        # parseResponse,
    ]
    imports [
        Parser.Core.{ Parser, ParseResult, map, apply, skip, const, oneOrMore, many },
        Parser.Str.{
            RawStr,
            oneOf,
            string,
            codeunit,
            parseStr,
            codeunitSatisfies,
            strFromRaw,
            anyRawString,
        },
    ]

# https://www.ietf.org/rfc/rfc2616.txt
Method : [Options, Get, Post, Put, Delete, Head, Trace, Connect, Patch]

HttpVersion : Str

RequestStartLine : {
    method : Method,
    uri : RequestUri,
    httpVersion : HttpVersion,
}

Request : {
    method : Method,
    uri : Str,
    httpVersion : HttpVersion,
    headers : List [Header Str Str],
    body : List U8,
}

method : Parser RawStr Method
method =
    oneOf [
        string "OPTIONS" |> map \_ -> Options,
        string "GET" |> map \_ -> Get,
        string "POST" |> map \_ -> Post,
        string "PUT" |> map \_ -> Put,
        string "DELETE" |> map \_ -> Delete,
        string "HEAD" |> map \_ -> Head,
        string "TRACE" |> map \_ -> Trace,
        string "CONNECT" |> map \_ -> Connect,
        string "PATCH" |> map \_ -> Patch,
    ]

expect parseStr method "GET" == Ok Get
expect parseStr method "DELETE" == Ok Delete

# TODO: do we want more structure in the URI, or is Str actually what programs want anyway?
# This is not a full URL!
#        Request-URI    = "*" | absoluteURI | abs_path | authority
RequestUri : Str

requestUri : Parser RawStr RequestUri
requestUri =
    codeunitSatisfies \c -> c != ' '
    |> oneOrMore
    |> map strFromRaw

sp = codeunit ' '
crlf = string "\r\n"

# TODO: The 'digit' and 'digits' from Parser.Str are causing repl.expect to blow up
digit = codeunitSatisfies \c -> c >= '0' && c <= '9'
digits = digit |> oneOrMore |> map strFromRaw

httpVersion : Parser RawStr HttpVersion
httpVersion =
    const (\major -> \minor -> "\(major).\(minor)")
    |> skip (string "HTTP/")
    |> apply digits
    |> skip (codeunit '.')
    |> apply digits

requestStartLine : Parser RawStr RequestStartLine
requestStartLine =
    const (\m -> \u -> \hv -> { method: m, uri: u, httpVersion: hv })
    |> apply method
    |> skip sp
    |> apply requestUri
    |> skip sp
    |> apply httpVersion
    |> skip crlf

expect
    actual = parseStr requestStartLine "GET / HTTP/1.1\r\n"
    expected = Ok { method: Get, uri: "/", httpVersion: "1.1" }
    actual == expected

expect
    actual = parseStr requestStartLine "GET /things?id=1 HTTP/1.1\r\n"
    expected = Ok { method: Get, uri: "/things?id=1", httpVersion: "1.1" }
    actual == expected

expect
    actual = parseStr requestStartLine "POST /things HTTP/1.1\r\n"
    expected = Ok { method: Post, uri: "/things", httpVersion: "1.1" }
    actual == expected

expect
    actual = parseStr requestStartLine "OPTIONS * HTTP/1.1\r\n"
    expected = Ok { method: Options, uri: "*", httpVersion: "1.1" }
    actual == expected

Header : [Header Str Str]

headerKey : Parser RawStr Str
headerKey =
    codeunitSatisfies \c -> c != ':'
    |> oneOrMore
    |> map strFromRaw

headerValue : Parser RawStr Str
headerValue =
    codeunitSatisfies \c -> c != '\r'
    |> oneOrMore
    |> map strFromRaw

header : Parser RawStr Header
header =
    const (\k -> \v -> Header k v)
    |> apply headerKey
    |> skip (string ": ")
    |> apply headerValue
    |> skip crlf

expect
    actual = parseStr header "Accept-Encoding: gzip, deflate\r\n"
    expected = Ok (Header "Accept-Encoding" "gzip, deflate")
    actual == expected

request : Parser RawStr Request
request =
    const (\m -> \u -> \hv -> \hs -> \b -> { method: m, uri: u, httpVersion: hv, headers: hs, body: b })
    |> apply method
    |> skip sp
    |> apply requestUri
    |> skip sp
    |> apply httpVersion
    |> skip crlf
    |> apply (many header)
    |> skip crlf
    |> apply anyRawString

expect
    httpText =
        """
        GET /things?id=1 HTTP/1.1\r
        Host: bar.example\r
        Accept-Encoding: gzip, deflate\r
        \r
        Hello, world!
        """

    actual = parseStr request httpText
    expected = Ok {
        method: Get,
        uri: "/things?id=1",
        httpVersion: "1.1",
        headers: [
            Header "Host" "bar.example",
            Header "Accept-Encoding" "gzip, deflate",
        ],
        body: "Hello, world!" |> Str.toUtf8,
    }

    actual == expected

expect
    httpText =
        """
        OPTIONS /resources/post-here/ HTTP/1.1\r
        Host: bar.example\r
        Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r
        Accept-Language: en-us,en;q=0.5\r
        Accept-Encoding: gzip,deflate\r
        Connection: keep-alive\r
        Origin: https://foo.example\r
        Access-Control-Request-Method: POST\r
        Access-Control-Request-Headers: X-PINGOTHER, Content-Type\r
        \r\n
        """
    actual = parseStr request httpText
    expected = Ok {
        method: Options,
        uri: "/resources/post-here/",
        httpVersion: "1.1",
        headers: [
            Header "Host" "bar.example",
            Header "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            Header "Accept-Language" "en-us,en;q=0.5",
            Header "Accept-Encoding" "gzip,deflate",
            Header "Connection" "keep-alive",
            Header "Origin" "https://foo.example",
            Header "Access-Control-Request-Method" "POST",
            Header "Access-Control-Request-Headers" "X-PINGOTHER, Content-Type",
        ],
        body: [],
    }

    actual == expected
