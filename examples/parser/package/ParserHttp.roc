interface ParserHttp
    exposes [
        Request,
        Response,
        request,
        response,
    ]
    imports [
        ParserCore.{ Parser, ParseResult, map, keep, skip, const, oneOrMore, many },
        ParserStr.{ RawStr, oneOf, string, codeunit, parseStr, codeunitSatisfies, strFromRaw, anyRawString },
    ]

# https://www.ietf.org/rfc/rfc2616.txt
Method : [Options, Get, Post, Put, Delete, Head, Trace, Connect, Patch]

HttpVersion : Str

Request : {
    method : Method,
    uri : Str,
    httpVersion : HttpVersion,
    headers : List [Header Str Str],
    body : List U8,
}

Response : {
    httpVersion : HttpVersion,
    statusCode : Str,
    status : Str,
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

# TODO: The 'digit' and 'digits' from ParserStr are causing repl.expect to blow up
digit = codeunitSatisfies \c -> c >= '0' && c <= '9'
digits = digit |> oneOrMore |> map strFromRaw

httpVersion : Parser RawStr HttpVersion
httpVersion =
    const (\major -> \minor -> "\(major).\(minor)")
    |> skip (string "HTTP/")
    |> keep digits
    |> skip (codeunit '.')
    |> keep digits

Header : [Header Str Str]

stringWithoutColon : Parser RawStr Str
stringWithoutColon =
    codeunitSatisfies \c -> c != ':'
    |> oneOrMore
    |> map strFromRaw

stringWithoutCr : Parser RawStr Str
stringWithoutCr =
    codeunitSatisfies \c -> c != '\r'
    |> oneOrMore
    |> map strFromRaw

header : Parser RawStr Header
header =
    const (\k -> \v -> Header k v)
    |> keep stringWithoutColon
    |> skip (string ": ")
    |> keep stringWithoutCr
    |> skip crlf

expect
    actual = parseStr header "Accept-Encoding: gzip, deflate\r\n"
    expected = Ok (Header "Accept-Encoding" "gzip, deflate")
    actual == expected

request : Parser RawStr Request
request =
    const (\m -> \u -> \hv -> \hs -> \b -> { method: m, uri: u, httpVersion: hv, headers: hs, body: b })
    |> keep method
    |> skip sp
    |> keep requestUri
    |> skip sp
    |> keep httpVersion
    |> skip crlf
    |> keep (many header)
    |> skip crlf
    |> keep anyRawString

expect
    requestText =
        """
        GET /things?id=1 HTTP/1.1\r
        Host: bar.example\r
        Accept-Encoding: gzip, deflate\r
        \r
        Hello, world!
        """
    actual =
        parseStr request requestText

    expected : Result Request [ParsingFailure Str, ParsingIncomplete Str]
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
    requestText =
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
    actual =
        parseStr request requestText
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

response : Parser RawStr Response
response =
    const (\hv -> \sc -> \s -> \hs -> \b -> { httpVersion: hv, statusCode: sc, status: s, headers: hs, body: b })
    |> keep httpVersion
    |> skip sp
    |> keep digits
    |> skip sp
    |> keep stringWithoutCr
    |> skip crlf
    |> keep (many header)
    |> skip crlf
    |> keep anyRawString

expect
    body =
        """
        <!DOCTYPE html>\r
        <html lang="en">\r
        <head>\r
        <meta charset="utf-8">\r
        <title>A simple webpage</title>\r
        </head>\r
        <body>\r
        <h1>Simple HTML webpage</h1>\r
        <p>Hello, world!</p>\r
        </body>\r
        </html>\r\n
        """
    responseText =
        """
        HTTP/1.1 200 OK\r
        Content-Type: text/html; charset=utf-8\r
        Content-Length: 55743\r
        Connection: keep-alive\r
        Cache-Control: s-maxage=300, public, max-age=0\r
        Content-Language: en-US\r
        Date: Thu, 06 Dec 2018 17:37:18 GMT\r
        ETag: "2e77ad1dc6ab0b53a2996dfd4653c1c3"\r
        Server: meinheld/0.6.1\r
        Strict-Transport-Security: max-age=63072000\r
        X-Content-Type-Options: nosniff\r
        X-Frame-Options: DENY\r
        X-XSS-Protection: 1; mode=block\r
        Vary: Accept-Encoding,Cookie\r
        Age: 7\r
        \r
        \(body)
        """
    actual =
        parseStr response responseText
    expected =
        Ok {
            httpVersion: "1.1",
            statusCode: "200",
            status: "OK",
            headers: [
                Header "Content-Type" "text/html; charset=utf-8",
                Header "Content-Length" "55743",
                Header "Connection" "keep-alive",
                Header "Cache-Control" "s-maxage=300, public, max-age=0",
                Header "Content-Language" "en-US",
                Header "Date" "Thu, 06 Dec 2018 17:37:18 GMT",
                Header "ETag" "\"2e77ad1dc6ab0b53a2996dfd4653c1c3\"",
                Header "Server" "meinheld/0.6.1",
                Header "Strict-Transport-Security" "max-age=63072000",
                Header "X-Content-Type-Options" "nosniff",
                Header "X-Frame-Options" "DENY",
                Header "X-XSS-Protection" "1; mode=block",
                Header "Vary" "Accept-Encoding,Cookie",
                Header "Age" "7",
            ],
            body: Str.toUtf8 body,
        }
    actual == expected

