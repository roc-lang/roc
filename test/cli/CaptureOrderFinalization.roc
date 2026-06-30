CaptureOrderFinalization :: {}.{
    Method : [Get, Post]

    Version : { major : U8, minor : U8 }

    Request : {
        method : Method,
        uri : Str,
        http_version : Version,
        headers : List(Str),
        body : List(U8),
    }

    make_request =
        |m| {
            |u| {
                |hv| {
                    |hs| {
                        |b| {
                            { method: m, uri: u, http_version: hv, headers: hs, body: b }
                        }
                    }
                }
            }
        }
}

expect {
    actual =
        CaptureOrderFinalization.make_request(Get)("/things")({ major: 1, minor: 1 })(["Host"])([])

    expected =
        { method: Get, uri: "/things", http_version: { major: 1, minor: 1 }, headers: ["Host"], body: [] }

    actual == expected
}
