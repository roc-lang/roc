Issue9487StaticDispatchEq :: [].{}

Tag := [A, B]

Req :: { tag : Tag }.{
    from_tag : Tag -> Req
    from_tag = |t| { tag: t }

    tag : Req -> Tag
    tag = |req| req.tag
}

label = |req| req.tag() == A

expect label(Req.from_tag(A))
