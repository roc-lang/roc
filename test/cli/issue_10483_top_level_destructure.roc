module [answer]

s = { req: 1, other: 2 }
{ req, .. } = s
answer = req
