app "app"
    packages { pf: "platform/main.roc" }
    imports [pf.Task]
    provides [main] to pf

# TODO get the URL, send response as List U8
# TODO split the request URL on a query param which will be another URL, then go hit that URL and include its resp in our response
main = \str -> Task.ok "hi, \(str)!!"
