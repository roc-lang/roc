app [main!] { pf: platform "platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/10705: `Request` and
# `Response` are separate public names that resolve to the same source module,
# so the type-check environment list must hold `Container` once.
import pf.Request
import pf.Response

status_for : Request, Response -> U16
status_for = |request, response|
    if request.path == "/" response.status else 500

main! = |_| status_for(Request.{ path: "/" }, Response.{ status: 200 })
