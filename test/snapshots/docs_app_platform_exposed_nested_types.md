# META
~~~ini
description=App docs use nested public platform views instead of their private source mod
type=docs
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "platform.roc" }

import pf.Request
import pf.Response

status_for : Request, Response -> U16
status_for = |request, response| if request.path == "/" response.status else 500

main = status_for(Request.{ path: "/" }, Response.{ status: 200 })
~~~
## platform.roc
~~~roc
platform "nested-types"
    requires {} { main : U16 }
    exposes [Container.Request, Container.Response]
    packages {}
    provides { "roc_main": main_for_host }
    targets: {}

import Container

main_for_host = main
~~~
## Container.roc
~~~roc
Container :: [].{
    Request := { path : Str }
    Response := { status : U16 }
}
~~~
# DOCS
~~~clojure
(package-docs
  (name "test-app")
  (mod
    (name "app")
    (package "app")
    (kind app)
    (entry
      (name "status_for")
      (kind value)
      (type (fn (type-ref (mod "pf.Request") (name "Request")) (type-ref (mod "pf.Response") (name "Response")) (type-ref (name "U16"))))
    )
    (entry
      (name "main")
      (kind value)
      (type (type-ref (name "U16")))
    )
  )
  (mod
    (name "Request")
    (package "pf")
    (kind type_mod)
    (entry
      (name "Request")
      (kind nominal)
      (type "Request := " (record (field "path" (type-ref (name "Str")))))
    )
  )
  (mod
    (name "Response")
    (package "pf")
    (kind type_mod)
    (entry
      (name "Response")
      (kind nominal)
      (type "Response := " (record (field "status" (type-ref (name "U16")))))
    )
  )
)
~~~
