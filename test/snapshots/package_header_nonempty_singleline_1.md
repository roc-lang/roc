# META
~~~ini
description=package_header_nonempty_singleline (1)
type=file
~~~
# SOURCE
~~~roc
package [something, SomeType] { somePkg: "../main.roc", other: "../../other/main.roc" }
~~~
# TOKENS
~~~text
KwPackage OpenSquare LowerIdent Comma UpperIdent CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (exposes
    (lc "something")

    (uc "SomeType")
)
  (packages
    (lc "somePkg")

    (binop_colon
      (tuple_literal
        (str_literal_big "../main.roc")
        (lc "other")
      )
      (str_literal_big "../../other/main.roc")
    )
))
~~~
# FORMATTED
~~~roc
package [something, SomeType] packages {somePkg, ("../main.roc", other) : "../../other/main.roc"}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package_header_nonempty_singleline_1.md:1:10:1:19
EXPOSED BUT NOT DEFINED - package_header_nonempty_singleline_1.md:1:21:1:29
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# Header type not yet fully supported
~~~
