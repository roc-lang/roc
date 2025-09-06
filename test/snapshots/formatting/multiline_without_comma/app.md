# META
~~~ini
description=Multiline without comma formatting app
type=file
~~~
# SOURCE
~~~roc
app [
	a1!,
	a2!
] {
	pf: platform "../basic-cli/main.roc",
	a: "a"
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "a1")

    (not_lc "a2")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )

    (binop_colon
      (lc "a")
      (str_literal_small "a")
    )
))
~~~
# FORMATTED
~~~roc
app [a1!, a2!] { pf: "../basic-cli/main.roc" platform [], a: "a" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
