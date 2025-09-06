# META
~~~ini
description=Multiple type variables in a single type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Multiple type variables 'a' and 'b' introduced in annotation
swap : (a, b) -> (b, a)
swap = |pair| {
    (first, second) = pair
    (second, first)
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

# Multiple type variables 'a' and 'b' introduced in annotation
swap : (a, b) -> (b, a)
swap = |pair| {
	(first, second) = pair((second, first))
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**type_var_multiple.md:6:27:7:5:**
```roc
    (first, second) = pair
    (second, first)
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "swap")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "swap"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
