# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{{0
}}

""
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly OpenCurly Int CloseCurly CloseCurly BlankLine String ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "f")
      (binop_platform
        (str_literal_small "")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

{
	{
		0
	}
}
""
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_044.md:1:20:2:3:**
```roc
app[]{f:platform""}{{0
}}
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_044.md:4:1:4:3:**
```roc
""
```
^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
