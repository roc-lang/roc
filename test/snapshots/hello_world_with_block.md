# META
~~~ini
description=Hello world with a block
type=file
~~~
# SOURCE
~~~roc
# Hello world!

# Multiline comments?
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout

main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# TOKENS
~~~text
LineComment BlankLine LineComment KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign String LineComment UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
# Hello world!

# Multiline comments?
app { pf: "../basic-cli/platform.roc" platform [main!] }


import pf.Stdout
main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**hello_world_with_block.md:11:2:11:14:**
```roc
	Stdout.line!("Hello, world!")
```
	^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **world** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_world` to suppress this warning.
The unused variable is declared here:

**hello_world_with_block.md:9:2:9:7:**
```roc
	world = "World"
```
	^^^^^


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
