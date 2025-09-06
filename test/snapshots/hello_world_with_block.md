# META
~~~ini
description=Hello world with a block
type=file
~~~
# SOURCE
~~~roc
# Hello world!

# Multiline comments?
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# TOKENS
~~~text
LineComment BlankLine LineComment KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign String LineComment UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound CloseCurly ~~~
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
# Hello world!

# Multiline comments?
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

import pf.Stdout

main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# EXPECTED
MODULE NOT FOUND - hello_world_with_block.md:6:1:6:17
UNUSED VARIABLE - hello_world_with_block.md:9:2:9:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**hello_world_with_block.md:11:2:11:14:**
```roc
	Stdout.line!("Hello, world!")
```
	^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**hello_world_with_block.md:11:8:11:14:**
```roc
	Stdout.line!("Hello, world!")
```
	      ^^^^^^


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
  (Stmt.import)
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
