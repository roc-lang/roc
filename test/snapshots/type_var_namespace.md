# META
~~~ini
description=Type variables and values exist in separate namespaces
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Type variable 'elem' introduced in annotation
process : List(elem) -> elem
process = |list| {
    # value identifier named 'elem' is allowed - different namespace from type variable
    elem = 42

    # type variable 'elem' still refers to the function annotation's type parameter
    result : elem
    result = List.first(list) |> Result.withDefault(elem)

    result
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LineComment LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpColon LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpBar OpGreaterThan UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Type variable 'elem' introduced in annotation
process : List elem -> elem
process = |list| {
	# value identifier named 'elem' is allowed - different namespace from type variable
	elem = 42

	# type variable 'elem' still refers to the function annotation's type parameter
	result : elem
	List.first(list)
	Result | .withDefault(elem)

	result
}

main! = |_| {}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_var_namespace.md:11:32:11:34
UNEXPECTED TOKEN IN EXPRESSION - type_var_namespace.md:11:34:11:40
UNSUPPORTED NODE - type_var_namespace.md:11:34:11:52
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_namespace.md:11:32:11:34:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Result** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_namespace.md:11:34:11:40:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
                                 ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_var_namespace.md:11:34:11:52:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
                                 ^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "process")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
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
