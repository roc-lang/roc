# META
~~~ini
description=Type variables nested within complex type constructors
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform.roc" }

# Map over Result type
map_result : Result(a, e), (a -> b) -> Result(b, e)
map_result = |result, transform| {
    match result {
        Ok(value) => Ok(transform(value))
        Err(error) => Err(error)
    }
}

# Simple identity function with type variable
identity : a -> a
identity = |x| x

# Nested type variables in records
make_pair : a, b -> { first: a, second: b }
make_pair = |x, y| { first: x, second: y }

# Function that works with lists of any type
list_length : List(_a) -> U64
list_length = |_lst| 42

# Nested Result types
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))

main = |_| "done"
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound UpperIdent OpenRound LowerIdent Comma UpperIdent CloseRound Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LowerIdent OpAssign OpBar Underscore OpBar String ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "platform.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [main] { pf: "platform.roc" platform [] }

# Map over Result type
map_result : Result(a, e) -> (a -> b) -> Result(b, e)
map_result = |result, transform| {
	match result
		Ok(value) => Ok
	)
	)
	Err(error)
	=> 
	Err(error)
}

}

# Simple identity function with type variable
identity : a -> a
identity = |x| x

# Nested type variables in records
make_pair : a -> b -> {first: a, second: b}
make_pair = |x, y| { first: x, second: y }

# Function that works with lists of any type
list_length : List _a -> U64
list_length = |_lst| 42

# Nested Result types
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))

main = |_| "done"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **Ok** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:7:22:7:24:**
```roc
        Ok(value) => Ok(transform(value))
```
                     ^^


**PARSE ERROR**
A parsing error occurred: **expected_close_round**
This is an unexpected parsing error. Please check your syntax.

**type_var_nested.md:7:34:7:35:**
```roc
        Ok(value) => Ok(transform(value))
```
                                 ^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**type_var_nested.md:7:35:7:40:**
```roc
        Ok(value) => Ok(transform(value))
```
                                  ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:7:40:7:41:**
```roc
        Ok(value) => Ok(transform(value))
```
                                       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
        ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:7:41:8:9:**
```roc
        Ok(value) => Ok(transform(value))
        Err(error) => Err(error)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:8:20:8:23:**
```roc
        Err(error) => Err(error)
```
                   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

# Simple identity function with type variable
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:10:1:13:1:**
```roc
}

# Simple identity function with type variable
identity : a -> a
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_var_nested.md:7:9:7:24:**
```roc
        Ok(value) => Ok(transform(value))
```
        ^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_var_nested.md:8:13:8:18:**
```roc
        Err(error) => Err(error)
```
            ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_var_nested.md:8:27:8:32:**
```roc
        Err(error) => Err(error)
```
                          ^^^^^


**UNUSED VARIABLE**
Variable **transform** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_transform` to suppress this warning.
The unused variable is declared here:

**type_var_nested.md:5:23:5:32:**
```roc
map_result = |result, transform| {
```
                      ^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_var_nested.md:10:1:13:1:**
```roc
}

# Simple identity function with type variable
identity : a -> a
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "map_result")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "map_result"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
  (Stmt.type_anno
    (name "identity")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "make_pair")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "make_pair"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "list_length")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "list_length"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "wrap_in_result")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "wrap_in_result"))
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
