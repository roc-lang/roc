# META
~~~ini
description=Type variables nested within complex type constructors
type=file
~~~
# SOURCE
~~~roc
app { pf: "platform.roc" platform [main] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound UpperIdent OpenRound LowerIdent Comma UpperIdent CloseRound Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LowerIdent OpAssign OpBar Underscore OpBar String ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "platform.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "platform.roc" platform [main] }

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
make_pair : a -> b -> {first : a, second : b}
make_pair = |x, y| { first : x, second : y }
list_length : List _a -> U64
list_length = |_lst| 42
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))
main = |_| "done"# Nested type variables in records
# Function that works with lists of any type
# Nested Result types
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


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "map_result")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.binop_thin_arrow
          (Expr.lookup "a")
          (Expr.lookup "b")
        )
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "map_result")
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "identity")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.lookup "a")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "identity")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "make_pair")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.binop_thin_arrow
        (Expr.lookup "b")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "first")
            (Expr.lookup "a")
          )
          (Expr.binop_colon
            (Expr.lookup "second")
            (Expr.lookup "b")
          )
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "make_pair")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "list_length")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "list_length")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "wrap_in_result")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "wrap_in_result")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
map_result : _c
identity : _c
make_pair : _c
list_length : _c
wrap_in_result : _c
main : _c
~~~
