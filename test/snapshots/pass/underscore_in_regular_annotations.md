# META
~~~ini
description=Underscores are allowed in regular type annotations (not in type declarations)
type=file
~~~
# SOURCE
~~~roc
module []

main : _ -> _
main = |x| x

identity : a -> a
identity = |x| x

# Function with underscore in annotation
process : List(_) -> Str
process = |list| "processed"

# Record with underscore
get_data : { field: _, other: U32 } -> U32
get_data = |record| record.other

# Pattern matching with underscore type annotation
handle_result : Result(_, Str) -> Str
handle_result = |result|
    match result {
        Ok(_) => "success",
        Err(msg) => msg,
    }

# Underscore in function arguments
map : (a -> b), List(a) -> List(b)
map = |_, _| []

# Named underscore type variables
transform : _a -> _b -> _b
transform = |_, b| b
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon Underscore OpArrow Underscore LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound Underscore CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon OpenCurly LowerIdent OpColon Underscore Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound Underscore Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpFatArrow String Comma UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent Comma CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar OpenSquare CloseSquare BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore Comma LowerIdent OpBar LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

main : _ -> _
main = |x| x
identity : a -> a
identity = |x| x
process : List _ -> Str
process = |list| "processed"
get_data : {field : _, other : U32} -> U32
get_data = |record| record.other
handle_result : Result(_, Str) -> Str
handle_result = |result| match result
	Ok(_) => "success" => (Err(msg) => msg)

map :
	(a -> b) -> List a -> List b
map = |_, _| []
transform :
	(_a -> _b) -> _b
transform = |_, b| b# Function with underscore in annotation
# Record with underscore
# Pattern matching with underscore type annotation
# Underscore in function arguments
# Named underscore type variables
~~~
# EXPECTED
NIL
# PROBLEMS
**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_regular_annotations.md:3:8:3:9:**
```roc
main : _ -> _
```
       ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_regular_annotations.md:3:13:3:14:**
```roc
main : _ -> _
```
            ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_regular_annotations.md:10:16:10:17:**
```roc
process : List(_) -> Str
```
               ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_regular_annotations.md:14:21:14:22:**
```roc
get_data : { field: _, other: U32 } -> U32
```
                    ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_regular_annotations.md:18:24:18:25:**
```roc
handle_result : Result(_, Str) -> Str
```
                       ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_in_regular_annotations.md:21:9:22:20:**
```roc
        Ok(_) => "success",
        Err(msg) => msg,
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "main")
    (Expr.binop_thin_arrow
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.lambda)
  )
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
    (Expr.lookup "process")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "process")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "get_data")
    (Expr.binop_thin_arrow
      (Expr.record_literal
        (Expr.binop_colon
          (Expr.lookup "field")
          (Expr.malformed)
        )
        (Expr.binop_colon
          (Expr.lookup "other")
          (Expr.apply_tag)
        )
      )
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "get_data")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "handle_result")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "handle_result")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "map")
    (Expr.binop_thin_arrow
      (Expr.binop_thin_arrow
        (Expr.lookup "a")
        (Expr.lookup "b")
      )
      (Expr.binop_thin_arrow
        (Expr.apply_tag)
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "map")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "transform")
    (Expr.binop_thin_arrow
      (Expr.binop_thin_arrow
        (Expr.lookup "_a")
        (Expr.lookup "_b")
      )
      (Expr.lookup "_b")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "transform")
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
main : _c
identity : _c
process : _c
get_data : _c
handle_result : _c
map : _c
transform : _c
~~~
