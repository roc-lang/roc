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

# Function with underscore in annotation
process : List _ -> Str
process = |list| "processed"

# Record with underscore
get_data : {field : _, other : U32} -> U32
get_data = |record| record.other

# Pattern matching with underscore type annotation
handle_result : Result(_, Str) -> Str
handle_result = |result| match result
	Ok(_) => "success" => (Err(msg) => msg)

# Underscore in function arguments
map :
	(a -> b) -> List a -> List b
map = |_, _| []

# Named underscore type variables
transform :
	(_a -> _b) -> _b
transform = |_, b| b
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **list** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:

**underscore_in_regular_annotations.md:11:12:11:16:**
```roc
process = |list| "processed"
```
           ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_in_regular_annotations.md:15:21:15:33:**
```roc
get_data = |record| record.other
```
                    ^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **record** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_record` to suppress this warning.
The unused variable is declared here:

**underscore_in_regular_annotations.md:15:13:15:19:**
```roc
get_data = |record| record.other
```
            ^^^^^^


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
  (Stmt.type_anno
    (name "main")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "identity")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "process")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "get_data")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "get_data"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "handle_result")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "handle_result"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "map")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "map"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "transform")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "transform"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
