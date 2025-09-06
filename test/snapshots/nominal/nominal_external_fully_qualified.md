# META
~~~ini
description=Example of external nominal tag union fully qualified name
type=file
~~~
# SOURCE
~~~roc
module [handleResult]

import MyResultModule

handleResult : MyResultModule.MyResultType(Str, I32) -> Str
handleResult = |result| {
    match result {
        MyResultModule.MyResultType.Ok(value) => value
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
    }
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport UpperIdent BlankLine LowerIdent OpColon UpperIdent Dot UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent UpperIdent Dot UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow String CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "handleResult")
))
~~~
# FORMATTED
~~~roc
module [handleResult]

import MyResultModule

handleResult : MyResultModule.MyResultType((Str, I32)) -> Str
handleResult = |result| {
	match result
	MyResultType.Ok(value)
	=> 
	value : value
	MyResultModule.MyResultType | Err(code)
	=> 
	"Error: $(code.toStr())"
}

}
~~~
# EXPECTED
MODULE NOT FOUND - nominal_external_fully_qualified.md:3:1:3:22
UNUSED VARIABLE - nominal_external_fully_qualified.md:9:41:9:45
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**nominal_external_fully_qualified.md:8:23:8:24:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_external_fully_qualified.md:8:47:8:50:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                                              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_external_fully_qualified.md:9:47:9:50:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                                              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_external_fully_qualified.md:11:1:11:2:**
```roc
}
```
^


**UNDEFINED VARIABLE**
Nothing is named **MyResultType.Ok** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_external_fully_qualified.md:8:24:8:39:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                       ^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_external_fully_qualified.md:8:40:8:45:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                                       ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_external_fully_qualified.md:9:9:9:40:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **code** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_external_fully_qualified.md:9:41:9:45:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                                        ^^^^


**UNUSED VARIABLE**
Variable **value** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:

**nominal_external_fully_qualified.md:8:50:8:55:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                                                 ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_external_fully_qualified.md:11:1:11:2:**
```roc
}
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.type_anno
    (name "handleResult")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "handleResult"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
