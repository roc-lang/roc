# META
~~~ini
description=Basic tag union type canonicalization
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

process : [Some(Str), None] -> Str
process = |maybe| "result"

is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result {
    Ok(_) => True
    Err(_) => False
}

is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = |result| match result {
    Ok(_) => Bool.True
    Err(_) => Bool.False
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent CloseCurly BlankLine LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

process : [Some(Str), None] -> Str
process = |maybe| "result"

is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result
	Ok(_) => True
	Err(_) => False

is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = |result| match result
	Ok(_) => Bool

Err(_)
=> 
Bool.False
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **True
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:8:14:9:5:**
```roc
    Ok(_) => True
    Err(_) => False
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **False
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:9:15:10:1:**
```roc
    Err(_) => False
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Bool** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:14:14:14:18:**
```roc
    Ok(_) => Bool.True
```
             ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **.** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**type_tag_union_basic.md:14:18:14:19:**
```roc
    Ok(_) => Bool.True
```
                 ^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**type_tag_union_basic.md:14:19:15:5:**
```roc
    Ok(_) => Bool.True
    Err(_) => Bool.False
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:15:12:15:15:**
```roc
    Err(_) => Bool.False
```
           ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:16:1:18:1:**
```roc
}

main! = |_| {}
```


**UNUSED VARIABLE**
Variable **maybe** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_maybe` to suppress this warning.
The unused variable is declared here:

**type_tag_union_basic.md:4:12:4:17:**
```roc
process = |maybe| "result"
```
           ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_tag_union_basic.md:8:5:9:5:**
```roc
    Ok(_) => True
    Err(_) => False
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_tag_union_basic.md:14:5:14:18:**
```roc
    Ok(_) => Bool.True
```
    ^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_tag_union_basic.md:15:5:15:11:**
```roc
    Err(_) => Bool.False
```
    ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_tag_union_basic.md:15:12:15:15:**
```roc
    Err(_) => Bool.False
```
           ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_tag_union_basic.md:15:15:15:25:**
```roc
    Err(_) => Bool.False
```
              ^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_tag_union_basic.md:16:1:18:1:**
```roc
}

main! = |_| {}
```


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
  (Stmt.type_anno
    (name "is_ok_ret_unqualified_bool")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "is_ok_ret_unqualified_bool"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "is_ok_ret_bool")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "is_ok_ret_bool"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
