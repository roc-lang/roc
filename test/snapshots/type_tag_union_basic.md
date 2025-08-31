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

**type_tag_union_basic.md:9:5:9:11:**
```roc
    Err(_) => False
```
    ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_tag_union_basic.md:14:5:14:18:**
```roc
    Ok(_) => Bool.True
```
    ^^^^^^^^^^^^^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**type_tag_union_basic.md:15:9:15:10:**
```roc
    Err(_) => Bool.False
```
        ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.binop_thin_arrow
      (Expr.list_literal)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "process")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "is_ok_ret_unqualified_bool")
    (Expr.binop_thin_arrow
      (Expr.list_literal)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "is_ok_ret_unqualified_bool")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "is_ok_ret_bool")
    (Expr.binop_thin_arrow
      (Expr.list_literal)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "is_ok_ret_bool")
    (Expr.lambda)
  )
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
process : _a
is_ok_ret_unqualified_bool : _a
is_ok_ret_bool : _a
~~~
