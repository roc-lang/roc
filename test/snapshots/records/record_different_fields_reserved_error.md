# META
~~~ini
description=Record with reserved keyword fields (error case)
type=expr
~~~
# SOURCE
~~~roc
{
    if: "conditional",
    when: "pattern match",
    expect: "test assertion",
    import: "module load",
    and: Bool.true,
    or: Bool.false,
}
~~~
# TOKENS
~~~text
OpenCurly KwIf OpColon String Comma LowerIdent OpColon String Comma KwExpect OpColon String Comma KwImport OpColon String Comma OpAnd OpColon UpperIdent Dot LowerIdent Comma OpOr OpColon UpperIdent Dot LowerIdent Comma CloseCurly ~~~
# PARSE
~~~clojure
(block
  (if_without_else
    (condition       (malformed)
)
    (then       (binop_colon
        (tuple_literal
          (binop_colon
            (tuple_literal
              (binop_colon
                (tuple_literal
                  (binop_colon
                    (tuple_literal
                      (binop_colon
                        (tuple_literal
                          (str_literal_big "conditional")
                          (lc "when")
                        )
                        (str_literal_big "pattern match")
                      )
                      (malformed)
                    )
                    (str_literal_big "test assertion")
                  )
                  (malformed)
                )
                (str_literal_big "module load")
              )
              (malformed)
            )
            (binop_pipe
              (uc "Bool")
              (dot_lc "true")
            )
          )
          (malformed)
        )
        (binop_pipe
          (uc "Bool")
          (dot_lc "false")
        )
      )
))
)
~~~
# FORMATTED
~~~roc
if : ((((("conditional", when) : "pattern match") : "test assertion") : "module load") : Bool.true) : Bool.false
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:2:7:2:8
IF WITHOUT ELSE - record_different_fields_reserved_error.md:2:5:2:7
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_reserved_error.md:3:11:3:12
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:3:12:3:25
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:3:25:3:26
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:3:26:3:27
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:4:11:4:12
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:4:29:4:30
IMPORT MUST BE TOP LEVEL - record_different_fields_reserved_error.md:5:5:5:11
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:5:11:5:12
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:5:26:5:27
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:6:5:6:8
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:6:8:6:9
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:6:19:6:20
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:5:7:7
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:7:7:8
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:19:7:20
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:2:5:2:23
MALFORMED TYPE - record_different_fields_reserved_error.md:3:11:3:12
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:3:12:3:25
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:3:25:3:26
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:3:26:3:27
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:4:11:4:12
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:4:29:4:30
NOT IMPLEMENTED - :0:0:0:0
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:5:11:5:12
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:5:26:5:27
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:6:5:6:8
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:6:8:6:9
UNDEFINED VARIABLE - record_different_fields_reserved_error.md:6:10:6:19
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:6:19:6:20
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:7:5:7:7
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:7:7:7:8
UNDEFINED VARIABLE - record_different_fields_reserved_error.md:7:9:7:19
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:7:19:7:20
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **: ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:2:7:2:9:**
```roc
    if: "conditional",
```
      ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **expect** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:4:5:4:11:**
```roc
    expect: "test assertion",
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **import** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:5:5:5:11:**
```roc
    import: "module load",
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **and** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:6:5:6:8:**
```roc
    and: Bool.true,
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **or** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:7:5:7:7:**
```roc
    or: Bool.false,
```
    ^^


**STATEMENT IN EXPRESSION CONTEXT**
Found a statement where an expression was expected.
Statements like **return**, **dbg**, or **expect** cannot be used in expression contexts.

**record_different_fields_reserved_error.md:2:9:7:19:**
```roc
    if: "conditional",
    when: "pattern match",
    expect: "test assertion",
    import: "module load",
    and: Bool.true,
    or: Bool.false,
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.if_else)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
