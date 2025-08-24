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
  (if_without_else <26 branches>)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
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
MALFORMED TYPE - record_different_fields_reserved_error.md:3:11:3:12
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - record_different_fields_reserved_error.md:6:10:6:19
UNDEFINED VARIABLE - record_different_fields_reserved_error.md:7:9:7:19
# PROBLEMS
**Parse Error**
at 2:7 to 2:7

**Parse Error**
at 2:5 to 2:9

**Parse Error**
at 4:5 to 4:5

**Parse Error**
at 5:5 to 5:5

**Parse Error**
at 6:5 to 6:5

**Parse Error**
at 7:5 to 7:5

**Parse Error**
at 8:1 to 8:1

**Parse Error**
at 1:1 to 8:2

**Unsupported Node**
at 2:5 to 8:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
