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
if : (((((("conditional", when) : "pattern match") : "test assertion") : "module load") : Bool.true) : Bool.false)
~~~
# EXPECTED
NIL
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

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.unary_neg)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
