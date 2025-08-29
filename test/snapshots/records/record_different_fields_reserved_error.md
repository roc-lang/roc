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
if :  (
	((((("conditional", when) : "pattern match") : "test assertion") : "module load") : Bool.true) : Bool.false,
)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:7 to 2:9

**Parse Error**
at 2:5 to 2:9

**Parse Error**
at 4:5 to 4:11

**Parse Error**
at 5:5 to 5:11

**Parse Error**
at 6:5 to 6:8

**Parse Error**
at 7:5 to 7:7

**Parse Error**
at 8:1 to 8:2

**Parse Error**
at 1:1 to 8:2

**Unsupported Node**
at 6:10 to 6:14

**Unsupported Node**
at 7:9 to 7:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.if_else)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
