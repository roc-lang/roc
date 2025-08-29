# META
~~~ini
description=Type parameters should not contain underscores (_) as they represent 'I don't care' types, which doesn't make sense when declaring a type.
type=file
~~~
# SOURCE
~~~roc
module []

# Type with underscore in parameter position
MyType(_) : Str

# Type with underscore and regular parameter
MyType2(_, b) : b

# Type with parameters where underscore comes second
MyType3(a, _) : a

# More complex type with underscore parameter
ComplexType(_, b) : { field: b }

# Type with multiple underscores
MultiType(_, _, c) : c
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpenRound Underscore CloseRound OpColon UpperIdent UpperIdent OpenRound Underscore Comma LowerIdent CloseRound OpColon LowerIdent UpperIdent OpenRound LowerIdent Comma Underscore CloseRound OpColon LowerIdent UpperIdent OpenRound Underscore Comma LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon LowerIdent CloseCurly UpperIdent OpenRound Underscore Comma Underscore Comma LowerIdent CloseRound OpColon LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (apply_uc
      (uc "MyType")
      (underscore)
    )
    (uc "Str")
  )
  (binop_colon
    (apply_uc
      (uc "MyType2")
      (tuple_literal
        (underscore)
        (lc "b")
      )
    )
    (lc "b")
  )
  (binop_colon
    (apply_uc
      (uc "MyType3")
      (tuple_literal
        (lc "a")
        (underscore)
      )
    )
    (lc "a")
  )
  (binop_colon
    (apply_uc
      (uc "ComplexType")
      (tuple_literal
        (underscore)
        (lc "b")
      )
    )
    (block
      (binop_colon
        (lc "field")
        (lc "b")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "MultiType")
      (tuple_literal
        (underscore)
        (underscore)
        (lc "c")
      )
    )
    (lc "c")
  )
)
~~~
# FORMATTED
~~~roc
module []

MyType(_) : Str
MyType2((_, b)) : b
MyType3((a, _)) : a
ComplexType((_, b)) : {
	field : b
}
MultiType((_, _, c)) : c
~~~
# EXPECTED
NIL
# PROBLEMS
**Pattern in Expression Context**
at 4:8 to 4:9

**Pattern in Expression Context**
at 7:9 to 7:10

**Pattern in Expression Context**
at 10:12 to 10:13

**Pattern in Expression Context**
at 13:13 to 13:14

**Pattern in Expression Context**
at 16:11 to 16:12

**Pattern in Expression Context**
at 16:14 to 16:15

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.lookup "b")
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.lookup "a")
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "field")
        (Expr.lookup "b")
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.lookup "c")
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~
