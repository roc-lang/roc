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

# Type with underscore and regular parameter
MyType2((_, b)) : b

# Type with parameters where underscore comes second
MyType3((a, _)) : a

# More complex type with underscore parameter
ComplexType((_, b)) : {
	field : b
}

# Type with multiple underscores
MultiType((_, _, c)) : c
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~
