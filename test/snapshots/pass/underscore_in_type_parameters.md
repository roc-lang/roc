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
KwModule OpenSquare CloseSquare BlankLine LineComment UpperIdent OpenRound Underscore CloseRound OpColon UpperIdent BlankLine LineComment UpperIdent OpenRound Underscore Comma LowerIdent CloseRound OpColon LowerIdent BlankLine LineComment UpperIdent OpenRound LowerIdent Comma Underscore CloseRound OpColon LowerIdent BlankLine LineComment UpperIdent OpenRound Underscore Comma LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon LowerIdent CloseCurly BlankLine LineComment UpperIdent OpenRound Underscore Comma Underscore Comma LowerIdent CloseRound OpColon LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
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

# Type with underscore in parameter position
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
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:4:8:4:9
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:7:9:7:10
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:10:12:10:13
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:13:13:13:14
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:16:11:16:12
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:16:14:16:15
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 39
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
~~~
# TYPES
~~~roc
~~~
