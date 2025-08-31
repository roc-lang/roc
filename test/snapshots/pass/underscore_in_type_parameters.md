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
