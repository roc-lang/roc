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
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:4:8:4:9
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:7:9:7:10
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:10:12:10:13
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:13:13:13:14
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:16:11:16:12
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:16:14:16:15
# PROBLEMS
**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**underscore_in_type_parameters.md:13:21:13:33:**
```roc
ComplexType(_, b) : { field: b }
```
                    ^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name node:apply_uc)
    (type uc)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type lc)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type lc)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type block)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type lc)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
