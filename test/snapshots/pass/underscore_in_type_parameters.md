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

MyType(_) : Str
MyType2((_, b)) : b
MyType3((a, _)) : a
ComplexType((_, b)) : {
	field : b
}
MultiType((_, _, c)) : c# Type with underscore in parameter position
# Type with underscore and regular parameter
# Type with parameters where underscore comes second
# More complex type with underscore parameter
# Type with multiple underscores
~~~
# EXPECTED
NIL
# PROBLEMS
**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_type_parameters.md:4:8:4:9:**
```roc
MyType(_) : Str
```
       ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_type_parameters.md:7:9:7:10:**
```roc
MyType2(_, b) : b
```
        ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_type_parameters.md:10:12:10:13:**
```roc
MyType3(a, _) : a
```
           ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_type_parameters.md:13:13:13:14:**
```roc
ComplexType(_, b) : { field: b }
```
            ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_type_parameters.md:16:11:16:12:**
```roc
MultiType(_, _, c) : c
```
          ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**underscore_in_type_parameters.md:16:14:16:15:**
```roc
MultiType(_, _, c) : c
```
             ^


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
