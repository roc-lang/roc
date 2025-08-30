# META
~~~ini
description=Comprehensive type scope validation - built-ins, user types, redeclaration, forward refs
type=file
~~~
# SOURCE
~~~roc
module [MyU64, Person, Result, Tree, Node]

# Built-in types should work
MyU64 : U64
MyString : Str
MyBool : Bool

# Simple user-defined type
Person : { name: Str, age: U64 }

# Type with parameters
Result(ok, err) : [Ok(ok), Err(err)]

# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]

# Node definition comes after Tree
Node(a) : { value: a, children: List(Tree(a)) }

# Using a previously defined type
MyResult : Result(Str, U64)

# Type redeclaration (should error)
Person : U64

# Using an undeclared type (should error)
BadType : SomeUndeclaredType

# Using built-in types with parameters
MyList : List(Str)
MyDict : Dict(Str, U64)

# Complex nested type using multiple declared types
Complex : {
    person: Person,
    result: Result(Bool, Str),
    tree: Tree(U64)
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseCurly UpperIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent OpenRound UpperIdent CloseRound UpperIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound Comma LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "MyU64")

    (uc "Person")

    (uc "Result")

    (uc "Tree")

    (uc "Node")
))
~~~
# FORMATTED
~~~roc
module [MyU64, Person, Result, Tree, Node]

MyU64 : U64
MyString : Str
MyBool : Bool
Person : {name : Str, age : U64}
Result((ok, err)) : [Ok(ok), Err(err)]
Tree(a) : [Branch(Node(a)), Leaf(a)]
Node(a) : {value : a, children : List Tree a}
MyResult : Result(Str, U64)
Person : U64
BadType : SomeUndeclaredType
MyList : List Str
MyDict : Dict(Str, U64)
Complex : {person : Person, result : Result(Bool, Str), tree : Tree U64}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "name")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "age")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "value")
        (Expr.lookup "a")
      )
      (Expr.binop_colon
        (Expr.lookup "children")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "person")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "result")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "tree")
        (Expr.apply_tag)
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
