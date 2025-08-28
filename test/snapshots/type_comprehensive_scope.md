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
(block
  (binop_colon
    (uc "MyU64")
    (uc "U64")
  )
  (binop_colon
    (uc "MyString")
    (uc "Str")
  )
  (binop_colon
    (uc "MyBool")
    (uc "Bool")
  )
  (binop_colon
    (uc "Person")
    (record_literal
      (binop_colon
        (lc "name")
        (uc "Str")
      )
      (binop_colon
        (lc "age")
        (uc "U64")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Result")
      (tuple_literal
        (lc "ok")
        (lc "err")
      )
    )
    (list_literal
      (apply_uc
        (uc "Ok")
        (lc "ok")
      )
      (apply_uc
        (uc "Err")
        (lc "err")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Tree")
      (lc "a")
    )
    (list_literal
      (apply_uc
        (uc "Branch")
        (apply_uc
          (uc "Node")
          (lc "a")
        )
      )
      (apply_uc
        (uc "Leaf")
        (lc "a")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Node")
      (lc "a")
    )
    (record_literal
      (binop_colon
        (lc "value")
        (lc "a")
      )
      (binop_colon
        (lc "children")
        (apply_uc
          (uc "List")
          (apply_uc
            (uc "Tree")
            (lc "a")
          )
        )
      )
    )
  )
  (binop_colon
    (uc "MyResult")
    (apply_uc
      (uc "Result")
      (tuple_literal
        (uc "Str")
        (uc "U64")
      )
    )
  )
  (binop_colon
    (uc "Person")
    (uc "U64")
  )
  (binop_colon
    (uc "BadType")
    (uc "SomeUndeclaredType")
  )
  (binop_colon
    (uc "MyList")
    (apply_uc
      (uc "List")
      (uc "Str")
    )
  )
  (binop_colon
    (uc "MyDict")
    (apply_uc
      (uc "Dict")
      (tuple_literal
        (uc "Str")
        (uc "U64")
      )
    )
  )
  (binop_colon
    (uc "Complex")
    (record_literal
      (binop_colon
        (lc "person")
        (uc "Person")
      )
      (binop_colon
        (lc "result")
        (apply_uc
          (uc "Result")
          (tuple_literal
            (uc "Bool")
            (uc "Str")
          )
        )
      )
      (binop_colon
        (lc "tree")
        (apply_uc
          (uc "Tree")
          (uc "U64")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	MyU64,
	Person,
	Result,
	Tree,
	Node,
]

MyU64 : U64
MyString : Str
MyBool : Bool
Person : {name : Str, age : U64}
Result((ok, err)) : [Ok(ok), Err(err)]
Tree(a) : [Branch(Node(a)), Leaf(a)]
Node(a) : {value : a, children : List Tree a}
MyResult : Result (Str, U64)
Person : U64
BadType : SomeUndeclaredType
MyList : List Str
MyDict : Dict (Str, U64)
Complex : {person : Person, result : Result (Bool, Str), tree : Tree U64}
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
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
