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
KwModule OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare BlankLine LineComment UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent BlankLine LineComment UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseCurly BlankLine LineComment UpperIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound BlankLine LineComment UpperIdent OpColon UpperIdent BlankLine LineComment UpperIdent OpColon UpperIdent BlankLine LineComment UpperIdent OpColon UpperIdent OpenRound UpperIdent CloseRound UpperIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound BlankLine LineComment UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound Comma LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound CloseCurly ~~~
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
module [MyU64, Person, Result, Tree, Node]

# Built-in types should work
MyU64 : U64
MyString : Str
MyBool : Bool
# Simple user-defined type
Person : {name: Str, age: U64}
# Type with parameters
Result((ok, err)) : [Ok(ok), Err(err)]
# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]
# Node definition comes after Tree
Node(a) : {value: a, children: List(Tree(a))}
# Using a previously defined type
MyResult : Result(Str, U64)
# Type redeclaration (should error)
Person : U64
# Using an undeclared type (should error)
BadType : SomeUndeclaredType
# Using built-in types with parameters
MyList : List Str
MyDict : Dict(Str, U64)
# Complex nested type using multiple declared types
Complex : {person: Person, result: Result((Bool, Str)), tree: Tree(U64)}
~~~
# EXPECTED
TYPE REDECLARED - type_comprehensive_scope.md:12:1:12:37
UNDECLARED TYPE - type_comprehensive_scope.md:15:19:15:23
TYPE REDECLARED - type_comprehensive_scope.md:24:1:24:13
UNDECLARED TYPE - type_comprehensive_scope.md:27:11:27:29
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
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 109
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
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 _)
(var #90 _)
(var #91 _)
(var #92 _)
(var #93 _)
(var #94 _)
(var #95 _)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 _)
(var #106 _)
(var #107 _)
(var #108 _)
~~~
# TYPES
~~~roc
~~~
