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
    (block
      (binop_colon
        (lc "name")
        (binop_colon
          (tuple_literal
            (uc "Str")
            (lc "age")
          )
          (uc "U64")
        )
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
      (tuple_literal
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
  )
  (binop_colon
    (apply_uc
      (uc "Tree")
      (lc "a")
    )
    (list_literal
      (tuple_literal
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
  )
  (binop_colon
    (apply_uc
      (uc "Node")
      (lc "a")
    )
    (block
      (binop_colon
        (lc "value")
        (binop_colon
          (tuple_literal
            (lc "a")
            (lc "children")
          )
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
    (block
      (binop_colon
        (lc "person")
        (binop_colon
          (tuple_literal
            (binop_colon
              (tuple_literal
                (uc "Person")
                (lc "result")
              )
              (apply_uc
                (uc "Result")
                (tuple_literal
                  (uc "Bool")
                  (uc "Str")
                )
              )
            )
            (lc "tree")
          )
          (apply_uc
            (uc "Tree")
            (uc "U64")
          )
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
TYPE REDECLARED - type_comprehensive_scope.md:12:1:12:37
UNDECLARED TYPE - type_comprehensive_scope.md:15:19:15:23
TYPE REDECLARED - type_comprehensive_scope.md:24:1:24:13
UNDECLARED TYPE - type_comprehensive_scope.md:27:11:27:29
# PROBLEMS
**Pattern in Expression Context**
at 4:1 to 4:6

**Pattern in Expression Context**
at 4:9 to 4:12

**Pattern in Expression Context**
at 5:1 to 5:9

**Pattern in Expression Context**
at 5:12 to 5:15

**Pattern in Expression Context**
at 6:1 to 6:7

**Pattern in Expression Context**
at 6:10 to 6:14

**Pattern in Expression Context**
at 9:1 to 9:7

**Unsupported Node**
at 9:26 to 9:27

**Pattern in Expression Context**
at 9:28 to 9:31

**Unsupported Node**
at 12:19 to 13:1

**Unsupported Node**
at 15:11 to 16:1

**Unsupported Node**
at 18:31 to 18:32

**Pattern in Expression Context**
at 21:1 to 21:9

**Pattern in Expression Context**
at 24:1 to 24:7

**Pattern in Expression Context**
at 24:10 to 24:13

**Pattern in Expression Context**
at 27:1 to 27:8

**Pattern in Expression Context**
at 27:11 to 27:29

**Pattern in Expression Context**
at 30:1 to 30:7

**Pattern in Expression Context**
at 31:1 to 31:7

**Pattern in Expression Context**
at 34:1 to 34:8

**Unsupported Node**
at 37:9 to 37:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "name")
        (Expr.binop_colon
          (Expr.malformed)
          (Expr.malformed)
        )
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "value")
        (Expr.binop_colon
          (Expr.malformed)
          (Expr.apply_tag)
        )
      )
    )
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "person")
        (Expr.binop_colon
          (Expr.malformed)
          (Expr.apply_tag)
        )
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
