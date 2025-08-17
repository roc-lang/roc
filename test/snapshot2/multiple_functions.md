# META
~~~ini
description=Multiple function definitions
type=file
~~~

# SOURCE
~~~roc
module [add, multiply]
add = |a, b| a + b
multiply = |x, y| x * y
~~~

# TOKENS
~~~zig
KwModule(0-6),OpenSquare(7-8),LowerIdent(8-11),Comma(11-12),LowerIdent(13-21),CloseSquare(21-22),LowerIdent(23-26),OpAssign(27-28),OpBar(29-30),LowerIdent(30-31),Comma(31-32),LowerIdent(33-34),OpBar(34-35),LowerIdent(36-37),OpPlus(38-39),LowerIdent(40-41),LowerIdent(42-50),OpAssign(51-52),OpBar(53-54),LowerIdent(54-55),Comma(55-56),LowerIdent(57-58),OpBar(58-59),LowerIdent(60-61),OpStar(62-63),LowerIdent(64-65),EndOfFile(65-65)
~~~

# PARSE_AST2
~~~clojure
(file
  (module-header
    (exposes (lc "add" @8), (lc "multiply" @13))
  )
  (statements
    (binop_equals @27
      lhs: (lc "a" @30)
      rhs: (lc "a" @30)
    (binop_equals @51
      lhs: (lc "multiply" @13)
      rhs: (lc "add" @23)
  )
)

~~~
