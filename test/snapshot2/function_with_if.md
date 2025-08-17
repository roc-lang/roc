# META
~~~ini
description=Function with if-else expression
type=file
~~~

# SOURCE
~~~roc
module [isPositive]
isPositive = |n|
    if (n > 0)
        "positive"
    else
        "not positive"
~~~

# TOKENS
~~~zig
KwModule(0-6),OpenSquare(7-8),LowerIdent(8-18),CloseSquare(18-19),LowerIdent(20-30),OpAssign(31-32),OpBar(33-34),LowerIdent(34-35),OpBar(35-36),KwIf(41-43),OpenRound(44-45),LowerIdent(45-46),OpGreaterThan(47-48),Int(49-50),CloseRound(50-51),StringStart(60-61),StringPart(61-69),StringEnd(69-70),KwElse(75-79),StringStart(88-89),StringPart(89-101),StringEnd(101-102),EndOfFile(102-102)
~~~

# PARSE_AST2
~~~clojure
(file
  (module-header
    (exposes (lc "isPositive" @8))
  )
  (statements
    (binop_equals @31
      lhs: (lc "isPositive" @20)
      rhs: (lambda [(lc "n" @34)] (if_else(binop_gt (lc "n" @45) (num_literal_i32 0 @49) @47) (str_literal_big "<big>" @60) (str_literal_big "<big>" @88) @41) @33)
  )
)

~~~
