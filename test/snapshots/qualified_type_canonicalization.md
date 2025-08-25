# META
~~~ini
description=Qualified type canonicalization
type=file
~~~
# SOURCE
~~~roc
module [
    Color,
    ModuleA.ModuleB.TypeC,
    Result,
    ExternalModule,
]

import Basics.Result
import Color
import ModuleA.ModuleB exposing [TypeC]
import ExternalModule as ExtMod

# Simple qualified type
simpleQualified : Color.RGB
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })

# Aliased qualified type
aliasedQualified : ExtMod.DataType
aliasedQualified = ExtMod.DataType.Default

# Multi-level qualified type
multiLevelQualified : ModuleA.ModuleB.TypeC
multiLevelQualified = TypeC.new

# Using qualified type with generics
resultType : Result.Result(I32, Str)
resultType = Result.Ok(42)

# Function returning qualified type
getColor : {} -> Color.RGB
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })

# Function accepting qualified type
processColor : Color.RGB -> Str
processColor = |color|
    "Color processed"

# Multiple qualified types in a function signature
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
transform = |result|
    match result {
        Result.Ok(rgb) => TypeC.fromColor(rgb)
        Result.Err(err) => TypeC.default
    }
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma UpperIdent Dot UpperIdent Dot UpperIdent Comma UpperIdent Comma UpperIdent Comma CloseSquare KwImport UpperIdent Dot UpperIdent KwImport UpperIdent KwImport UpperIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare KwImport UpperIdent KwAs UpperIdent LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma LowerIdent OpColon Int CloseCurly CloseRound LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent LowerIdent OpColon UpperIdent Dot UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpColon UpperIdent Dot UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound Int CloseRound LowerIdent OpColon OpenCurly CloseCurly OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar Underscore OpBar UpperIdent Dot UpperIdent OpenRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma LowerIdent OpColon Int CloseCurly CloseRound LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon UpperIdent Dot UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent CloseRound OpArrow UpperIdent Dot UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (binop_pipe
    (uc "ModuleB")
    (uc "TypeC")
  )
  (malformed malformed:expr_unexpected_token)
  (uc "Result")
  (malformed malformed:expr_unexpected_token)
  (uc "ExternalModule")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (import
    (uc "Basics")
    (uc "Result")
  )
  (import
    (uc "Color")
  )
  (import
    (uc "ModuleA")
    (uc "ModuleB")
    (uc "TypeC")
  )
  (import
    (uc "ExternalModule")
    (uc "ExtMod")
  )
  (binop_colon
    (lc "simpleQualified")
    (binop_pipe
      (uc "Color")
      (uc "RGB")
    )
  )
  (binop_equals
    (lc "simpleQualified")
    (apply_anon
      (binop_pipe
        (uc "Color")
        (uc "RGB")
      )
      (block
        (binop_colon
          (lc "r")
          (binop_colon
            (tuple_literal
              (binop_colon
                (tuple_literal
                  (num_literal_i32 255)
                  (lc "g")
                )
                (num_literal_i32 0)
              )
              (lc "b")
            )
            (num_literal_i32 0)
          )
        )
      )
    )
  )
  (binop_colon
    (lc "aliasedQualified")
    (binop_pipe
      (uc "ExtMod")
      (uc "DataType")
    )
  )
  (binop_equals
    (lc "aliasedQualified")
    (binop_pipe
      (binop_pipe
        (uc "ExtMod")
        (uc "DataType")
      )
      (uc "Default")
    )
  )
  (binop_colon
    (lc "multiLevelQualified")
    (binop_pipe
      (binop_pipe
        (uc "ModuleA")
        (uc "ModuleB")
      )
      (uc "TypeC")
    )
  )
  (binop_equals
    (lc "multiLevelQualified")
    (binop_pipe
      (uc "TypeC")
      (dot_lc "new")
    )
  )
  (binop_colon
    (lc "resultType")
    (apply_anon
      (binop_pipe
        (uc "Result")
        (uc "Result")
      )
      (tuple_literal
        (uc "I32")
        (uc "Str")
      )
    )
  )
  (binop_equals
    (lc "resultType")
    (apply_anon
      (binop_pipe
        (uc "Result")
        (uc "Ok")
      )
      (num_literal_i32 42)
    )
  )
  (binop_colon
    (lc "getColor")
    (binop_thin_arrow
      (record_literal)
      (binop_pipe
        (uc "Color")
        (uc "RGB")
      )
    )
  )
  (binop_equals
    (lc "getColor")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "Color")
            (uc "RGB")
          )
          (block
            (binop_colon
              (lc "r")
              (binop_colon
                (tuple_literal
                  (binop_colon
                    (tuple_literal
                      (num_literal_i32 0)
                      (lc "g")
                    )
                    (num_literal_i32 255)
                  )
                  (lc "b")
                )
                (num_literal_i32 0)
              )
            )
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
  (binop_colon
    (lc "processColor")
    (binop_thin_arrow
      (binop_pipe
        (uc "Color")
        (uc "RGB")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "processColor")
    (lambda
      (body
        (str_literal_big "Color processed")
      )
      (args
        (lc "color")
      )
    )
  )
  (binop_colon
    (lc "transform")
    (binop_thin_arrow
      (apply_anon
        (binop_pipe
          (uc "Result")
          (uc "Result")
        )
        (tuple_literal
          (binop_pipe
            (uc "Color")
            (uc "RGB")
          )
          (binop_pipe
            (uc "ExtMod")
            (uc "Error")
          )
        )
      )
      (binop_pipe
        (binop_pipe
          (uc "ModuleA")
          (uc "ModuleB")
        )
        (uc "TypeC")
      )
    )
  )
  (binop_equals
    (lc "transform")
    (lambda
      (body
        (match <137 branches>)
      )
      (args
        (lc "result")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	Color,
	ModuleA.ModuleB.TypeC,
	Result,
	ExternalModule,
]

<malformed>ModuleB.TypeC<malformed>
Result<malformed>
ExternalModule<malformed>
<malformed>

import Basics exposing [Result]
import Color
import ModuleA exposing [ModuleB, TypeC]
import ExternalModule exposing [ExtMod]

# Simple qualified type
simpleQualified: Color.RGB
simpleQualified = Color.RGB({
	r: (((255, g): 0, b): 0)
})

# Aliased qualified type
aliasedQualified: ExtMod.DataType
aliasedQualified = (ExtMod.DataType) | Default

# Multi-level qualified type
multiLevelQualified: (ModuleA.ModuleB) | TypeC
multiLevelQualified = TypeC | .new

# Using qualified type with generics
resultType: Result.Result((I32, Str))
resultType = Result.Ok(42)
getColor: ({  } -> Color.RGB)
getColor = \_ -> Color.RGB({
	r: (((0, g): 255, b): 0)
})

# Function accepting qualified type
processColor: (Color.RGB -> Str)
processColor = \color -> "Color processed"

# Multiple qualified types in a function signature
transform: (Result.Result((Color.RGB, ExtMod.Error)) -> (ModuleA.ModuleB) | TypeC)
transform = \result -> when result is {
	Result.Ok(rgb)
	<malformed>
	TypeC | .fromColor(rgb)
	Result.Err(err)
	<malformed>
	TypeC | .default
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 3:12

**Parse Error**
at 3:12 to 3:12

**Parse Error**
at 3:26 to 3:26

**Parse Error**
at 4:11 to 4:11

**Parse Error**
at 5:19 to 5:19

**Parse Error**
at 6:1 to 6:1

**Parse Error**
at 41:5 to 41:18

**Parse Error**
at 42:24 to 42:24

**Parse Error**
at 43:25 to 43:25

**Parse Error**
at 41:5 to 44:6

**Parse Error**
at 44:6 to 44:6

**Unsupported Node**
at 3:12 to 3:12

**Unsupported Node**
at 3:13 to 3:20

**Unsupported Node**
at 3:26 to 3:26

**Unsupported Node**
at 4:11 to 4:11

**Unsupported Node**
at 5:19 to 5:19

**Unsupported Node**
at 6:1 to 6:1

**Unsupported Node**
at 8:1 to 8:21

**Unsupported Node**
at 9:1 to 9:13

**Unsupported Node**
at 10:1 to 10:39

**Unsupported Node**
at 11:1 to 11:32

**Unsupported Node**
at 14:19 to 14:24

**Unsupported Node**
at 15:19 to 15:24

**Unsupported Node**
at 15:46 to 15:47

**Unsupported Node**
at 18:20 to 18:26

**Unsupported Node**
at 19:20 to 19:26

**Unsupported Node**
at 19:26 to 19:34

**Unsupported Node**
at 22:23 to 22:30

**Unsupported Node**
at 22:30 to 22:37

**Unsupported Node**
at 23:23 to 23:28

**Unsupported Node**
at 26:14 to 26:20

**Unsupported Node**
at 27:14 to 27:20

**Unsupported Node**
at 30:12 to 30:26

**Unsupported Node**
at 31:12 to 31:16

**Unsupported Node**
at 34:16 to 34:32

**Unsupported Node**
at 35:16 to 36:5

**Unsupported Node**
at 39:13 to 39:75

**Unsupported Node**
at 40:13 to 41:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.lambda)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "simpleQualified")
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "aliasedQualified")
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "multiLevelQualified")
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "resultType")
    (Expr.apply_ident)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "getColor")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "processColor")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "transform")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
