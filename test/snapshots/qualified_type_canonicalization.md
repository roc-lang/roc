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
      (record_literal
        (binop_colon
          (lc "r")
          (num_literal_i32 255)
        )
        (binop_colon
          (lc "g")
          (num_literal_i32 0)
        )
        (binop_colon
          (lc "b")
          (num_literal_i32 0)
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
          (record_literal
            (binop_colon
              (lc "r")
              (num_literal_i32 0)
            )
            (binop_colon
              (lc "g")
              (num_literal_i32 255)
            )
            (binop_colon
              (lc "b")
              (num_literal_i32 0)
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
        (match
          (scrutinee             (lc "result")
))
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
module [Color, ModuleA]

.
ModuleB.TypeC
Result
ExternalModule
]

import Basics.Result
import Color
import ModuleA.ModuleB exposing [TypeC]
import ExternalModule as ExtMod

# Simple qualified type
simpleQualified : Color.RGB
simpleQualified = Color.RGB({ r : 255, g : 0, b : 0 })

# Aliased qualified type
aliasedQualified : ExtMod.DataType
aliasedQualified = ExtMod.DataType | Default

# Multi-level qualified type
multiLevelQualified : ModuleA.ModuleB | TypeC
multiLevelQualified = TypeC.new

# Using qualified type with generics
resultType : Result.Result((I32, Str))
resultType = Result.Ok(42)

# Function returning qualified type
getColor : {} -> Color.RGB
getColor = \_ -> Color.RGB({ r : 0, g : 255, b : 0 })

# Function accepting qualified type
processColor : Color.RGB -> Str
processColor = \color -> "Color processed"

# Multiple qualified types in a function signature
transform : Result.Result((Color.RGB, ExtMod.Error)) -> ModuleA.ModuleB | TypeC
transform = \result -> match result
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
at 42:24 to 42:24

**Parse Error**
at 43:25 to 43:25

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.frac_literal_big)
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_plus)
  (Expr.binop_plus)
  (Expr.binop_plus)
  (Expr.binop_plus)
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
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
