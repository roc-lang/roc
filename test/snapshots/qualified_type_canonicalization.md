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
(module-header
  (exposes
    (uc "Color")

    (uc "ModuleA")
))
~~~
# FORMATTED
~~~roc
module [Color, ModuleA]

.ModuleB.TypeC,
    Result,
    ExternalModule,
]

import Basics.Result
import Color
import ModuleA.ModuleB exposing [TypeC]
import ExternalModule as ExtMod
simpleQualified : Color.RGB
simpleQualified = Color.RGB({ r : 255, g : 0, b : 0 })
aliasedQualified : ExtMod.DataType
aliasedQualified = (ExtMod.DataType | Default)
multiLevelQualified : ModuleA.ModuleB | TypeC
multiLevelQualified = TypeC.new
resultType : Result.Result((I32, Str))
resultType = Result.Ok(42)
getColor : {} -> Color.RGB
getColor = |_| Color.RGB({ r : 0, g : 255, b : 0 })
processColor : Color.RGB -> Str
processColor = |color| "Color processed"
transform : Result.Result((Color.RGB, ExtMod.Error)) -> ModuleA.ModuleB | TypeC
transform = |result| match result
	Result.Ok(rgb) => TypeC.fromColor(rgb)
	Result.Err(err) => TypeC.default
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 3:12

**Parse Error**
at 3:12 to 3:13

**Parse Error**
at 3:26 to 4:5

**Parse Error**
at 4:11 to 5:5

**Parse Error**
at 5:19 to 6:1

**Parse Error**
at 6:1 to 8:1

**Unsupported Node**
at 8:1 to 8:21

**Unsupported Node**
at 9:1 to 9:13

**Unsupported Node**
at 10:1 to 10:40

**Unsupported Node**
at 11:1 to 11:32

**Unsupported Node**
at 19:20 to 19:26

**Unsupported Node**
at 19:26 to 19:35

**Unsupported Node**
at 22:23 to 22:30

**Unsupported Node**
at 22:30 to 22:38

**Unsupported Node**
at 23:23 to 23:28

**Unsupported Node**
at 39:55 to 39:62

**Unsupported Node**
at 39:62 to 39:70

**Unsupported Node**
at 42:24 to 42:26

**Unsupported Node**
at 43:9 to 43:24

**Unsupported Node**
at 43:28 to 43:33

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
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
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "simpleQualified")
    (Expr.apply_ident)
  )
  (Expr.binop_colon
    (Expr.lookup "aliasedQualified")
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "aliasedQualified")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "multiLevelQualified")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "multiLevelQualified")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "resultType")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "resultType")
    (Expr.apply_ident)
  )
  (Expr.binop_colon
    (Expr.lookup "getColor")
    (Expr.binop_thin_arrow
      (Expr.record_literal
      )
      (Expr.module_access
        (Expr.malformed)
        (Expr.malformed)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "getColor")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "processColor")
    (Expr.binop_thin_arrow
      (Expr.module_access
        (Expr.malformed)
        (Expr.malformed)
      )
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "processColor")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "transform")
    (Expr.binop_thin_arrow
      (Expr.apply_ident)
      (Expr.lambda)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "transform")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
simpleQualified : _a
aliasedQualified : _a
multiLevelQualified : _a
resultType : _a
getColor : _a
processColor : _a
transform : _a
~~~
