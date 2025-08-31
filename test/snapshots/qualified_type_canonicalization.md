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
KwModule OpenSquare UpperIdent Comma UpperIdent Dot UpperIdent Dot UpperIdent Comma UpperIdent Comma UpperIdent Comma CloseSquare BlankLine KwImport UpperIdent Dot UpperIdent KwImport UpperIdent KwImport UpperIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare KwImport UpperIdent KwAs UpperIdent BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma LowerIdent OpColon Int CloseCurly CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent OpenRound Int CloseRound BlankLine LineComment LowerIdent OpColon OpenCurly CloseCurly OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar Underscore OpBar UpperIdent Dot UpperIdent OpenRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma LowerIdent OpColon Int CloseCurly CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent CloseRound OpArrow UpperIdent Dot UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent CloseCurly ~~~
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

.
ModuleB.TypeC
,
Result
,
ExternalModule
,
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

Ok(rgb)
=> 
TypeC.fromColor(rgb)
Result.Err(err)
=> 
TypeC.default
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **header_expected_close_square**
This is an unexpected parsing error. Please check your syntax.

**qualified_type_canonicalization.md:1:1:3:12:**
```roc
module [
    Color,
    ModuleA.ModuleB.TypeC,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**qualified_type_canonicalization.md:3:12:3:13:**
```roc
    ModuleA.ModuleB.TypeC,
```
           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**qualified_type_canonicalization.md:3:26:4:5:**
```roc
    ModuleA.ModuleB.TypeC,
    Result,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**qualified_type_canonicalization.md:4:11:5:5:**
```roc
    Result,
    ExternalModule,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**qualified_type_canonicalization.md:5:19:6:1:**
```roc
    ExternalModule,
]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**qualified_type_canonicalization.md:6:1:8:1:**
```roc
]

import Basics.Result
```


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**qualified_type_canonicalization.md:42:15:42:16:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
              ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**qualified_type_canonicalization.md:42:24:42:27:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
                       ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**qualified_type_canonicalization.md:43:25:43:28:**
```roc
        Result.Err(err) => TypeC.default
```
                        ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**qualified_type_canonicalization.md:44:5:44:6:**
```roc
    }
```
    ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:8:1:8:21:**
```roc
import Basics.Result
```
^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:9:1:9:13:**
```roc
import Color
```
^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:10:1:10:40:**
```roc
import ModuleA.ModuleB exposing [TypeC]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:11:1:11:32:**
```roc
import ExternalModule as ExtMod
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:19:20:19:26:**
```roc
aliasedQualified = ExtMod.DataType.Default
```
                   ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:19:26:19:35:**
```roc
aliasedQualified = ExtMod.DataType.Default
```
                         ^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:22:23:22:30:**
```roc
multiLevelQualified : ModuleA.ModuleB.TypeC
```
                      ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:22:30:22:38:**
```roc
multiLevelQualified : ModuleA.ModuleB.TypeC
```
                             ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:23:23:23:28:**
```roc
multiLevelQualified = TypeC.new
```
                      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:39:55:39:62:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
                                                      ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:39:62:39:70:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
                                                             ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:42:27:42:32:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
                          ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:43:28:43:33:**
```roc
        Result.Err(err) => TypeC.default
```
                           ^^^^^


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
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.lambda)
  (Expr.malformed)
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
