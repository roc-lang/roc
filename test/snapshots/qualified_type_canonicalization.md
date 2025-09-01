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
module [Color, ModuleA].
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

# Simple qualified type
simpleQualified : Color.RGB
simpleQualified = Color.RGB({ r : 255, g : 0, b : 0 })

# Aliased qualified type
aliasedQualified : ExtMod.DataType
aliasedQualified = (ExtMod.DataType | Default)

# Multi-level qualified type
multiLevelQualified : ModuleA.ModuleB | TypeC
multiLevelQualified = TypeC.new

# Using qualified type with generics
resultType : Result.Result((I32, Str))
resultType = Result.Ok(42)

# Function returning qualified type
getColor : {} -> Color.RGB
getColor = |_| Color.RGB({ r : 0, g : 255, b : 0 })

# Function accepting qualified type
processColor : Color.RGB -> Str
processColor = |color| "Color processed"

# Multiple qualified types in a function signature
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

**qualified_type_canonicalization.md:3:12:3:13:**
```roc
    ModuleA.ModuleB.TypeC,
```
           ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:3:13:3:26:**
```roc
    ModuleA.ModuleB.TypeC,
```
            ^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:3:26:4:5:**
```roc
    ModuleA.ModuleB.TypeC,
    Result,
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:4:5:4:11:**
```roc
    Result,
```
    ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:4:11:5:5:**
```roc
    Result,
    ExternalModule,
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:5:5:5:19:**
```roc
    ExternalModule,
```
    ^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:5:19:6:1:**
```roc
    ExternalModule,
]
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:6:1:8:1:**
```roc
]

import Basics.Result
```


**UNDEFINED VARIABLE**
Nothing is named **r** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:15:31:15:32:**
```roc
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })
```
                              ^


**UNDEFINED VARIABLE**
Nothing is named **g** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:15:39:15:40:**
```roc
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })
```
                                      ^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:15:45:15:46:**
```roc
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })
```
                                            ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:19:20:19:43:**
```roc
aliasedQualified = ExtMod.DataType.Default
```
                   ^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **TypeC.new** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:23:23:23:32:**
```roc
multiLevelQualified = TypeC.new
```
                      ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **r** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:31:28:31:29:**
```roc
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })
```
                           ^


**UNDEFINED VARIABLE**
Nothing is named **g** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:31:34:31:35:**
```roc
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })
```
                                 ^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:31:42:31:43:**
```roc
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })
```
                                         ^


**UNUSED VARIABLE**
Variable **color** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_color` to suppress this warning.
The unused variable is declared here:

**qualified_type_canonicalization.md:35:17:35:22:**
```roc
processColor = |color|
```
                ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:42:16:42:23:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
               ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:42:24:42:27:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
                       ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:42:27:42:47:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
                          ^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:43:9:43:24:**
```roc
        Result.Err(err) => TypeC.default
```
        ^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:43:25:43:28:**
```roc
        Result.Err(err) => TypeC.default
```
                        ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:43:28:43:41:**
```roc
        Result.Err(err) => TypeC.default
```
                           ^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_type_canonicalization.md:44:5:44:6:**
```roc
    }
```
    ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.type_anno
    (name "simpleQualified")
    (type binop_pipe)
  )
  (Stmt.assign
    (pattern (Patt.ident "simpleQualified"))
    (Expr.apply_ident)
  )
  (Stmt.type_anno
    (name "aliasedQualified")
    (type binop_pipe)
  )
  (Stmt.assign
    (pattern (Patt.ident "aliasedQualified"))
    (Expr.malformed)
  )
  (Stmt.type_anno
    (name "multiLevelQualified")
    (type binop_pipe)
  )
  (Stmt.assign
    (pattern (Patt.ident "multiLevelQualified"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Stmt.type_anno
    (name "resultType")
    (type apply_anon)
  )
  (Stmt.assign
    (pattern (Patt.ident "resultType"))
    (Expr.apply_ident)
  )
  (Stmt.type_anno
    (name "getColor")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "getColor"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "processColor")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processColor"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "transform")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "transform"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
