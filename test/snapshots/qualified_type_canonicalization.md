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

    (binop_dot
      (binop_dot
        (uc "ModuleA")
        (uc "ModuleB")
      )
      (uc "TypeC")
    )

    (uc "Result")

    (uc "ExternalModule")
))
(block
  (import
    (binop_dot
      (uc "Basics")
      (uc "Result")
    )
  )
  (import
    (uc "Color")
  )
  (import
    (binop_exposing
      (binop_dot
        (uc "ModuleA")
        (uc "ModuleB")
      )
      (list_literal
        (uc "TypeC")
      )
    )
  )
  (import
    (binop_as
      (uc "ExternalModule")
      (uc "ExtMod")
    )
  )
  (binop_colon
    (lc "simpleQualified")
    (binop_dot
      (uc "Color")
      (uc "RGB")
    )
  )
  (binop_equals
    (lc "simpleQualified")
    (apply_anon
      (binop_dot
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
    (binop_dot
      (uc "ExtMod")
      (uc "DataType")
    )
  )
  (binop_equals
    (lc "aliasedQualified")
    (binop_dot
      (binop_dot
        (uc "ExtMod")
        (uc "DataType")
      )
      (uc "Default")
    )
  )
  (binop_colon
    (lc "multiLevelQualified")
    (binop_dot
      (binop_dot
        (uc "ModuleA")
        (uc "ModuleB")
      )
      (uc "TypeC")
    )
  )
  (binop_equals
    (lc "multiLevelQualified")
    (binop_dot
      (uc "TypeC")
      (dot_lc "new")
    )
  )
  (binop_colon
    (lc "resultType")
    (apply_anon
      (binop_dot
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
      (binop_dot
        (uc "Result")
        (uc "Ok")
      )
      (num_literal_i32 42)
    )
  )
  (binop_colon
    (lc "getColor")
    (binop_arrow_call
      (record_literal)
      (binop_dot
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
          (binop_dot
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
    (binop_arrow_call
      (binop_dot
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
    (binop_arrow_call
      (apply_anon
        (binop_dot
          (uc "Result")
          (uc "Result")
        )
        (tuple_literal
          (binop_dot
            (uc "Color")
            (uc "RGB")
          )
          (binop_dot
            (uc "ExtMod")
            (uc "Error")
          )
        )
      )
      (binop_dot
        (binop_dot
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
  (apply_uc
    (uc "Ok")
    (lc "rgb")
  )
  (malformed)
  (apply_anon
    (binop_dot
      (uc "TypeC")
      (dot_lc "fromColor")
    )
    (lc "rgb")
  )
  (apply_anon
    (binop_dot
      (uc "Result")
      (uc "Err")
    )
    (lc "err")
  )
  (malformed)
  (binop_dot
    (uc "TypeC")
    (dot_lc "default")
  )
  (malformed)
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

import Basics.Result
import Color
import ModuleA.ModuleB exposing [TypeC]
import ExternalModule as ExtMod
# Simple qualified type
simpleQualified : Color.RGB
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })
# Aliased qualified type
aliasedQualified : ExtMod.DataType
aliasedQualified = (ExtMod.DataType.Default)
# Multi-level qualified type
multiLevelQualified : ModuleA.ModuleB.TypeC
multiLevelQualified = (TypeC..new)
# Using qualified type with generics
resultType : Result.Result((I32, Str))
resultType = Result.Ok(42)
# Function returning qualified type
getColor : {} -> Color.RGB
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })
# Function accepting qualified type
processColor : Color.RGB -> Str
processColor = |color| "Color processed"
# Multiple qualified types in a function signature
transform : Result.Result((Color.RGB, ExtMod.Error)) -> ModuleA.ModuleB.TypeC
transform = |result| match result

Ok(rgb)
=> 
TypeC..fromColor(rgb)
Result.Err(err)
=> 
TypeC..default
}
~~~
# EXPECTED
PARSE ERROR - qualified_type_canonicalization.md:8:1:8:7
PARSE ERROR - qualified_type_canonicalization.md:8:14:8:21
PARSE ERROR - qualified_type_canonicalization.md:10:15:10:23
PARSE ERROR - qualified_type_canonicalization.md:10:24:10:32
PARSE ERROR - qualified_type_canonicalization.md:10:33:10:34
PARSE ERROR - qualified_type_canonicalization.md:10:39:10:40
MODULE NOT FOUND - qualified_type_canonicalization.md:9:1:9:13
MODULE NOT FOUND - qualified_type_canonicalization.md:10:1:10:15
MODULE NOT FOUND - qualified_type_canonicalization.md:11:1:11:32
UNDECLARED TYPE - qualified_type_canonicalization.md:15:19:15:24
MODULE NOT IMPORTED - qualified_type_canonicalization.md:22:23:22:44
UNDEFINED VARIABLE - qualified_type_canonicalization.md:23:23:23:32
MODULE NOT IMPORTED - qualified_type_canonicalization.md:26:14:26:27
UNDECLARED TYPE - qualified_type_canonicalization.md:31:16:31:21
UNUSED VARIABLE - qualified_type_canonicalization.md:35:17:35:22
MODULE NOT IMPORTED - qualified_type_canonicalization.md:39:13:39:26
MODULE NOT IMPORTED - qualified_type_canonicalization.md:39:55:39:76
UNDEFINED VARIABLE - qualified_type_canonicalization.md:42:27:42:42
UNDEFINED VARIABLE - qualified_type_canonicalization.md:43:28:43:41
UNUSED VARIABLE - qualified_type_canonicalization.md:43:20:43:23
# PROBLEMS
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


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:14:1:14:16:**
```roc
simpleQualified : Color.RGB
```
^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:15:1:15:16:**
```roc
simpleQualified = Color.RGB({ r: 255, g: 0, b: 0 })
```
^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:18:1:18:17:**
```roc
aliasedQualified : ExtMod.DataType
```
^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:19:1:19:17:**
```roc
aliasedQualified = ExtMod.DataType.Default
```
^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:22:1:22:20:**
```roc
multiLevelQualified : ModuleA.ModuleB.TypeC
```
^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:23:1:23:20:**
```roc
multiLevelQualified = TypeC.new
```
^^^^^^^^^^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**qualified_type_canonicalization.md:26:14:26:37:**
```roc
resultType : Result.Result(I32, Str)
```
             ^^^^^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:26:1:26:11:**
```roc
resultType : Result.Result(I32, Str)
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:27:1:27:11:**
```roc
resultType = Result.Ok(42)
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:30:1:30:9:**
```roc
getColor : {} -> Color.RGB
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:31:1:31:9:**
```roc
getColor = |_| Color.RGB({ r: 0, g: 255, b: 0 })
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:34:1:34:13:**
```roc
processColor : Color.RGB -> Str
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:35:1:35:13:**
```roc
processColor = |color|
```
^^^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**qualified_type_canonicalization.md:39:13:39:51:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:39:1:39:10:**
```roc
transform : Result.Result(Color.RGB, ExtMod.Error) -> ModuleA.ModuleB.TypeC
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**qualified_type_canonicalization.md:40:1:40:10:**
```roc
transform = |result|
```
^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **rgb** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:42:19:42:22:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
                  ^^^


**UNDEFINED VARIABLE**
Nothing is named **rgb** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:42:43:42:46:**
```roc
        Result.Ok(rgb) => TypeC.fromColor(rgb)
```
                                          ^^^


**UNDEFINED VARIABLE**
Nothing is named **err** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_type_canonicalization.md:43:20:43:23:**
```roc
        Result.Err(err) => TypeC.default
```
                   ^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "simpleQualified"))
    (type type_29)
  )
  (Stmt.assign
    (pattern (Patt.ident "simpleQualified"))
    (Expr.fn_call)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "aliasedQualified"))
    (type type_50)
  )
  (Stmt.assign
    (pattern (Patt.ident "aliasedQualified"))
    (Expr.record_access)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "multiLevelQualified"))
    (type type_64)
  )
  (Stmt.assign
    (pattern (Patt.ident "multiLevelQualified"))
    (Expr.record_access)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "resultType"))
    (type type_78)
  )
  (Stmt.assign
    (pattern (Patt.ident "resultType"))
    (Expr.fn_call)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "getColor"))
    (type type_92)
  )
  (Stmt.assign
    (pattern (Patt.ident "getColor"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processColor"))
    (type type_117)
  )
  (Stmt.assign
    (pattern (Patt.ident "processColor"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "transform"))
    (type type_141)
  )
  (Stmt.assign
    (pattern (Patt.ident "transform"))
    (Expr.lambda (canonicalized))
  )
  (Expr.tag_applied)
  (Expr.malformed)
  (Expr.fn_call)
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.record_access)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 195
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
(var #31 -> #45)
(var #32 _)
(var #33 _)
(var #34 -> #171)
(var #35 _)
(var #36 Num *)
(var #37 _)
(var #38 _)
(var #39 Num *)
(var #40 _)
(var #41 _)
(var #42 Num *)
(var #43 _)
(var #44 -> #173)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 -> #57)
(var #53 _)
(var #54 _)
(var #55 -> #175)
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
(var #66 -> #69)
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
(var #80 -> #85)
(var #81 _)
(var #82 _)
(var #83 -> #176)
(var #84 Num *)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 _)
(var #90 _)
(var #91 _)
(var #92 _)
(var #93 _)
(var #94 -> #183)
(var #95 _)
(var #96 _)
(var #97 _)
(var #98 -> #179)
(var #99 _)
(var #100 Num *)
(var #101 _)
(var #102 _)
(var #103 Num *)
(var #104 _)
(var #105 _)
(var #106 Num *)
(var #107 _)
(var #108 -> #181)
(var #109 _)
(var #110 -> #183)
(var #111 _)
(var #112 _)
(var #113 _)
(var #114 _)
(var #115 _)
(var #116 _)
(var #117 _)
(var #118 _)
(var #119 -> #185)
(var #120 _)
(var #121 Str)
(var #122 -> #185)
(var #123 _)
(var #124 _)
(var #125 _)
(var #126 _)
(var #127 _)
(var #128 _)
(var #129 _)
(var #130 _)
(var #131 _)
(var #132 _)
(var #133 _)
(var #134 _)
(var #135 _)
(var #136 _)
(var #137 _)
(var #138 _)
(var #139 _)
(var #140 _)
(var #141 _)
(var #142 _)
(var #143 -> #187)
(var #144 _)
(var #145 _)
(var #146 _)
(var #147 _)
(var #148 _)
(var #149 -> #187)
(var #150 _)
(var #151 -> #188)
(var #152 _)
(var #153 _)
(var #154 _)
(var #155 _)
(var #156 _)
(var #157 -> #190)
(var #158 _)
(var #159 _)
(var #160 _)
(var #161 _)
(var #162 -> #191)
(var #163 _)
(var #164 _)
(var #165 _)
(var #166 _)
(var #167 _)
(var #168 _)
(var #169 _)
(var #170 _)
(var #171 -> #174)
(var #172 {})
(var #173 record)
(var #174 fn_pure)
(var #175 _)
(var #176 -> #177)
(var #177 fn_pure)
(var #178 _)
(var #179 -> #182)
(var #180 {})
(var #181 record)
(var #182 fn_pure)
(var #183 fn_pure)
(var #184 _)
(var #185 fn_pure)
(var #186 _)
(var #187 fn_pure)
(var #188 fn_pure)
(var #189 _)
(var #190 fn_pure)
(var #191 -> #192)
(var #192 fn_pure)
(var #193 _)
(var #194 _)
~~~
# TYPES
~~~roc
aliasedQualified : _a
multiLevelQualified : _a
transform : _arg -> _ret
resultType : _a
processColor : _arg -> Str
result : _a
color : _a
getColor : _arg -> _ret
simpleQualified : _a
~~~
