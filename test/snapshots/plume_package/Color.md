# META
~~~ini
description=Color module from package
type=package
~~~
# SOURCE
~~~roc
module [
    Color,
    to_str,
    rgb,
    rgba,
    hex,
    named,
]

Color := [
    RGB(U8, U8, U8),
    RGBA(U8, U8, U8, Dec),
    Named(Str),
    Hex(Str),
]

rgb : U8, U8, U8 -> Color
rgb = |r, g, b| Color.RGB(r, g, b)

rgba : U8, U8, U8, U8 -> Color
rgba = |r, g, b, a| {
    rounded = a.to_frac() / 255.0
    Color.RGBA(r, g, b, rounded)
}

hex : Str -> Result(Color, [InvalidHex(Str)])
hex = |str| {

    bytes = str.to_utf8()
    is_char_in_hex_range = |b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')

    match bytes {
        ['#', a, b, c, d, e, f] => {
            is_valid =
                a.is_char_in_hex_range()
                and b.is_char_in_hex_range()
                and c.is_char_in_hex_range()
                and d.is_char_in_hex_range()
                and e.is_char_in_hex_range()
                and f.is_char_in_hex_range()

            if is_valid Ok(Color.Hex(str)) else Err(InvalidHex("Expected Hex to be in the range 0-9, a-f, A-F, got ${str}"))
        }
        _ => Err(InvalidHex("Expected Hex must start with # and be 7 characters long, got ${str}"))
    }
}

to_str : Color -> Str
to_str = |color| match color {
    Color.RGB(r, g, b) => "rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"
    Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
    Color.Named(inner) => inner
    Color.Hex(inner) => inner
}

expect rgb(124, 56, 245).to_str() == "rgb(124, 56, 245)"
expect rgba(124, 56, 245, 255).to_str() == "rgba(124, 56, 245, 1.0)"
expect hex("#ff00ff").map_ok(to_str) == Ok("#ff00ff")

named : Str -> Result(Color, [UnknownColor(Str)])
named = |str|
    if str.is_named_color()
        Ok(Color.Named(str))
    else
        Err(UnknownColor("Unknown color ${str}"))

is_named_color = |str|{
    colors = Set.from_list(["AliceBlue", "AntiqueWhite", "Aqua"])

    colors.contains(str)
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma CloseSquare BlankLine UpperIdent OpColonEqual OpenSquare UpperIdent OpenRound UpperIdent Comma UpperIdent Comma UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent CloseRound Comma CloseSquare BlankLine LowerIdent OpColon UpperIdent Comma UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent Dot LowerIdent OpenRound CloseRound OpSlash Float UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseCurly BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma OpenSquare UpperIdent OpenRound UpperIdent CloseRound CloseSquare CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly BlankLine LowerIdent OpAssign LowerIdent Dot LowerIdent OpenRound CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenRound LowerIdent OpGreaterThanOrEq SingleQuote OpAnd LowerIdent OpLessThanOrEq SingleQuote CloseRound OpOr OpenRound LowerIdent OpGreaterThanOrEq SingleQuote OpAnd LowerIdent OpLessThanOrEq SingleQuote CloseRound OpOr OpenRound LowerIdent OpGreaterThanOrEq SingleQuote OpAnd LowerIdent OpLessThanOrEq SingleQuote CloseRound BlankLine KwMatch LowerIdent OpenCurly OpenSquare SingleQuote Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare OpFatArrow OpenCurly LowerIdent OpAssign LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound BlankLine KwIf LowerIdent UpperIdent OpenRound UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound CloseRound KwElse UpperIdent OpenRound UpperIdent OpenRound String CloseRound CloseRound CloseCurly Underscore OpFatArrow UpperIdent OpenRound UpperIdent OpenRound String CloseRound CloseRound CloseCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound OpFatArrow String UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound OpFatArrow String UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent CloseCurly BlankLine KwExpect LowerIdent OpenRound Int Comma Int Comma Int CloseRound Dot LowerIdent OpenRound CloseRound OpEquals String KwExpect LowerIdent OpenRound Int Comma Int Comma Int Comma Int CloseRound Dot LowerIdent OpenRound CloseRound OpEquals String KwExpect LowerIdent OpenRound String CloseRound Dot LowerIdent OpenRound LowerIdent CloseRound OpEquals UpperIdent OpenRound String CloseRound BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma OpenSquare UpperIdent OpenRound UpperIdent CloseRound CloseSquare CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Dot LowerIdent OpenRound CloseRound UpperIdent OpenRound UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound CloseRound KwElse UpperIdent OpenRound UpperIdent OpenRound String CloseRound CloseRound BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound OpenSquare String Comma String Comma String CloseSquare CloseRound BlankLine LowerIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Color")

    (lc "to_str")

    (lc "rgb")

    (lc "rgba")

    (lc "hex")

    (lc "named")
))
~~~
# FORMATTED
~~~roc
module [
	Color,
	to_str,
	rgb,
	rgba,
	hex,
	named,
]

Color := [RGB((U8, U8, U8)), RGBA((U8, U8, U8, Dec)), Named(Str), Hex(Str)]
rgb : U8 -> U8 -> U8 -> Color
rgb = |r, g, b| Color.RGB((r, g, b))
rgba : U8 -> U8 -> U8 -> U8 -> Color
rgba = |r, g, b, a| {
	rounded = a..to_frac() / 255.0
	Color.RGBA((r, g, b, rounded))
}

hex : Str -> Result(Color, [InvalidHex(Str)])
hex = |str| {
	bytes = str..to_utf8()
	is_char_in_hex_range = |b| (b >= '0' && b <= '9' || b >= 'a' && b <= 'f') || b >= 'A' && b <= 'F'
	match bytes
		['#', a, b, c, d, e, f] => 
			is_valid = ((((a..is_char_in_hex_range() && b..is_char_in_hex_range()) && c..is_char_in_hex_range()) && d..is_char_in_hex_range()) && e..is_char_in_hex_range()) && f..is_char_in_hex_range()
			if is_valid Ok(Color.Hex(str)) else Err(InvalidHex("Expected Hex to be in the range 0-9, a-f, A-F, got ${str}"))
		_ => Err
}

to_str : Color -> Str
to_str = |color| match color

RGB((r, g, b))
=> 
"rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"
Color.RGBA((r, g, b, a))
=> 
"rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
Color.Named(inner)
=> 
inner
Color.Hex(inner)
=> 
inner
}

expect rgb((124, 56, 245))..to_str() == "rgb(124, 56, 245)"
expect rgba((124, 56, 245, 255))..to_str() == "rgba(124, 56, 245, 1.0)"
expect hex("#ff00ff")..map_ok(to_str) == Ok("#ff00ff")
named : Str -> Result(Color, [UnknownColor(Str)])
named = |str| if str..is_named_color() Ok(Color.Named(str)) else Err(UnknownColor("Unknown color ${str}"))
is_named_color = |str| {
	colors = Set..from_list(["AliceBlue", "AntiqueWhite", "Aqua"])
	colors..contains(str)
}
~~~
# EXPECTED
UNUSED VARIABLE - Color.md:30:5:30:25
UNDEFINED VARIABLE - Color.md:68:14:68:27
INVALID NOMINAL TAG - Color.md:23:5:23:33
# PROBLEMS
**UNEXPECTED TOKEN IN PATTERN**
The token **'#'** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**Color.md:33:10:33:13:**
```roc
        ['#', a, b, c, d, e, f] => {
```
         ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Err** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**Color.md:44:14:44:17:**
```roc
        _ => Err(InvalidHex("Expected Hex must start with # and be 7 characters long, got ${str}"))
```
             ^^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**Color.md:45:5:46:1:**
```roc
    }
}
```


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**Color.md:50:10:50:11:**
```roc
    Color.RGB(r, g, b) => "rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"
```
         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**Color.md:50:24:50:27:**
```roc
    Color.RGB(r, g, b) => "rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"
```
                       ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**Color.md:51:28:51:31:**
```roc
    Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
```
                           ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**Color.md:52:24:52:27:**
```roc
    Color.Named(inner) => inner
```
                       ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**Color.md:53:22:53:25:**
```roc
    Color.Hex(inner) => inner
```
                     ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**Color.md:54:1:56:1:**
```roc
}

expect rgb(124, 56, 245).to_str() == "rgb(124, 56, 245)"
```


**STATEMENT IN EXPRESSION CONTEXT**
Found a statement where an expression was expected.
Statements like **return**, **dbg**, or **expect** cannot be used in expression contexts.

**Color.md:10:7:10:9:**
```roc
Color := [
```
      ^^


**SHADOWING**
This definition shadows an existing one.

**Color.md:18:1:18:4:**
```roc
rgb = |r, g, b| Color.RGB(r, g, b)
```
^^^


**SHADOWING**
This definition shadows an existing one.

**Color.md:21:9:21:10:**
```roc
rgba = |r, g, b, a| {
```
        ^


**SHADOWING**
This definition shadows an existing one.

**Color.md:21:12:21:13:**
```roc
rgba = |r, g, b, a| {
```
           ^


**SHADOWING**
This definition shadows an existing one.

**Color.md:21:15:21:16:**
```roc
rgba = |r, g, b, a| {
```
              ^


**SHADOWING**
This definition shadows an existing one.

**Color.md:21:1:21:5:**
```roc
rgba = |r, g, b, a| {
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**Color.md:30:29:30:30:**
```roc
    is_char_in_hex_range = |b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')
```
                            ^


**SHADOWING**
This definition shadows an existing one.

**Color.md:33:15:33:16:**
```roc
        ['#', a, b, c, d, e, f] => {
```
              ^


**SHADOWING**
This definition shadows an existing one.

**Color.md:33:18:33:19:**
```roc
        ['#', a, b, c, d, e, f] => {
```
                 ^


**SHADOWING**
This definition shadows an existing one.

**Color.md:27:1:27:4:**
```roc
hex = |str| {
```
^^^


**SHADOWING**
This definition shadows an existing one.

**Color.md:49:1:49:7:**
```roc
to_str = |color| match color {
```
^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **inner** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:52:17:52:22:**
```roc
    Color.Named(inner) => inner
```
                ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **inner** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:52:27:52:32:**
```roc
    Color.Named(inner) => inner
```
                          ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **inner** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:53:15:53:20:**
```roc
    Color.Hex(inner) => inner
```
              ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **inner** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:53:25:53:30:**
```roc
    Color.Hex(inner) => inner
```
                        ^^^^^


**SHADOWING**
This definition shadows an existing one.

**Color.md:61:10:61:13:**
```roc
named = |str|
```
         ^^^


**SHADOWING**
This definition shadows an existing one.

**Color.md:61:1:61:6:**
```roc
named = |str|
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**Color.md:67:19:67:22:**
```roc
is_named_color = |str|{
```
                  ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "rgb"))
    (type type_36)
  )
  (Stmt.assign
    (pattern (Patt.ident "rgb"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "rgba"))
    (type type_61)
  )
  (Stmt.assign
    (pattern (Patt.ident "rgba"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "hex"))
    (type type_98)
  )
  (Stmt.assign
    (pattern (Patt.ident "hex"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "to_str"))
    (type type_205)
  )
  (Stmt.assign
    (pattern (Patt.ident "to_str"))
    (Expr.lambda (canonicalized))
  )
  (Expr.tag_applied)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.lookup "inner")
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.lookup "inner")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "named"))
    (type type_296)
  )
  (Stmt.assign
    (pattern (Patt.ident "named"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "is_named_color"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 400
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
(var #38 -> #349)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 -> #344)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 -> #345)
(var #49 _)
(var #50 -> #349)
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
(var #63 -> #361)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 -> #74)
(var #69 _)
(var #70 _)
(var #71 -> #354)
(var #72 -> #73)
(var #73 -> #74)
(var #74 F64)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 -> #355)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 -> #356)
(var #84 _)
(var #85 _)
(var #86 -> #361)
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
(var #100 -> #366)
(var #101 _)
(var #102 -> #106)
(var #103 _)
(var #104 _)
(var #105 -> #363)
(var #106 _)
(var #107 _)
(var #108 -> #365)
(var #109 _)
(var #110 -> #111)
(var #111 -> #112)
(var #112 -> #115)
(var #113 -> #114)
(var #114 -> #115)
(var #115 -> #116)
(var #116 -> #123)
(var #117 -> #118)
(var #118 -> #119)
(var #119 -> #122)
(var #120 -> #121)
(var #121 -> #122)
(var #122 -> #123)
(var #123 -> #124)
(var #124 -> #131)
(var #125 -> #126)
(var #126 -> #127)
(var #127 -> #130)
(var #128 -> #129)
(var #129 -> #130)
(var #130 -> #131)
(var #131 -> #132)
(var #132 Str)
(var #133 -> #365)
(var #134 _)
(var #135 _)
(var #136 _)
(var #137 _)
(var #138 _)
(var #139 _)
(var #140 _)
(var #141 _)
(var #142 _)
(var #143 _)
(var #144 _)
(var #145 _)
(var #146 _)
(var #147 _)
(var #148 _)
(var #149 _)
(var #150 _)
(var #151 _)
(var #152 _)
(var #153 _)
(var #154 _)
(var #155 _)
(var #156 _)
(var #157 _)
(var #158 _)
(var #159 _)
(var #160 _)
(var #161 _)
(var #162 _)
(var #163 _)
(var #164 _)
(var #165 _)
(var #166 _)
(var #167 _)
(var #168 _)
(var #169 _)
(var #170 _)
(var #171 _)
(var #172 _)
(var #173 _)
(var #174 _)
(var #175 _)
(var #176 _)
(var #177 _)
(var #178 _)
(var #179 _)
(var #180 _)
(var #181 _)
(var #182 _)
(var #183 _)
(var #184 _)
(var #185 Str)
(var #186 _)
(var #187 _)
(var #188 _)
(var #189 _)
(var #190 _)
(var #191 _)
(var #192 _)
(var #193 _)
(var #194 _)
(var #195 _)
(var #196 _)
(var #197 _)
(var #198 _)
(var #199 _)
(var #200 -> #366)
(var #201 _)
(var #202 _)
(var #203 _)
(var #204 _)
(var #205 _)
(var #206 _)
(var #207 -> #368)
(var #208 _)
(var #209 _)
(var #210 _)
(var #211 _)
(var #212 _)
(var #213 -> #368)
(var #214 _)
(var #215 -> #370)
(var #216 _)
(var #217 _)
(var #218 _)
(var #219 -> #369)
(var #220 _)
(var #221 _)
(var #222 Str)
(var #223 _)
(var #224 _)
(var #225 -> #372)
(var #226 _)
(var #227 _)
(var #228 _)
(var #229 _)
(var #230 -> #373)
(var #231 _)
(var #232 _)
(var #233 Str)
(var #234 _)
(var #235 _)
(var #236 -> #376)
(var #237 _)
(var #238 _)
(var #239 _)
(var #240 _)
(var #241 _)
(var #242 _)
(var #243 -> #379)
(var #244 _)
(var #245 _)
(var #246 _)
(var #247 _)
(var #248 _)
(var #249 _)
(var #250 Num *)
(var #251 Num *)
(var #252 Num *)
(var #253 _)
(var #254 _)
(var #255 _)
(var #256 _)
(var #257 _)
(var #258 Str)
(var #259 _)
(var #260 _)
(var #261 _)
(var #262 Num *)
(var #263 Num *)
(var #264 Num *)
(var #265 Num *)
(var #266 _)
(var #267 _)
(var #268 _)
(var #269 _)
(var #270 _)
(var #271 Str)
(var #272 _)
(var #273 _)
(var #274 _)
(var #275 Str)
(var #276 _)
(var #277 _)
(var #278 _)
(var #279 _)
(var #280 _)
(var #281 _)
(var #282 Str)
(var #283 _)
(var #284 _)
(var #285 _)
(var #286 _)
(var #287 _)
(var #288 _)
(var #289 _)
(var #290 _)
(var #291 _)
(var #292 _)
(var #293 _)
(var #294 _)
(var #295 _)
(var #296 _)
(var #297 _)
(var #298 -> #394)
(var #299 _)
(var #300 _)
(var #301 _)
(var #302 -> #387)
(var #303 -> #388)
(var #304 -> #391)
(var #305 _)
(var #306 _)
(var #307 -> #389)
(var #308 _)
(var #309 _)
(var #310 -> #315)
(var #311 -> #393)
(var #312 -> #392)
(var #313 Str)
(var #314 _)
(var #315 _)
(var #316 -> #315)
(var #317 -> #394)
(var #318 _)
(var #319 -> #399)
(var #320 _)
(var #321 -> #329)
(var #322 _)
(var #323 _)
(var #324 -> #397)
(var #325 Str)
(var #326 -> #325)
(var #327 -> #325)
(var #328 -> #396)
(var #329 _)
(var #330 _)
(var #331 _)
(var #332 _)
(var #333 -> #398)
(var #334 _)
(var #335 _)
(var #336 _)
(var #337 -> #399)
(var #338 _)
(var #339 _)
(var #340 _)
(var #341 _)
(var #342 _)
(var #343 _)
(var #344 -> #346)
(var #345 tuple)
(var #346 fn_pure)
(var #347 fn_pure)
(var #348 fn_pure)
(var #349 fn_pure)
(var #350 _)
(var #351 _)
(var #352 _)
(var #353 _)
(var #354 fn_pure)
(var #355 -> #357)
(var #356 tuple)
(var #357 fn_pure)
(var #358 fn_pure)
(var #359 fn_pure)
(var #360 fn_pure)
(var #361 fn_pure)
(var #362 _)
(var #363 fn_pure)
(var #364 _)
(var #365 fn_pure)
(var #366 fn_pure)
(var #367 _)
(var #368 fn_pure)
(var #369 tuple)
(var #370 fn_pure)
(var #371 _)
(var #372 -> #374)
(var #373 tuple)
(var #374 fn_pure)
(var #375 _)
(var #376 -> #377)
(var #377 fn_pure)
(var #378 _)
(var #379 -> #380)
(var #380 fn_pure)
(var #381 _)
(var #382 _)
(var #383 _)
(var #384 _)
(var #385 _)
(var #386 _)
(var #387 fn_pure)
(var #388 _)
(var #389 -> #390)
(var #390 fn_pure)
(var #391 fn_pure)
(var #392 fn_pure)
(var #393 fn_pure)
(var #394 fn_pure)
(var #395 _)
(var #396 List #325)
(var #397 fn_pure)
(var #398 fn_pure)
(var #399 fn_pure)
~~~
# TYPES
~~~roc
hex : _arg -> _ret
str : _h
named : _arg -> _ret
r : _h
d : _h
f : _h
g : _h
rgb : _arg -> _arg2 -> _arg3 -> _ret
b : _h
color : _h
c : _h
rgba : _arg -> _arg2 -> _arg3 -> _arg4 -> _ret
to_str : _arg -> _ret
a : _h
e : _h
~~~
