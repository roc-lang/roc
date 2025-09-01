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
	rounded = a.to_frac() / 255.0
	Color.RGBA((r, g, b, rounded))
}

hex : Str -> Result(Color, [InvalidHex(Str)])
hex = |str| {

	bytes = str.to_utf8()
	is_char_in_hex_range = |b| (b >= '0' && b <= '9' || b >= 'a' && b <= 'f') || b >= 'A' && b <= 'F'

	match bytes
		['#', a, b, c, d, e, f] => 
			is_valid = ((((a.is_char_in_hex_range() && b.is_char_in_hex_range()) && c.is_char_in_hex_range()) && d.is_char_in_hex_range()) && e.is_char_in_hex_range()) && f.is_char_in_hex_range()
			
if is_valid Ok(Color.Hex(str))
			else 
			Err(InvalidHex("Expected Hex to be in the range 0-9, a-f, A-F, got ${str}"))
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

expect rgb((124, 56, 245)) | .to_str() == "rgb(124, 56, 245)"
expect rgba((124, 56, 245, 255)) | .to_str() == "rgba(124, 56, 245, 1.0)"
expect hex("#ff00ff") | .map_ok(to_str) == Ok("#ff00ff")

named : Str -> Result(Color, [UnknownColor(Str)])
named = |str| if str.is_named_color() Ok(Color.Named(str))
else
Err(UnknownColor("Unknown color ${str}"))

is_named_color = |str| {
	colors = Set.from_list(["AliceBlue", "AntiqueWhite", "Aqua"])

	colors.contains(str)
}
~~~
# EXPECTED
NIL
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
The token **else ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**Color.md:42:44:42:49:**
```roc
            if is_valid Ok(Color.Hex(str)) else Err(InvalidHex("Expected Hex to be in the range 0-9, a-f, A-F, got ${str}"))
```
                                           ^^^^^


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


**UNEXPECTED TOKEN IN EXPRESSION**
The token **else
        ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**Color.md:64:5:65:9:**
```roc
    else
        Err(UnknownColor("Unknown color ${str}"))
```


**UNDEFINED VARIABLE**
Nothing is named **Color.RGB** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:18:17:18:26:**
```roc
rgb = |r, g, b| Color.RGB(r, g, b)
```
                ^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**Color.md:22:15:22:24:**
```roc
    rounded = a.to_frac() / 255.0
```
              ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Color.RGBA** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:23:5:23:15:**
```roc
    Color.RGBA(r, g, b, rounded)
```
    ^^^^^^^^^^


**UNUSED VARIABLE**
Variable **a** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_a` to suppress this warning.
The unused variable is declared here:

**Color.md:21:18:21:19:**
```roc
rgba = |r, g, b, a| {
```
                 ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**Color.md:29:13:29:24:**
```roc
    bytes = str.to_utf8()
```
            ^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**Color.md:33:9:43:10:**
```roc
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
```


**UNUSED VARIABLE**
Variable **is_char_in_hex_range** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_is_char_in_hex_range` to suppress this warning.
The unused variable is declared here:

**Color.md:30:5:30:25:**
```roc
    is_char_in_hex_range = |b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')
```
    ^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **str** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_str` to suppress this warning.
The unused variable is declared here:

**Color.md:27:8:27:11:**
```roc
hex = |str| {
```
       ^^^


**UNDEFINED VARIABLE**
Nothing is named **r** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:50:15:50:16:**
```roc
    Color.RGB(r, g, b) => "rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"
```
              ^


**UNDEFINED VARIABLE**
Nothing is named **g** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:50:18:50:19:**
```roc
    Color.RGB(r, g, b) => "rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"
```
                 ^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:50:21:50:22:**
```roc
    Color.RGB(r, g, b) => "rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"
```
                    ^


**UNDEFINED VARIABLE**
Nothing is named **Color.RGBA** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:51:5:51:15:**
```roc
    Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
```
    ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **r** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:51:16:51:17:**
```roc
    Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
```
               ^


**UNDEFINED VARIABLE**
Nothing is named **g** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:51:19:51:20:**
```roc
    Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
```
                  ^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:51:22:51:23:**
```roc
    Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
```
                     ^


**UNDEFINED VARIABLE**
Nothing is named **a** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:51:25:51:26:**
```roc
    Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
```
                        ^


**UNDEFINED VARIABLE**
Nothing is named **Color.Named** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:52:5:52:16:**
```roc
    Color.Named(inner) => inner
```
    ^^^^^^^^^^^


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
Nothing is named **Color.Hex** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:53:5:53:14:**
```roc
    Color.Hex(inner) => inner
```
    ^^^^^^^^^


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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**Color.md:56:8:56:32:**
```roc
expect rgb(124, 56, 245).to_str() == "rgb(124, 56, 245)"
```
       ^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**Color.md:57:8:57:38:**
```roc
expect rgba(124, 56, 245, 255).to_str() == "rgba(124, 56, 245, 1.0)"
```
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**Color.md:58:8:58:29:**
```roc
expect hex("#ff00ff").map_ok(to_str) == Ok("#ff00ff")
```
       ^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**Color.md:62:8:62:26:**
```roc
    if str.is_named_color()
```
       ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Color.Named** in this scope.
Is there an **import** or **exposing** missing up-top?

**Color.md:63:12:63:23:**
```roc
        Ok(Color.Named(str))
```
           ^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**Color.md:70:5:70:20:**
```roc
    colors.contains(str)
```
    ^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **colors** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_colors` to suppress this warning.
The unused variable is declared here:

**Color.md:68:5:68:11:**
```roc
    colors = Set.from_list(["AliceBlue", "AntiqueWhite", "Aqua"])
```
    ^^^^^^


**UNUSED VARIABLE**
Variable **hex** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_hex` to suppress this warning.
The unused variable is declared here:

**Color.md:27:1:27:4:**
```roc
hex = |str| {
```
^^^


**UNUSED VARIABLE**
Variable **named** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_named` to suppress this warning.
The unused variable is declared here:

**Color.md:61:1:61:6:**
```roc
named = |str|
```
^^^^^


**UNUSED VARIABLE**
Variable **is_named_color** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_is_named_color` to suppress this warning.
The unused variable is declared here:

**Color.md:67:1:67:15:**
```roc
is_named_color = |str|{
```
^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **rgba** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rgba` to suppress this warning.
The unused variable is declared here:

**Color.md:21:1:21:5:**
```roc
rgba = |r, g, b, a| {
```
^^^^


**UNUSED VARIABLE**
Variable **rgb** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rgb` to suppress this warning.
The unused variable is declared here:

**Color.md:18:1:18:4:**
```roc
rgb = |r, g, b| Color.RGB(r, g, b)
```
^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Stmt.type_anno
    (name "rgb")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "rgb"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "rgba")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "rgba"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "hex")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "hex"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "to_str")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "to_str"))
    (Expr.lambda (canonicalized))
  )
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.lookup "inner")
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.lookup "inner")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.type_anno
    (name "named")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "named"))
    (Expr.lambda (canonicalized))
  )
  (Expr.malformed)
  (Expr.apply_tag)
  (Stmt.assign
    (pattern (Patt.ident "is_named_color"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_h")
~~~
# TYPES
~~~roc
# Type checking for this node type not yet implemented
~~~
