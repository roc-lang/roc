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
KwModule OpenSquare UpperIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma CloseSquare UpperIdent OpColonEqual OpenSquare UpperIdent OpenRound UpperIdent Comma UpperIdent Comma UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent CloseRound Comma CloseSquare LowerIdent OpColon UpperIdent Comma UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent Dot LowerIdent OpenRound CloseRound OpSlash Float UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma OpenSquare UpperIdent OpenRound UpperIdent CloseRound CloseSquare CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent Dot LowerIdent OpenRound CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenRound LowerIdent OpGreaterThanOrEq SingleQuote OpAnd LowerIdent OpLessThanOrEq SingleQuote CloseRound OpOr OpenRound LowerIdent OpGreaterThanOrEq SingleQuote OpAnd LowerIdent OpLessThanOrEq SingleQuote CloseRound OpOr OpenRound LowerIdent OpGreaterThanOrEq SingleQuote OpAnd LowerIdent OpLessThanOrEq SingleQuote CloseRound KwMatch LowerIdent OpenCurly OpenSquare SingleQuote Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare OpFatArrow OpenCurly LowerIdent OpAssign LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound OpAnd LowerIdent Dot LowerIdent OpenRound CloseRound KwIf LowerIdent UpperIdent OpenRound UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound CloseRound KwElse UpperIdent OpenRound UpperIdent OpenRound String CloseRound CloseRound CloseCurly Underscore OpFatArrow UpperIdent OpenRound UpperIdent OpenRound String CloseRound CloseRound CloseCurly CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound OpFatArrow String UpperIdent Dot UpperIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound OpFatArrow String UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent CloseCurly KwExpect LowerIdent OpenRound Int Comma Int Comma Int CloseRound Dot LowerIdent OpenRound CloseRound OpEquals String KwExpect LowerIdent OpenRound Int Comma Int Comma Int Comma Int CloseRound Dot LowerIdent OpenRound CloseRound OpEquals String KwExpect LowerIdent OpenRound String CloseRound Dot LowerIdent OpenRound LowerIdent CloseRound OpEquals UpperIdent OpenRound String CloseRound LowerIdent OpColon UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma OpenSquare UpperIdent OpenRound UpperIdent CloseRound CloseSquare CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Dot LowerIdent OpenRound CloseRound UpperIdent OpenRound UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound CloseRound KwElse UpperIdent OpenRound UpperIdent OpenRound String CloseRound CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound OpenSquare String Comma String Comma String CloseSquare CloseRound LowerIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "Color")
    (list_literal
      (tuple_literal
        (apply_uc
          (uc "RGB")
          (tuple_literal
            (uc "U8")
            (uc "U8")
            (uc "U8")
          )
        )
        (apply_uc
          (uc "RGBA")
          (tuple_literal
            (uc "U8")
            (uc "U8")
            (uc "U8")
            (uc "Dec")
          )
        )
        (apply_uc
          (uc "Named")
          (uc "Str")
        )
        (apply_uc
          (uc "Hex")
          (uc "Str")
        )
        (malformed malformed:expr_unexpected_token)
      )
    )
  )
  (binop_colon
    (lc "rgb")
    (binop_thin_arrow
      (uc "U8")
      (binop_thin_arrow
        (uc "U8")
        (binop_thin_arrow
          (uc "U8")
          (uc "Color")
        )
      )
    )
  )
  (binop_equals
    (lc "rgb")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "Color")
            (uc "RGB")
          )
          (tuple_literal
            (lc "r")
            (lc "g")
            (lc "b")
          )
        )
      )
      (args
        (tuple_literal
          (lc "r")
          (lc "g")
          (lc "b")
        )
      )
    )
  )
  (binop_colon
    (lc "rgba")
    (binop_thin_arrow
      (uc "U8")
      (binop_thin_arrow
        (uc "U8")
        (binop_thin_arrow
          (uc "U8")
          (binop_thin_arrow
            (uc "U8")
            (uc "Color")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "rgba")
    (lambda
      (body
        (block
          (binop_equals
            (lc "rounded")
            (binop_slash
              (apply_anon
                (binop_pipe
                  (lc "a")
                  (dot_lc "to_frac")
                )
              )
              (frac_literal_small 255)
            )
          )
          (apply_anon
            (binop_pipe
              (uc "Color")
              (uc "RGBA")
            )
            (tuple_literal
              (lc "r")
              (lc "g")
              (lc "b")
              (lc "rounded")
            )
          )
        )
      )
      (args
        (tuple_literal
          (lc "r")
          (lc "g")
          (lc "b")
          (lc "a")
        )
      )
    )
  )
  (binop_colon
    (lc "hex")
    (binop_thin_arrow
      (uc "Str")
      (apply_uc
        (uc "Result")
        (tuple_literal
          (uc "Color")
          (list_literal
            (apply_uc
              (uc "InvalidHex")
              (uc "Str")
            )
          )
        )
      )
    )
  )
  (binop_equals
    (lc "hex")
    (lambda
      (body
        (block
          (binop_equals
            (lc "bytes")
            (apply_anon
              (binop_pipe
                (lc "str")
                (dot_lc "to_utf8")
              )
            )
          )
          (binop_equals
            (lc "is_char_in_hex_range")
            (lambda
              (body
                (binop_or
                  (binop_or
                    (binop_and
                      (binop_gte
                        (lc "b")
                        (str_literal_small "")
                      )
                      (binop_lte
                        (lc "b")
                        (str_literal_small "")
                      )
                    )
                    (binop_and
                      (binop_gte
                        (lc "b")
                        (str_literal_small "")
                      )
                      (binop_lte
                        (lc "b")
                        (str_literal_small "")
                      )
                    )
                  )
                  (binop_and
                    (binop_gte
                      (lc "b")
                      (str_literal_small "")
                    )
                    (binop_lte
                      (lc "b")
                      (str_literal_small "")
                    )
                  )
                )
              )
              (args
                (lc "b")
              )
            )
          )
          (match <180 branches>)
          (binop_colon
            (lc "to_str")
            (binop_thin_arrow
              (uc "Color")
              (uc "Str")
            )
          )
          (binop_equals
            (lc "to_str")
            (lambda
              (body
                (match <226 branches>)
              )
              (args
                (lc "color")
              )
            )
          )
          (binop_double_equals
            (apply_anon
              (binop_pipe
                (apply_lc
                  (lc "rgb")
                  (tuple_literal
                    (num_literal_i32 124)
                    (num_literal_i32 56)
                    (num_literal_i32 245)
                  )
                )
                (dot_lc "to_str")
              )
            )
            (str_literal_big "rgb(124, 56, 245)")
          )
          (expect
            (binop_double_equals
              (apply_anon
                (binop_pipe
                  (apply_lc
                    (lc "rgba")
                    (tuple_literal
                      (num_literal_i32 124)
                      (num_literal_i32 56)
                      (num_literal_i32 245)
                      (num_literal_i32 255)
                    )
                  )
                  (dot_lc "to_str")
                )
              )
              (str_literal_big "rgba(124, 56, 245, 1.0)")
            )
          )
          (expect
            (binop_double_equals
              (apply_anon
                (binop_pipe
                  (apply_lc
                    (lc "hex")
                    (str_literal_big "#ff00ff")
                  )
                  (dot_lc "map_ok")
                )
                (lc "to_str")
              )
              (apply_uc
                (uc "Ok")
                (str_literal_big "#ff00ff")
              )
            )
          )
          (binop_colon
            (lc "named")
            (binop_thin_arrow
              (uc "Str")
              (apply_uc
                (uc "Result")
                (tuple_literal
                  (uc "Color")
                  (list_literal
                    (apply_uc
                      (uc "UnknownColor")
                      (uc "Str")
                    )
                  )
                )
              )
            )
          )
          (binop_equals
            (lc "named")
            (lambda
              (body
                (if_else <284 branches>)
              )
              (args
                (lc "str")
              )
            )
          )
          (binop_equals
            (lc "is_named_color")
            (lambda
              (body
                (block
                  (binop_equals
                    (lc "colors")
                    (apply_anon
                      (binop_pipe
                        (uc "Set")
                        (dot_lc "from_list")
                      )
                      (list_literal
                        (tuple_literal
                          (str_literal_big "AliceBlue")
                          (str_literal_big "AntiqueWhite")
                          (str_literal_small "Aqua")
                        )
                      )
                    )
                  )
                  (apply_anon
                    (binop_pipe
                      (lc "colors")
                      (dot_lc "contains")
                    )
                    (lc "str")
                  )
                )
              )
              (args
                (lc "str")
              )
            )
          )
        )
      )
      (args
        (lc "str")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNUSED VARIABLE - Color.md:30:5:30:25
UNDEFINED VARIABLE - Color.md:68:14:68:27
INVALID NOMINAL TAG - Color.md:23:5:23:33
# PROBLEMS
**Parse Error**
at 15:1 to 15:1

**Parse Error**
at 10:10 to 17:1

**Parse Error**
at 32:5 to 32:17

**Parse Error**
at 33:33 to 33:33

**Parse Error**
at 42:13 to 42:25

**Parse Error**
at 44:11 to 44:11

**Parse Error**
at 32:5 to 46:1

**Parse Error**
at 46:1 to 46:1

**Parse Error**
at 49:18 to 49:30

**Parse Error**
at 50:24 to 50:24

**Parse Error**
at 51:28 to 51:28

**Parse Error**
at 52:24 to 52:24

**Parse Error**
at 53:22 to 53:22

**Parse Error**
at 49:18 to 56:1

**Parse Error**
at 56:1 to 56:1

**Parse Error**
at 62:5 to 63:9

**Parse Error**
at 27:13 to 72:1

**Unsupported Node**
at 10:1 to 16:1

**Unsupported Node**
at 17:7 to 17:26

**Unsupported Node**
at 18:7 to 18:17

**Unsupported Node**
at 20:8 to 20:31

**Unsupported Node**
at 21:8 to 21:21

**Unsupported Node**
at 26:7 to 27:1

**Unsupported Node**
at 27:7 to 27:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "rgb")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "rgba")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "hex")
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
# Type checking for this node type not yet implemented
~~~
