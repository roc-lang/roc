# META
~~~ini
description=Color module from package
type=package
~~~
# SOURCE
~~~roc
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

hex : Str -> Try(Color, [InvalidHex(Str)])
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

named : Str -> Try(Color, [UnknownColor(Str)])
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
# EXPECTED
UNUSED VARIABLE - Color.md:21:5:21:25
DOES NOT EXIST - Color.md:41:34:41:44
DOES NOT EXIST - Color.md:41:52:41:62
DOES NOT EXIST - Color.md:41:70:41:80
DOES NOT EXIST - Color.md:42:39:42:49
DOES NOT EXIST - Color.md:42:57:42:67
DOES NOT EXIST - Color.md:42:75:42:85
DOES NOT EXIST - Color.md:42:93:42:103
MISSING METHOD - Color.md:13:17:13:24
MISSING METHOD - Color.md:26:19:26:39
MISSING METHOD - Color.md:53:12:53:26
MISSING METHOD - Color.md:47:26:47:32
MISSING METHOD - Color.md:48:32:48:38
# PROBLEMS

┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `is_char_in_hex_range` is defined here and ─────┐
└┬────────────────┘  then never used.                                         │
 │                                                                            │
 │  is_char_in_hex_range = |b| (b >= '0' and b <= '9') or (b >= 'a' and b <=… │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └───────────────────────────────────────────────────────────── Color.md:21:5 ┘

    If you don't need this variable, prefix it with an underscore like
    `_is_char_in_hex_range` to suppress this warning.


┌────────────────┐
│ DOES NOT EXIST ├─ `Num.to_str` does not exist. ─────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  … "rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"             │
 │           ‾‾‾‾‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────────────────────────── Color.md:41:34 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `Num.to_str` does not exist. ─────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  …(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"                               │
 │           ‾‾‾‾‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────────────────────────── Color.md:41:52 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `Num.to_str` does not exist. ─────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  …(g)}, ${Num.to_str(b)})"                                                 │
 │           ‾‾‾‾‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────────────────────────── Color.md:41:70 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `Num.to_str` does not exist. ─────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  …"rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str…│
 │           ‾‾‾‾‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────────────────────────── Color.md:42:39 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `Num.to_str` does not exist. ─────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  …(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"             │
 │           ‾‾‾‾‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────────────────────────── Color.md:42:57 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `Num.to_str` does not exist. ─────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  …(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"                               │
 │           ‾‾‾‾‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────────────────────────── Color.md:42:75 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `Num.to_str` does not exist. ─────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  …(b)}, ${Num.to_str(a)})"                                                 │
 │           ‾‾‾‾‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────────────────────────── Color.md:42:93 ┘



┌────────────────┐
│ MISSING METHOD ├─ This `to_frac` method is being called on a value whose ───┐
└┬───────────────┘  type doesn't have that method.                            │
 │                                                                            │
 │  rounded = a.to_frac() / 255.0                                             │
 │              ‾‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────────────────────────── Color.md:13:17 ┘

    The value's type, which does not have a method named `to_frac`, is:

        U8

    Hint: For this to work, the type would need to have a method named
    `to_frac` associated with it in the type's declaration.


┌────────────────┐
│ MISSING METHOD ├─ This `is_char_in_hex_range` method is being called on a ──┐
└┬───────────────┘  value whose type doesn't have that method.                │
 │                                                                            │
 │  a.is_char_in_hex_range()                                                  │
 │    ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                    │
 └──────────────────────────────────────────────────────────── Color.md:26:19 ┘

    The value's type, which does not have a method named
    `is_char_in_hex_range`, is:

        U8

    Hint: For this to work, the type would need to have a method named
    `is_char_in_hex_range` associated with it in the type's declaration.


┌────────────────┐
│ MISSING METHOD ├─ This `is_named_color` method is being called on a value ──┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  if str.is_named_color()                                                   │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └──────────────────────────────────────────────────────────── Color.md:53:12 ┘

    The value's type, which does not have a method named `is_named_color`, is:

        Str

    Hint: For this to work, the type would need to have a method named
    `is_named_color` associated with it in the type's declaration.


┌────────────────┐
│ MISSING METHOD ├─ This `to_str` method is being called on a value whose ────┐
└┬───────────────┘  type doesn't have that method.                            │
 │                                                                            │
 │  expect rgb(124, 56, 245).to_str() == "rgb(124, 56, 245)"                  │
 │                           ‾‾‾‾‾‾                                           │
 └──────────────────────────────────────────────────────────── Color.md:47:26 ┘

    The value's type, which does not have a method named `to_str`, is:

        Color

    Hint: For this to work, the type would need to have a method named `to_str`
    associated with it in the type's declaration.


┌────────────────┐
│ MISSING METHOD ├─ This `to_str` method is being called on a value whose ────┐
└┬───────────────┘  type doesn't have that method.                            │
 │                                                                            │
 │  expect rgba(124, 56, 245, 255).to_str() == "rgba(124, 56, 245, 1.0)"      │
 │                                 ‾‾‾‾‾‾                                     │
 └──────────────────────────────────────────────────────────── Color.md:48:32 ┘

    The value's type, which does not have a method named `to_str`, is:

        Color

    Hint: For this to work, the type would need to have a method named `to_str`
    associated with it in the type's declaration.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,
UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,
CloseSquare,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpSlash,Float,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenRound,LowerIdent,OpGreaterThanOrEq,SingleQuote,OpAnd,LowerIdent,OpLessThanOrEq,SingleQuote,CloseRound,OpOr,OpenRound,LowerIdent,OpGreaterThanOrEq,SingleQuote,OpAnd,LowerIdent,OpLessThanOrEq,SingleQuote,CloseRound,OpOr,OpenRound,LowerIdent,OpGreaterThanOrEq,SingleQuote,OpAnd,LowerIdent,OpLessThanOrEq,SingleQuote,CloseRound,
KwMatch,LowerIdent,OpenCurly,
OpenSquare,SingleQuote,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,OpenCurly,
LowerIdent,OpAssign,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
OpAnd,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
OpAnd,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
OpAnd,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
OpAnd,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
OpAnd,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
KwIf,LowerIdent,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,CloseRound,
CloseCurly,
Underscore,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,Comma,Int,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,Comma,Int,Comma,Int,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpEquals,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwIf,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
KwElse,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseRound,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Color")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "RGB"))
						(ty (name "U8"))
						(ty (name "U8"))
						(ty (name "U8")))
					(ty-apply
						(ty (name "RGBA"))
						(ty (name "U8"))
						(ty (name "U8"))
						(ty (name "U8"))
						(ty (name "Dec")))
					(ty-apply
						(ty (name "Named"))
						(ty (name "Str")))
					(ty-apply
						(ty (name "Hex"))
						(ty (name "Str"))))))
		(s-type-anno (name "rgb")
			(ty-fn
				(ty (name "U8"))
				(ty (name "U8"))
				(ty (name "U8"))
				(ty (name "Color"))))
		(s-decl
			(p-ident (raw "rgb"))
			(e-lambda
				(args
					(p-ident (raw "r"))
					(p-ident (raw "g"))
					(p-ident (raw "b")))
				(e-apply
					(e-tag (raw "Color.RGB"))
					(e-ident (raw "r"))
					(e-ident (raw "g"))
					(e-ident (raw "b")))))
		(s-type-anno (name "rgba")
			(ty-fn
				(ty (name "U8"))
				(ty (name "U8"))
				(ty (name "U8"))
				(ty (name "U8"))
				(ty (name "Color"))))
		(s-decl
			(p-ident (raw "rgba"))
			(e-lambda
				(args
					(p-ident (raw "r"))
					(p-ident (raw "g"))
					(p-ident (raw "b"))
					(p-ident (raw "a")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "rounded"))
							(e-binop (op "/")
								(e-method-call (method ".to_frac")
									(receiver
										(e-ident (raw "a")))
									(args))
								(e-frac (raw "255.0"))))
						(e-apply
							(e-tag (raw "Color.RGBA"))
							(e-ident (raw "r"))
							(e-ident (raw "g"))
							(e-ident (raw "b"))
							(e-ident (raw "rounded")))))))
		(s-type-anno (name "hex")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Color"))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "InvalidHex"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "hex"))
			(e-lambda
				(args
					(p-ident (raw "str")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "bytes"))
							(e-method-call (method ".to_utf8")
								(receiver
									(e-ident (raw "str")))
								(args)))
						(s-decl
							(p-ident (raw "is_char_in_hex_range"))
							(e-lambda
								(args
									(p-ident (raw "b")))
								(e-binop (op "or")
									(e-tuple
										(e-binop (op "and")
											(e-binop (op ">=")
												(e-ident (raw "b"))
												(e-single-quote (raw "'0'")))
											(e-binop (op "<=")
												(e-ident (raw "b"))
												(e-single-quote (raw "'9'")))))
									(e-binop (op "or")
										(e-tuple
											(e-binop (op "and")
												(e-binop (op ">=")
													(e-ident (raw "b"))
													(e-single-quote (raw "'a'")))
												(e-binop (op "<=")
													(e-ident (raw "b"))
													(e-single-quote (raw "'f'")))))
										(e-tuple
											(e-binop (op "and")
												(e-binop (op ">=")
													(e-ident (raw "b"))
													(e-single-quote (raw "'A'")))
												(e-binop (op "<=")
													(e-ident (raw "b"))
													(e-single-quote (raw "'F'")))))))))
						(e-match
							(e-ident (raw "bytes"))
							(branches
								(branch
									(p-list
										(p-single-quote (raw "'#'"))
										(p-ident (raw "a"))
										(p-ident (raw "b"))
										(p-ident (raw "c"))
										(p-ident (raw "d"))
										(p-ident (raw "e"))
										(p-ident (raw "f")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "is_valid"))
												(e-binop (op "and")
													(e-method-call (method ".is_char_in_hex_range")
														(receiver
															(e-ident (raw "a")))
														(args))
													(e-binop (op "and")
														(e-method-call (method ".is_char_in_hex_range")
															(receiver
																(e-ident (raw "b")))
															(args))
														(e-binop (op "and")
															(e-method-call (method ".is_char_in_hex_range")
																(receiver
																	(e-ident (raw "c")))
																(args))
															(e-binop (op "and")
																(e-method-call (method ".is_char_in_hex_range")
																	(receiver
																		(e-ident (raw "d")))
																	(args))
																(e-binop (op "and")
																	(e-method-call (method ".is_char_in_hex_range")
																		(receiver
																			(e-ident (raw "e")))
																		(args))
																	(e-method-call (method ".is_char_in_hex_range")
																		(receiver
																			(e-ident (raw "f")))
																		(args))))))))
											(e-if-then-else
												(e-ident (raw "is_valid"))
												(e-apply
													(e-tag (raw "Ok"))
													(e-apply
														(e-tag (raw "Color.Hex"))
														(e-ident (raw "str"))))
												(e-apply
													(e-tag (raw "Err"))
													(e-apply
														(e-tag (raw "InvalidHex"))
														(e-string
															(e-string-part (raw "Expected Hex to be in the range 0-9, a-f, A-F, got "))
															(e-ident (raw "str"))
															(e-string-part (raw "")))))))))
								(branch
									(p-underscore)
									(e-apply
										(e-tag (raw "Err"))
										(e-apply
											(e-tag (raw "InvalidHex"))
											(e-string
												(e-string-part (raw "Expected Hex must start with # and be 7 characters long, got "))
												(e-ident (raw "str"))
												(e-string-part (raw ""))))))))))))
		(s-type-anno (name "to_str")
			(ty-fn
				(ty (name "Color"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "to_str"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-match
					(e-ident (raw "color"))
					(branches
						(branch
							(p-tag (raw ".RGB")
								(p-ident (raw "r"))
								(p-ident (raw "g"))
								(p-ident (raw "b")))
							(e-string
								(e-string-part (raw "rgb("))
								(e-apply
									(e-ident (raw "Num.to_str"))
									(e-ident (raw "r")))
								(e-string-part (raw ", "))
								(e-apply
									(e-ident (raw "Num.to_str"))
									(e-ident (raw "g")))
								(e-string-part (raw ", "))
								(e-apply
									(e-ident (raw "Num.to_str"))
									(e-ident (raw "b")))
								(e-string-part (raw ")"))))
						(branch
							(p-tag (raw ".RGBA")
								(p-ident (raw "r"))
								(p-ident (raw "g"))
								(p-ident (raw "b"))
								(p-ident (raw "a")))
							(e-string
								(e-string-part (raw "rgba("))
								(e-apply
									(e-ident (raw "Num.to_str"))
									(e-ident (raw "r")))
								(e-string-part (raw ", "))
								(e-apply
									(e-ident (raw "Num.to_str"))
									(e-ident (raw "g")))
								(e-string-part (raw ", "))
								(e-apply
									(e-ident (raw "Num.to_str"))
									(e-ident (raw "b")))
								(e-string-part (raw ", "))
								(e-apply
									(e-ident (raw "Num.to_str"))
									(e-ident (raw "a")))
								(e-string-part (raw ")"))))
						(branch
							(p-tag (raw ".Named")
								(p-ident (raw "inner")))
							(e-ident (raw "inner")))
						(branch
							(p-tag (raw ".Hex")
								(p-ident (raw "inner")))
							(e-ident (raw "inner")))))))
		(s-expect
			(e-binop (op "==")
				(e-method-call (method ".to_str")
					(receiver
						(e-apply
							(e-ident (raw "rgb"))
							(e-int (raw "124"))
							(e-int (raw "56"))
							(e-int (raw "245"))))
					(args))
				(e-string
					(e-string-part (raw "rgb(124, 56, 245)")))))
		(s-expect
			(e-binop (op "==")
				(e-method-call (method ".to_str")
					(receiver
						(e-apply
							(e-ident (raw "rgba"))
							(e-int (raw "124"))
							(e-int (raw "56"))
							(e-int (raw "245"))
							(e-int (raw "255"))))
					(args))
				(e-string
					(e-string-part (raw "rgba(124, 56, 245, 1.0)")))))
		(s-expect
			(e-binop (op "==")
				(e-method-call (method ".map_ok")
					(receiver
						(e-apply
							(e-ident (raw "hex"))
							(e-string
								(e-string-part (raw "#ff00ff")))))
					(args
						(e-ident (raw "to_str"))))
				(e-apply
					(e-tag (raw "Ok"))
					(e-string
						(e-string-part (raw "#ff00ff"))))))
		(s-type-anno (name "named")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Color"))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "UnknownColor"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "named"))
			(e-lambda
				(args
					(p-ident (raw "str")))
				(e-if-then-else
					(e-method-call (method ".is_named_color")
						(receiver
							(e-ident (raw "str")))
						(args))
					(e-apply
						(e-tag (raw "Ok"))
						(e-apply
							(e-tag (raw "Color.Named"))
							(e-ident (raw "str"))))
					(e-apply
						(e-tag (raw "Err"))
						(e-apply
							(e-tag (raw "UnknownColor"))
							(e-string
								(e-string-part (raw "Unknown color "))
								(e-ident (raw "str"))
								(e-string-part (raw ""))))))))
		(s-decl
			(p-ident (raw "is_named_color"))
			(e-lambda
				(args
					(p-ident (raw "str")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "colors"))
							(e-apply
								(e-ident (raw "Set.from_list"))
								(e-list
									(e-string
										(e-string-part (raw "AliceBlue")))
									(e-string
										(e-string-part (raw "AntiqueWhite")))
									(e-string
										(e-string-part (raw "Aqua"))))))
						(e-method-call (method ".contains")
							(receiver
								(e-ident (raw "colors")))
							(args
								(e-ident (raw "str"))))))))))
~~~
# FORMATTED
~~~roc
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

hex : Str -> Try(Color, [InvalidHex(Str)])
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

named : Str -> Try(Color, [UnknownColor(Str)])
named = |str|
	if str.is_named_color()
		Ok(Color.Named(str))
	else
		Err(UnknownColor("Unknown color ${str}"))

is_named_color = |str| {
	colors = Set.from_list(["AliceBlue", "AntiqueWhite", "Aqua"])

	colors.contains(str)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "rgb"))
		(e-lambda
			(args
				(p-assign (ident "r"))
				(p-assign (ident "g"))
				(p-assign (ident "b")))
			(e-nominal (nominal "Color")
				(e-tag (name "RGB")
					(args
						(e-lookup-local
							(p-assign (ident "r")))
						(e-lookup-local
							(p-assign (ident "g")))
						(e-lookup-local
							(p-assign (ident "b")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Color") (local)))))
	(d-let
		(p-assign (ident "rgba"))
		(e-lambda
			(args
				(p-assign (ident "r"))
				(p-assign (ident "g"))
				(p-assign (ident "b"))
				(p-assign (ident "a")))
			(e-block
				(s-let
					(p-assign (ident "rounded"))
					(e-dispatch-call (method "div_by") (constraint-fn-var 649)
						(receiver
							(e-dispatch-call (method "to_frac") (constraint-fn-var 613)
								(receiver
									(e-lookup-local
										(p-assign (ident "a"))))
								(args)))
						(args
							(e-dec-small (numerator "255") (denominator-power-of-ten "0") (value "255")))))
				(e-nominal (nominal "Color")
					(e-tag (name "RGBA")
						(args
							(e-lookup-local
								(p-assign (ident "r")))
							(e-lookup-local
								(p-assign (ident "g")))
							(e-lookup-local
								(p-assign (ident "b")))
							(e-lookup-local
								(p-assign (ident "rounded"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Color") (local)))))
	(d-let
		(p-assign (ident "hex"))
		(e-lambda
			(args
				(p-assign (ident "str")))
			(e-block
				(s-let
					(p-assign (ident "bytes"))
					(e-dispatch-call (method "to_utf8") (constraint-fn-var 844)
						(receiver
							(e-lookup-local
								(p-assign (ident "str"))))
						(args)))
				(s-let
					(p-assign (ident "is_char_in_hex_range"))
					(e-lambda
						(args
							(p-assign (ident "b")))
						(e-if
							(if-branches
								(if-branch
									(e-if
										(if-branches
											(if-branch
												(e-dispatch-call (method "is_gte") (constraint-fn-var 902)
													(receiver
														(e-lookup-local
															(p-assign (ident "b"))))
													(args
														(e-num (value "48"))))
												(e-dispatch-call (method "is_lte") (constraint-fn-var 943)
													(receiver
														(e-lookup-local
															(p-assign (ident "b"))))
													(args
														(e-num (value "57"))))))
										(if-else
											(e-nominal-external
												(builtin)
												(e-tag (name "False")))))
									(e-nominal-external
										(builtin)
										(e-tag (name "True")))))
							(if-else
								(e-if
									(if-branches
										(if-branch
											(e-if
												(if-branches
													(if-branch
														(e-dispatch-call (method "is_gte") (constraint-fn-var 997)
															(receiver
																(e-lookup-local
																	(p-assign (ident "b"))))
															(args
																(e-num (value "97"))))
														(e-dispatch-call (method "is_lte") (constraint-fn-var 1038)
															(receiver
																(e-lookup-local
																	(p-assign (ident "b"))))
															(args
																(e-num (value "102"))))))
												(if-else
													(e-nominal-external
														(builtin)
														(e-tag (name "False")))))
											(e-nominal-external
												(builtin)
												(e-tag (name "True")))))
									(if-else
										(e-if
											(if-branches
												(if-branch
													(e-dispatch-call (method "is_gte") (constraint-fn-var 1089)
														(receiver
															(e-lookup-local
																(p-assign (ident "b"))))
														(args
															(e-num (value "65"))))
													(e-dispatch-call (method "is_lte") (constraint-fn-var 1130)
														(receiver
															(e-lookup-local
																(p-assign (ident "b"))))
														(args
															(e-num (value "70"))))))
											(if-else
												(e-nominal-external
													(builtin)
													(e-tag (name "False")))))))))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "bytes"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-list
											(patterns
												(p-num (value "35"))
												(p-assign (ident "a"))
												(p-assign (ident "b"))
												(p-assign (ident "c"))
												(p-assign (ident "d"))
												(p-assign (ident "e"))
												(p-assign (ident "f"))))))
								(value
									(e-block
										(s-let
											(p-assign (ident "is_valid"))
											(e-if
												(if-branches
													(if-branch
														(e-dispatch-call (method "is_char_in_hex_range") (constraint-fn-var 1262)
															(receiver
																(e-lookup-local
																	(p-assign (ident "a"))))
															(args))
														(e-if
															(if-branches
																(if-branch
																	(e-dispatch-call (method "is_char_in_hex_range") (constraint-fn-var 1268)
																		(receiver
																			(e-lookup-local
																				(p-assign (ident "b"))))
																		(args))
																	(e-if
																		(if-branches
																			(if-branch
																				(e-dispatch-call (method "is_char_in_hex_range") (constraint-fn-var 1274)
																					(receiver
																						(e-lookup-local
																							(p-assign (ident "c"))))
																					(args))
																				(e-if
																					(if-branches
																						(if-branch
																							(e-dispatch-call (method "is_char_in_hex_range") (constraint-fn-var 1280)
																								(receiver
																									(e-lookup-local
																										(p-assign (ident "d"))))
																								(args))
																							(e-if
																								(if-branches
																									(if-branch
																										(e-dispatch-call (method "is_char_in_hex_range") (constraint-fn-var 1286)
																											(receiver
																												(e-lookup-local
																													(p-assign (ident "e"))))
																											(args))
																										(e-dispatch-call (method "is_char_in_hex_range") (constraint-fn-var 1292)
																											(receiver
																												(e-lookup-local
																													(p-assign (ident "f"))))
																											(args))))
																								(if-else
																									(e-nominal-external
																										(builtin)
																										(e-tag (name "False")))))))
																					(if-else
																						(e-nominal-external
																							(builtin)
																							(e-tag (name "False")))))))
																		(if-else
																			(e-nominal-external
																				(builtin)
																				(e-tag (name "False")))))))
															(if-else
																(e-nominal-external
																	(builtin)
																	(e-tag (name "False")))))))
												(if-else
													(e-nominal-external
														(builtin)
														(e-tag (name "False"))))))
										(e-if
											(if-branches
												(if-branch
													(e-lookup-local
														(p-assign (ident "is_valid")))
													(e-tag (name "Ok")
														(args
															(e-nominal (nominal "Color")
																(e-tag (name "Hex")
																	(args
																		(e-lookup-local
																			(p-assign (ident "str"))))))))))
											(if-else
												(e-tag (name "Err")
													(args
														(e-tag (name "InvalidHex")
															(args
																(e-block
																	(s-let
																		(p-assign (ident "#interp_0"))
																		(e-lookup-local
																			(p-assign (ident "str"))))
																	(e-interpolation (constraint-fn-var 1411)
																		(first
																			(e-literal (string "Expected Hex to be in the range 0-9, a-f, A-F, got ")))
																		(parts
																			(e-lookup-local
																				(p-assign (ident "#interp_0")))
																			(e-literal (string ""))))))))))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-underscore)))
								(value
									(e-tag (name "Err")
										(args
											(e-tag (name "InvalidHex")
												(args
													(e-block
														(s-let
															(p-assign (ident "#interp_1"))
															(e-lookup-local
																(p-assign (ident "str"))))
														(e-interpolation (constraint-fn-var 1475)
															(first
																(e-literal (string "Expected Hex must start with # and be 7 characters long, got ")))
															(parts
																(e-lookup-local
																	(p-assign (ident "#interp_1")))
																(e-literal (string ""))))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Color") (local))
					(ty-tag-union
						(ty-tag-name (name "InvalidHex")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "to_str"))
		(e-lambda
			(args
				(p-assign (ident "color")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "color"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-block
									(s-let
										(p-assign (ident "#interp_2"))
										(e-call
											(e-runtime-error (tag "qualified_ident_does_not_exist"))
											(e-lookup-local
												(p-assign (ident "r")))))
									(s-let
										(p-assign (ident "#interp_3"))
										(e-call
											(e-runtime-error (tag "qualified_ident_does_not_exist"))
											(e-lookup-local
												(p-assign (ident "g")))))
									(s-let
										(p-assign (ident "#interp_4"))
										(e-call
											(e-runtime-error (tag "qualified_ident_does_not_exist"))
											(e-lookup-local
												(p-assign (ident "b")))))
									(e-interpolation
										(first
											(e-literal (string "rgb(")))
										(parts
											(e-lookup-local
												(p-assign (ident "#interp_2")))
											(e-literal (string ", "))
											(e-lookup-local
												(p-assign (ident "#interp_3")))
											(e-literal (string ", "))
											(e-lookup-local
												(p-assign (ident "#interp_4")))
											(e-literal (string ")")))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-block
									(s-let
										(p-assign (ident "#interp_5"))
										(e-call
											(e-runtime-error (tag "qualified_ident_does_not_exist"))
											(e-lookup-local
												(p-assign (ident "r")))))
									(s-let
										(p-assign (ident "#interp_6"))
										(e-call
											(e-runtime-error (tag "qualified_ident_does_not_exist"))
											(e-lookup-local
												(p-assign (ident "g")))))
									(s-let
										(p-assign (ident "#interp_7"))
										(e-call
											(e-runtime-error (tag "qualified_ident_does_not_exist"))
											(e-lookup-local
												(p-assign (ident "b")))))
									(s-let
										(p-assign (ident "#interp_8"))
										(e-call
											(e-runtime-error (tag "qualified_ident_does_not_exist"))
											(e-lookup-local
												(p-assign (ident "a")))))
									(e-interpolation
										(first
											(e-literal (string "rgba(")))
										(parts
											(e-lookup-local
												(p-assign (ident "#interp_5")))
											(e-literal (string ", "))
											(e-lookup-local
												(p-assign (ident "#interp_6")))
											(e-literal (string ", "))
											(e-lookup-local
												(p-assign (ident "#interp_7")))
											(e-literal (string ", "))
											(e-lookup-local
												(p-assign (ident "#interp_8")))
											(e-literal (string ")")))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-lookup-local
									(p-assign (ident "inner")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-lookup-local
									(p-assign (ident "inner")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Color") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "named"))
		(e-lambda
			(args
				(p-assign (ident "str")))
			(e-if
				(if-branches
					(if-branch
						(e-dispatch-call (method "is_named_color") (constraint-fn-var 1887)
							(receiver
								(e-lookup-local
									(p-assign (ident "str"))))
							(args))
						(e-tag (name "Ok")
							(args
								(e-nominal (nominal "Color")
									(e-tag (name "Named")
										(args
											(e-lookup-local
												(p-assign (ident "str"))))))))))
				(if-else
					(e-tag (name "Err")
						(args
							(e-tag (name "UnknownColor")
								(args
									(e-block
										(s-let
											(p-assign (ident "#interp_9"))
											(e-lookup-local
												(p-assign (ident "str"))))
										(e-interpolation (constraint-fn-var 1980)
											(first
												(e-literal (string "Unknown color ")))
											(parts
												(e-lookup-local
													(p-assign (ident "#interp_9")))
												(e-literal (string ""))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Color") (local))
					(ty-tag-union
						(ty-tag-name (name "UnknownColor")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "is_named_color"))
		(e-lambda
			(args
				(p-assign (ident "str")))
			(e-block
				(s-let
					(p-assign (ident "colors"))
					(e-call (constraint-fn-var 2070)
						(e-lookup-external
							(builtin))
						(e-list
							(elems
								(e-string
									(e-literal (string "AliceBlue")))
								(e-string
									(e-literal (string "AntiqueWhite")))
								(e-string
									(e-literal (string "Aqua")))))))
				(e-dispatch-call (method "contains") (constraint-fn-var 2071)
					(receiver
						(e-lookup-local
							(p-assign (ident "colors"))))
					(args
						(e-lookup-local
							(p-assign (ident "str"))))))))
	(s-nominal-decl
		(ty-header (name "Color"))
		(ty-tag-union
			(ty-tag-name (name "RGB")
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin)))
			(ty-tag-name (name "RGBA")
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Dec") (builtin)))
			(ty-tag-name (name "Named")
				(ty-lookup (name "Str") (builtin)))
			(ty-tag-name (name "Hex")
				(ty-lookup (name "Str") (builtin)))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-dispatch-call (method "to_str") (constraint-fn-var 2413)
					(receiver
						(e-call (constraint-fn-var 2202)
							(e-lookup-local
								(p-assign (ident "rgb")))
							(e-num (value "124"))
							(e-num (value "56"))
							(e-num (value "245"))))
					(args)))
			(rhs
				(e-string
					(e-literal (string "rgb(124, 56, 245)"))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-dispatch-call (method "to_str") (constraint-fn-var 2853)
					(receiver
						(e-call (constraint-fn-var 2572)
							(e-lookup-local
								(p-assign (ident "rgba")))
							(e-num (value "124"))
							(e-num (value "56"))
							(e-num (value "245"))
							(e-num (value "255"))))
					(args)))
			(rhs
				(e-string
					(e-literal (string "rgba(124, 56, 245, 1.0)"))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-dispatch-call (method "map_ok") (constraint-fn-var 2921)
					(receiver
						(e-call (constraint-fn-var 2896)
							(e-lookup-local
								(p-assign (ident "hex")))
							(e-string
								(e-literal (string "#ff00ff")))))
					(args
						(e-lookup-local
							(p-assign (ident "to_str"))))))
			(rhs
				(e-tag (name "Ok")
					(args
						(e-string
							(e-literal (string "#ff00ff")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8, U8, U8 -> Color"))
		(patt (type "U8, U8, U8, U8 -> Color"))
		(patt (type "Str -> Try(Color, [InvalidHex(Str)])"))
		(patt (type "Color -> Str"))
		(patt (type "Str -> Try(Color, [UnknownColor(Str)])"))
		(patt (type "a -> Bool where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]")))
	(type_decls
		(nominal (type "Color")
			(ty-header (name "Color"))))
	(expressions
		(expr (type "U8, U8, U8 -> Color"))
		(expr (type "U8, U8, U8, U8 -> Color"))
		(expr (type "Str -> Try(Color, [InvalidHex(Str)])"))
		(expr (type "Color -> Str"))
		(expr (type "Str -> Try(Color, [UnknownColor(Str)])"))
		(expr (type "a -> Bool where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]"))))
~~~
