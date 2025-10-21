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
# EXPECTED
MODULE HEADER DEPRECATED - Color.md:1:1:8:2
UNUSED VARIABLE - Color.md:30:5:30:25
DOES NOT EXIST - Color.md:68:14:68:27
TYPE MISMATCH - Color.md:32:5:45:6
TYPE MISMATCH - Color.md:51:104:51:105
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**Color.md:1:1:8:2:**
```roc
module [
    Color,
    to_str,
    rgb,
    rgba,
    hex,
    named,
]
```


**UNUSED VARIABLE**
Variable `is_char_in_hex_range` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_is_char_in_hex_range` to suppress this warning.
The unused variable is declared here:
**Color.md:30:5:30:25:**
```roc
    is_char_in_hex_range = |b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')
```
    ^^^^^^^^^^^^^^^^^^^^


**DOES NOT EXIST**
`Set.from_list` does not exist.

**Color.md:68:14:68:27:**
```roc
    colors = Set.from_list(["AliceBlue", "AntiqueWhite", "Aqua"])
```
             ^^^^^^^^^^^^^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**Color.md:32:5:45:6:**
```roc
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
```

It has the type:
    _[InvalidHex(Str), Err([InvalidHex(Str)]_others)][Ok(Color)]_others2_

But the type annotation says it should have the type:
    _Result(Color, [InvalidHex(Str)])_

**TYPE MISMATCH**
The first argument being passed to this function has the wrong type:
**Color.md:51:104:51:105:**
```roc
    Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
```
                                                                                                       ^

This argument has the type:
    _Num(Frac(Decimal))_

But `to_str` needs the first argument to be:
    _Num(Int(Unsigned8))_

# TOKENS
~~~zig
KwModule,OpenSquare,
UpperIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
CloseSquare,
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
	(module
		(exposes
			(exposed-upper-ident (text "Color"))
			(exposed-lower-ident
				(text "to_str"))
			(exposed-lower-ident
				(text "rgb"))
			(exposed-lower-ident
				(text "rgba"))
			(exposed-lower-ident
				(text "hex"))
			(exposed-lower-ident
				(text "named"))))
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
								(e-field-access
									(e-ident (raw "a"))
									(e-apply
										(e-ident (raw "to_frac"))))
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
					(ty (name "Result"))
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
							(e-field-access
								(e-ident (raw "str"))
								(e-apply
									(e-ident (raw "to_utf8")))))
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
													(e-field-access
														(e-ident (raw "a"))
														(e-apply
															(e-ident (raw "is_char_in_hex_range"))))
													(e-binop (op "and")
														(e-field-access
															(e-ident (raw "b"))
															(e-apply
																(e-ident (raw "is_char_in_hex_range"))))
														(e-binop (op "and")
															(e-field-access
																(e-ident (raw "c"))
																(e-apply
																	(e-ident (raw "is_char_in_hex_range"))))
															(e-binop (op "and")
																(e-field-access
																	(e-ident (raw "d"))
																	(e-apply
																		(e-ident (raw "is_char_in_hex_range"))))
																(e-binop (op "and")
																	(e-field-access
																		(e-ident (raw "e"))
																		(e-apply
																			(e-ident (raw "is_char_in_hex_range"))))
																	(e-field-access
																		(e-ident (raw "f"))
																		(e-apply
																			(e-ident (raw "is_char_in_hex_range"))))))))))
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
				(e-field-access
					(e-apply
						(e-ident (raw "rgb"))
						(e-int (raw "124"))
						(e-int (raw "56"))
						(e-int (raw "245")))
					(e-apply
						(e-ident (raw "to_str"))))
				(e-string
					(e-string-part (raw "rgb(124, 56, 245)")))))
		(s-expect
			(e-binop (op "==")
				(e-field-access
					(e-apply
						(e-ident (raw "rgba"))
						(e-int (raw "124"))
						(e-int (raw "56"))
						(e-int (raw "245"))
						(e-int (raw "255")))
					(e-apply
						(e-ident (raw "to_str"))))
				(e-string
					(e-string-part (raw "rgba(124, 56, 245, 1.0)")))))
		(s-expect
			(e-binop (op "==")
				(e-field-access
					(e-apply
						(e-ident (raw "hex"))
						(e-string
							(e-string-part (raw "#ff00ff"))))
					(e-apply
						(e-ident (raw "map_ok"))
						(e-ident (raw "to_str"))))
				(e-apply
					(e-tag (raw "Ok"))
					(e-string
						(e-string-part (raw "#ff00ff"))))))
		(s-type-anno (name "named")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Result"))
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
					(e-field-access
						(e-ident (raw "str"))
						(e-apply
							(e-ident (raw "is_named_color"))))
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
						(e-field-access
							(e-ident (raw "colors"))
							(e-apply
								(e-ident (raw "contains"))
								(e-ident (raw "str"))))))))))
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
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U8") (builtin))
					(ty-lookup (name "U8") (builtin))
					(ty-lookup (name "U8") (builtin))
					(ty-lookup (name "Color") (local))))))
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
					(e-binop (op "div")
						(e-dot-access (field "to_frac")
							(receiver
								(e-lookup-local
									(p-assign (ident "a"))))
							(args))
						(e-dec-small (numerator "2550") (denominator-power-of-ten "1") (value "255"))))
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
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U8") (builtin))
					(ty-lookup (name "U8") (builtin))
					(ty-lookup (name "U8") (builtin))
					(ty-lookup (name "U8") (builtin))
					(ty-lookup (name "Color") (local))))))
	(d-let
		(p-assign (ident "hex"))
		(e-closure
			(captures
				(capture (ident "f"))
				(capture (ident "d"))
				(capture (ident "b"))
				(capture (ident "e"))
				(capture (ident "a"))
				(capture (ident "c"))
				(capture (ident "is_valid")))
			(e-lambda
				(args
					(p-assign (ident "str")))
				(e-block
					(s-let
						(p-assign (ident "bytes"))
						(e-dot-access (field "to_utf8")
							(receiver
								(e-lookup-local
									(p-assign (ident "str"))))
							(args)))
					(s-let
						(p-assign (ident "is_char_in_hex_range"))
						(e-lambda
							(args
								(p-assign (ident "b")))
							(e-binop (op "or")
								(e-binop (op "and")
									(e-binop (op "ge")
										(e-lookup-local
											(p-assign (ident "b")))
										(e-num (value "48")))
									(e-binop (op "le")
										(e-lookup-local
											(p-assign (ident "b")))
										(e-num (value "57"))))
								(e-binop (op "or")
									(e-binop (op "and")
										(e-binop (op "ge")
											(e-lookup-local
												(p-assign (ident "b")))
											(e-num (value "97")))
										(e-binop (op "le")
											(e-lookup-local
												(p-assign (ident "b")))
											(e-num (value "102"))))
									(e-binop (op "and")
										(e-binop (op "ge")
											(e-lookup-local
												(p-assign (ident "b")))
											(e-num (value "65")))
										(e-binop (op "le")
											(e-lookup-local
												(p-assign (ident "b")))
											(e-num (value "70"))))))))
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
												(e-binop (op "and")
													(e-dot-access (field "is_char_in_hex_range")
														(receiver
															(e-lookup-local
																(p-assign (ident "a"))))
														(args))
													(e-binop (op "and")
														(e-dot-access (field "is_char_in_hex_range")
															(receiver
																(e-lookup-local
																	(p-assign (ident "b"))))
															(args))
														(e-binop (op "and")
															(e-dot-access (field "is_char_in_hex_range")
																(receiver
																	(e-lookup-local
																		(p-assign (ident "c"))))
																(args))
															(e-binop (op "and")
																(e-dot-access (field "is_char_in_hex_range")
																	(receiver
																		(e-lookup-local
																			(p-assign (ident "d"))))
																	(args))
																(e-binop (op "and")
																	(e-dot-access (field "is_char_in_hex_range")
																		(receiver
																			(e-lookup-local
																				(p-assign (ident "e"))))
																		(args))
																	(e-dot-access (field "is_char_in_hex_range")
																		(receiver
																			(e-lookup-local
																				(p-assign (ident "f"))))
																		(args))))))))
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
																	(e-string
																		(e-literal (string "Expected Hex to be in the range 0-9, a-f, A-F, got "))
																		(e-lookup-local
																			(p-assign (ident "str")))
																		(e-literal (string ""))))))))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-tag (name "Err")
											(args
												(e-tag (name "InvalidHex")
													(args
														(e-string
															(e-literal (string "Expected Hex must start with # and be 7 characters long, got "))
															(e-lookup-local
																(p-assign (ident "str")))
															(e-literal (string "")))))))))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "Str") (builtin))
					(ty-apply (name "Result") (external-module "Result")
						(ty-lookup (name "Color") (local))
						(ty-tag-union
							(ty-tag-name (name "InvalidHex")
								(ty-lookup (name "Str") (builtin)))))))))
	(d-let
		(p-assign (ident "to_str"))
		(e-closure
			(captures
				(capture (ident "b"))
				(capture (ident "g"))
				(capture (ident "r"))
				(capture (ident "to_str"))
				(capture (ident "b"))
				(capture (ident "inner"))
				(capture (ident "r"))
				(capture (ident "g"))
				(capture (ident "a"))
				(capture (ident "inner")))
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
									(e-string
										(e-literal (string "rgb("))
										(e-call
											(e-lookup-local
												(p-assign (ident "to_str")))
											(e-lookup-local
												(p-assign (ident "r"))))
										(e-literal (string ", "))
										(e-call
											(e-lookup-local
												(p-assign (ident "to_str")))
											(e-lookup-local
												(p-assign (ident "g"))))
										(e-literal (string ", "))
										(e-call
											(e-lookup-local
												(p-assign (ident "to_str")))
											(e-lookup-local
												(p-assign (ident "b"))))
										(e-literal (string ")")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal
											(p-applied-tag))))
								(value
									(e-string
										(e-literal (string "rgba("))
										(e-call
											(e-lookup-local
												(p-assign (ident "to_str")))
											(e-lookup-local
												(p-assign (ident "r"))))
										(e-literal (string ", "))
										(e-call
											(e-lookup-local
												(p-assign (ident "to_str")))
											(e-lookup-local
												(p-assign (ident "g"))))
										(e-literal (string ", "))
										(e-call
											(e-lookup-local
												(p-assign (ident "to_str")))
											(e-lookup-local
												(p-assign (ident "b"))))
										(e-literal (string ", "))
										(e-call
											(e-lookup-local
												(p-assign (ident "to_str")))
											(e-lookup-local
												(p-assign (ident "a"))))
										(e-literal (string ")")))))
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
										(p-assign (ident "inner"))))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "Color") (local))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "named"))
		(e-lambda
			(args
				(p-assign (ident "str")))
			(e-if
				(if-branches
					(if-branch
						(e-dot-access (field "is_named_color")
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
									(e-string
										(e-literal (string "Unknown color "))
										(e-lookup-local
											(p-assign (ident "str")))
										(e-literal (string ""))))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "Str") (builtin))
					(ty-apply (name "Result") (external-module "Result")
						(ty-lookup (name "Color") (local))
						(ty-tag-union
							(ty-tag-name (name "UnknownColor")
								(ty-lookup (name "Str") (builtin)))))))))
	(d-let
		(p-assign (ident "is_named_color"))
		(e-lambda
			(args
				(p-assign (ident "str")))
			(e-block
				(s-let
					(p-assign (ident "colors"))
					(e-call
						(e-runtime-error (tag "qualified_ident_does_not_exist"))
						(e-list
							(elems
								(e-string
									(e-literal (string "AliceBlue")))
								(e-string
									(e-literal (string "AntiqueWhite")))
								(e-string
									(e-literal (string "Aqua")))))))
				(e-dot-access (field "contains")
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
		(e-binop (op "eq")
			(e-dot-access (field "to_str")
				(receiver
					(e-call
						(e-lookup-local
							(p-assign (ident "rgb")))
						(e-num (value "124"))
						(e-num (value "56"))
						(e-num (value "245"))))
				(args))
			(e-string
				(e-literal (string "rgb(124, 56, 245)")))))
	(s-expect
		(e-binop (op "eq")
			(e-dot-access (field "to_str")
				(receiver
					(e-call
						(e-lookup-local
							(p-assign (ident "rgba")))
						(e-num (value "124"))
						(e-num (value "56"))
						(e-num (value "245"))
						(e-num (value "255"))))
				(args))
			(e-string
				(e-literal (string "rgba(124, 56, 245, 1.0)")))))
	(s-expect
		(e-binop (op "eq")
			(e-dot-access (field "map_ok")
				(receiver
					(e-call
						(e-lookup-local
							(p-assign (ident "hex")))
						(e-string
							(e-literal (string "#ff00ff")))))
				(args
					(e-lookup-local
						(p-assign (ident "to_str")))))
			(e-tag (name "Ok")
				(args
					(e-string
						(e-literal (string "#ff00ff"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Unsigned8)), Num(Int(Unsigned8)), Num(Int(Unsigned8)) -> Color"))
		(patt (type "Num(Int(Unsigned8)), Num(Int(Unsigned8)), Num(Int(Unsigned8)), Num(Int(Unsigned8)) -> Color"))
		(patt (type "Str -> Error"))
		(patt (type "Error -> Error"))
		(patt (type "Str -> Result(Color, [UnknownColor(Str)])"))
		(patt (type "_arg -> Error")))
	(type_decls
		(nominal (type "Color")
			(ty-header (name "Color"))))
	(expressions
		(expr (type "Num(Int(Unsigned8)), Num(Int(Unsigned8)), Num(Int(Unsigned8)) -> Color"))
		(expr (type "Num(Int(Unsigned8)), Num(Int(Unsigned8)), Num(Int(Unsigned8)), Num(Int(Unsigned8)) -> Color"))
		(expr (type "Str -> Error"))
		(expr (type "Error -> Error"))
		(expr (type "Str -> Result(Color, [UnknownColor(Str)])"))
		(expr (type "_arg -> Error"))))
~~~
