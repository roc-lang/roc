# META
~~~ini
description=Fractional pattern literals have Dec type, causing mismatch with F32/F64
type=expr
~~~
# SOURCE
~~~roc
{
    # This works - Dec matches fractional patterns
    dec_value : Dec
    dec_value = 3.14

    dec_result =
        match dec_value {
            3.14 => "pi"
            _ => "other"
        }

    # This fails - F32 doesn't match Dec patterns
    f32_value : F32
    f32_value = 3.14

    f32_result =
        match f32_value {
            3.14 => "pi"  # Type mismatch here
            _ => "other"
        }

    # This also fails - F64 doesn't match Dec patterns
    f64_value : F64
    f64_value = 3.14

    f64_result =
        match f64_value {
            3.14 => "pi"  # Type mismatch here
            _ => "other"
        }

    (dec_result, f32_result, f64_result)
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
Newline(2:6-2:51),
LowerIdent(3:5-3:14),OpColon(3:15-3:16),UpperIdent(3:17-3:20),Newline(1:1-1:1),
LowerIdent(4:5-4:14),OpAssign(4:15-4:16),Float(4:17-4:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:5-6:15),OpAssign(6:16-6:17),Newline(1:1-1:1),
KwMatch(7:9-7:14),LowerIdent(7:15-7:24),OpenCurly(7:25-7:26),Newline(1:1-1:1),
Float(8:13-8:17),OpFatArrow(8:18-8:20),StringStart(8:21-8:22),StringPart(8:22-8:24),StringEnd(8:24-8:25),Newline(1:1-1:1),
Underscore(9:13-9:14),OpFatArrow(9:15-9:17),StringStart(9:18-9:19),StringPart(9:19-9:24),StringEnd(9:24-9:25),Newline(1:1-1:1),
CloseCurly(10:9-10:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:6-12:50),
LowerIdent(13:5-13:14),OpColon(13:15-13:16),UpperIdent(13:17-13:20),Newline(1:1-1:1),
LowerIdent(14:5-14:14),OpAssign(14:15-14:16),Float(14:17-14:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(16:5-16:15),OpAssign(16:16-16:17),Newline(1:1-1:1),
KwMatch(17:9-17:14),LowerIdent(17:15-17:24),OpenCurly(17:25-17:26),Newline(1:1-1:1),
Float(18:13-18:17),OpFatArrow(18:18-18:20),StringStart(18:21-18:22),StringPart(18:22-18:24),StringEnd(18:24-18:25),Newline(18:28-18:47),
Underscore(19:13-19:14),OpFatArrow(19:15-19:17),StringStart(19:18-19:19),StringPart(19:19-19:24),StringEnd(19:24-19:25),Newline(1:1-1:1),
CloseCurly(20:9-20:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(22:6-22:55),
LowerIdent(23:5-23:14),OpColon(23:15-23:16),UpperIdent(23:17-23:20),Newline(1:1-1:1),
LowerIdent(24:5-24:14),OpAssign(24:15-24:16),Float(24:17-24:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(26:5-26:15),OpAssign(26:16-26:17),Newline(1:1-1:1),
KwMatch(27:9-27:14),LowerIdent(27:15-27:24),OpenCurly(27:25-27:26),Newline(1:1-1:1),
Float(28:13-28:17),OpFatArrow(28:18-28:20),StringStart(28:21-28:22),StringPart(28:22-28:24),StringEnd(28:24-28:25),Newline(28:28-28:47),
Underscore(29:13-29:14),OpFatArrow(29:15-29:17),StringStart(29:18-29:19),StringPart(29:19-29:24),StringEnd(29:24-29:25),Newline(1:1-1:1),
CloseCurly(30:9-30:10),Newline(1:1-1:1),
Newline(1:1-1:1),
OpenRound(32:5-32:6),LowerIdent(32:6-32:16),Comma(32:16-32:17),LowerIdent(32:18-32:28),Comma(32:28-32:29),LowerIdent(32:30-32:40),CloseRound(32:40-32:41),Newline(1:1-1:1),
CloseCurly(33:1-33:2),EndOfFile(33:2-33:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-33.2
	(statements
		(s-type-anno @3.5-4.14 (name "dec_value")
			(ty (name "Dec")))
		(s-decl @4.5-4.21
			(p-ident @4.5-4.14 (raw "dec_value"))
			(e-frac @4.17-4.21 (raw "3.14")))
		(s-decl @6.5-10.10
			(p-ident @6.5-6.15 (raw "dec_result"))
			(e-match
				(e-ident @7.15-7.24 (qaul "") (raw "dec_value"))
				(branches
					(branch @1.1-1.1
						(p-frac @8.13-8.17 (raw "3.14"))
						(e-string @8.21-8.25
							(e-string-part @8.22-8.24 (raw "pi"))))
					(branch @1.1-1.1
						(p-underscore)
						(e-string @9.18-9.25
							(e-string-part @9.19-9.24 (raw "other")))))))
		(s-type-anno @13.5-14.14 (name "f32_value")
			(ty (name "F32")))
		(s-decl @14.5-14.21
			(p-ident @14.5-14.14 (raw "f32_value"))
			(e-frac @14.17-14.21 (raw "3.14")))
		(s-decl @16.5-20.10
			(p-ident @16.5-16.15 (raw "f32_result"))
			(e-match
				(e-ident @17.15-17.24 (qaul "") (raw "f32_value"))
				(branches
					(branch @18.13-18.47
						(p-frac @18.13-18.17 (raw "3.14"))
						(e-string @18.21-18.25
							(e-string-part @18.22-18.24 (raw "pi"))))
					(branch @1.1-1.1
						(p-underscore)
						(e-string @19.18-19.25
							(e-string-part @19.19-19.24 (raw "other")))))))
		(s-type-anno @23.5-24.14 (name "f64_value")
			(ty (name "F64")))
		(s-decl @24.5-24.21
			(p-ident @24.5-24.14 (raw "f64_value"))
			(e-frac @24.17-24.21 (raw "3.14")))
		(s-decl @26.5-30.10
			(p-ident @26.5-26.15 (raw "f64_result"))
			(e-match
				(e-ident @27.15-27.24 (qaul "") (raw "f64_value"))
				(branches
					(branch @28.13-28.47
						(p-frac @28.13-28.17 (raw "3.14"))
						(e-string @28.21-28.25
							(e-string-part @28.22-28.24 (raw "pi"))))
					(branch @1.1-1.1
						(p-underscore)
						(e-string @29.18-29.25
							(e-string-part @29.19-29.24 (raw "other")))))))
		(e-tuple @32.5-32.41
			(e-ident @32.6-32.16 (qaul "") (raw "dec_result"))
			(e-ident @32.18-32.28 (qaul "") (raw "f32_result"))
			(e-ident @32.30-32.40 (qaul "") (raw "f64_result")))))
~~~
# FORMATTED
~~~roc
{
	# This works - Dec matches fractional patterns
	dec_value : Dec
	dec_value = 3.14

	dec_result = 
		match dec_value {
			3.14 => "pi"
			_ => "other"
		}

	# This fails - F32 doesn't match Dec patterns
	f32_value : F32
	f32_value = 3.14

	f32_result = 
		match f32_value {
			3.14 => "pi" # Type mismatch here
			_ => "other"
		}

	# This also fails - F64 doesn't match Dec patterns
	f64_value : F64
	f64_value = 3.14

	f64_result = 
		match f64_value {
			3.14 => "pi" # Type mismatch here
			_ => "other"
		}

	(dec_result, f32_result, f64_result)
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-33.2
	(s-type-anno @3.5-4.14 (name "dec_value")
		(ty @3.17-3.20 (name "Dec")))
	(s-let @4.5-4.21
		(p-assign @4.5-4.14 (ident "dec_value"))
		(e-dec-small @4.17-4.21 (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(s-let @6.5-10.10
		(p-assign @6.5-6.15 (ident "dec_result"))
		(e-match @7.9-10.10
			(match @7.9-10.10
				(cond
					(e-lookup-local @7.15-7.24
						(pattern @4.5-4.14)))
				(branches
					(branch
						(patterns
							(p-small-dec @8.13-8.17 (degenerate false)))
						(value
							(e-string @8.21-8.25
								(e-literal @8.22-8.24 (string "pi")))))
					(branch
						(patterns
							(p-underscore @9.13-9.14 (degenerate false)))
						(value
							(e-string @9.18-9.25
								(e-literal @9.19-9.24 (string "other")))))))))
	(s-type-anno @13.5-14.14 (name "f32_value")
		(ty @13.17-13.20 (name "F32")))
	(s-let @14.5-14.21
		(p-assign @14.5-14.14 (ident "f32_value"))
		(e-dec-small @14.17-14.21 (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(s-let @16.5-20.10
		(p-assign @16.5-16.15 (ident "f32_result"))
		(e-match @17.9-20.10
			(match @17.9-20.10
				(cond
					(e-lookup-local @17.15-17.24
						(pattern @14.5-14.14)))
				(branches
					(branch
						(patterns
							(p-small-dec @18.13-18.17 (degenerate false)))
						(value
							(e-string @18.21-18.25
								(e-literal @18.22-18.24 (string "pi")))))
					(branch
						(patterns
							(p-underscore @19.13-19.14 (degenerate false)))
						(value
							(e-string @19.18-19.25
								(e-literal @19.19-19.24 (string "other")))))))))
	(s-type-anno @23.5-24.14 (name "f64_value")
		(ty @23.17-23.20 (name "F64")))
	(s-let @24.5-24.21
		(p-assign @24.5-24.14 (ident "f64_value"))
		(e-dec-small @24.17-24.21 (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(s-let @26.5-30.10
		(p-assign @26.5-26.15 (ident "f64_result"))
		(e-match @27.9-30.10
			(match @27.9-30.10
				(cond
					(e-lookup-local @27.15-27.24
						(pattern @24.5-24.14)))
				(branches
					(branch
						(patterns
							(p-small-dec @28.13-28.17 (degenerate false)))
						(value
							(e-string @28.21-28.25
								(e-literal @28.22-28.24 (string "pi")))))
					(branch
						(patterns
							(p-underscore @29.13-29.14 (degenerate false)))
						(value
							(e-string @29.18-29.25
								(e-literal @29.19-29.24 (string "other")))))))))
	(e-tuple @32.5-32.41
		(elems
			(e-lookup-local @32.6-32.16
				(pattern @6.5-6.15))
			(e-lookup-local @32.18-32.28
				(pattern @16.5-16.15))
			(e-lookup-local @32.30-32.40
				(pattern @26.5-26.15)))))
~~~
# TYPES
~~~clojure
(expr @1.1-33.2 (type "(Str, Str, Str)"))
~~~
