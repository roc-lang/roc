# META
~~~ini
description=Test dec_small, frac_dec, and frac_f64 split
type=expr
~~~
# SOURCE
~~~roc
# This tests that different fractional literals are canonicalized to the appropriate type
[
    # dec_small - fits in i16 numerator with small power of 10
    0.1,      # 1/10^1
    3.14,     # 314/10^2
    -5.25,    # -525/10^2
    127.99,   # 12799/10^2
    
    # Should use frac_dec - exceeds i16 range
    32768.0,  # 327680/10^1 > 32767
    
    # Should use frac_f64 - has scientific notation
    1.5e10,   # Scientific notation forces f64
    1e-40,    # Very small scientific notation
    
    # Negative zero - should use frac_dec (loses sign)
    -0.0,
]
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token ** This tests that different fractional literals are canonicalized to the appropriate type
[** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_frac_split.md:1:2:2:2:**
```roc
# This tests that different fractional literals are canonicalized to the appropriate type
[
```


# TOKENS
~~~zig
Newline(1:2-1:90),
OpenSquare(2:1-2:2),Newline(1:1-1:1),
Newline(3:6-3:63),
Float(4:5-4:8),Comma(4:8-4:9),Newline(4:16-4:23),
Float(5:5-5:9),Comma(5:9-5:10),Newline(5:16-5:25),
Float(6:5-6:10),Comma(6:10-6:11),Newline(6:16-6:26),
Float(7:5-7:11),Comma(7:11-7:12),Newline(7:16-7:27),
Newline(1:1-1:1),
Newline(9:6-9:46),
Float(10:5-10:12),Comma(10:12-10:13),Newline(10:16-10:36),
Newline(1:1-1:1),
Newline(12:6-12:52),
Float(13:5-13:11),Comma(13:11-13:12),Newline(13:16-13:47),
Float(14:5-14:10),Comma(14:10-14:11),Newline(14:16-14:47),
Newline(1:1-1:1),
Newline(16:6-16:55),
Float(17:5-17:9),Comma(17:9-17:10),Newline(1:1-1:1),
CloseSquare(18:1-18:2),EndOfFile(18:2-18:2),
~~~
# PARSE
~~~clojure
(e-malformed @1-2-2-2 (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~