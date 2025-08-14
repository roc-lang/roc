# META
~~~ini
description=Various numeric types and literals
type=repl
~~~
# SOURCE
~~~roc
» 1u8
» 2i8
» 3u16
» 4i16
» 5u32
» 6i32
» 7u64
» 8i64
» 9u128
» 10i128
» 11.0f32
» 12.0f64
» 13.0dec
» 0xE # 14
» 0xf # 15
» 0x10 # 16
» 0b10001 # 17
» 0b1_0010 # 18
» 19
» 20.0
» 21_000
» 22_000_000
» 0.0
» -0.1
» 2e4 # 20_000
» 3E2 # 300
» -0.2e-2 # -0.002
~~~
# OUTPUT
1
---
2
---
3
---
4
---
5
---
6
---
7
---
8
---
9
---
10
---
1.1e1
---
1.2e1
---
1.3e1
---
14
---
15
---
16
---
17
---
18
---
19
---
2e1
---
21000
---
22000000
---
0e0
---
-1e-1
---
2e4
---
3e2
---
-2e-3
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-int @1.1-1.1 (value "1"))
(e-int @1.2-1.5 (value "1"))
---
(e-int @1.1-1.1 (value "2"))
(e-int @1.2-1.5 (value "2"))
---
(e-int @1.1-1.1 (value "3"))
(e-int @1.2-1.6 (value "3"))
---
(e-int @1.1-1.1 (value "4"))
(e-int @1.2-1.6 (value "4"))
---
(e-int @1.1-1.1 (value "5"))
(e-int @1.2-1.6 (value "5"))
---
(e-int @1.1-1.1 (value "6"))
(e-int @1.2-1.6 (value "6"))
---
(e-int @1.1-1.1 (value "7"))
(e-int @1.2-1.6 (value "7"))
---
(e-int @1.1-1.1 (value "8"))
(e-int @1.2-1.6 (value "8"))
---
(e-int @1.1-1.1 (value "9"))
(e-int @1.2-1.7 (value "9"))
---
(e-int @1.1-1.1 (value "10"))
(e-int @1.2-1.8 (value "10"))
---
(e-frac-f32 @1.1-1.1 (value "11"))
(e-frac-f32 @1.2-1.9 (value "11"))
---
(e-frac-f64 @1.1-1.1 (value "12"))
(e-frac-f64 @1.2-1.9 (value "12"))
---
(e-frac-dec @1.1-1.1 (value "13"))
(e-frac-dec @1.2-1.9 (value "13"))
---
(e-int @1.1-1.1 (value "14"))
(e-int @1.2-1.5 (value "14"))
---
(e-int @1.1-1.1 (value "15"))
(e-int @1.2-1.5 (value "15"))
---
(e-int @1.1-1.1 (value "16"))
(e-int @1.2-1.6 (value "16"))
---
(e-int @1.1-1.1 (value "17"))
(e-int @1.2-1.9 (value "17"))
---
(e-int @1.1-1.1 (value "18"))
(e-int @1.2-1.10 (value "18"))
---
(e-int @1.1-1.1 (value "19"))
(e-int @1.2-1.4 (value "19"))
---
(e-dec-small @1.1-1.1 (numerator "200") (denominator-power-of-ten "1") (value "20"))
(e-dec-small @1.2-1.6 (numerator "200") (denominator-power-of-ten "1") (value "20"))
---
(e-int @1.1-1.1 (value "21000"))
(e-int @1.2-1.8 (value "21000"))
---
(e-int @1.1-1.1 (value "22000000"))
(e-int @1.2-1.12 (value "22000000"))
---
(e-dec-small @1.1-1.1 (numerator "0") (denominator-power-of-ten "1") (value "0.0"))
(e-dec-small @1.2-1.5 (numerator "0") (denominator-power-of-ten "1") (value "0.0"))
---
(e-dec-small @1.1-1.1 (numerator "-1") (denominator-power-of-ten "1") (value "-0.1"))
(e-dec-small @1.2-1.6 (numerator "-1") (denominator-power-of-ten "1") (value "-0.1"))
---
(e-dec-small @1.1-1.1 (numerator "20000") (denominator-power-of-ten "0") (value "20000"))
(e-dec-small @1.2-1.5 (numerator "20000") (denominator-power-of-ten "0") (value "20000"))
---
(e-dec-small @1.1-1.1 (numerator "300") (denominator-power-of-ten "0") (value "300"))
(e-dec-small @1.2-1.5 (numerator "300") (denominator-power-of-ten "0") (value "300"))
---
(e-frac-dec @1.1-1.1 (value "-0.002"))
(e-frac-dec @1.2-1.9 (value "-0.002"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "U8"))
(expr @1.2-1.5 (type "U8"))
---
(expr @1.1-1.1 (type "I8"))
(expr @1.2-1.5 (type "I8"))
---
(expr @1.1-1.1 (type "U16"))
(expr @1.2-1.6 (type "U16"))
---
(expr @1.1-1.1 (type "I16"))
(expr @1.2-1.6 (type "I16"))
---
(expr @1.1-1.1 (type "U32"))
(expr @1.2-1.6 (type "U32"))
---
(expr @1.1-1.1 (type "I32"))
(expr @1.2-1.6 (type "I32"))
---
(expr @1.1-1.1 (type "U64"))
(expr @1.2-1.6 (type "U64"))
---
(expr @1.1-1.1 (type "I64"))
(expr @1.2-1.6 (type "I64"))
---
(expr @1.1-1.1 (type "U128"))
(expr @1.2-1.7 (type "U128"))
---
(expr @1.1-1.1 (type "I128"))
(expr @1.2-1.8 (type "I128"))
---
(expr @1.1-1.1 (type "F32"))
(expr @1.2-1.9 (type "F32"))
---
(expr @1.1-1.1 (type "F64"))
(expr @1.2-1.9 (type "F64"))
---
(expr @1.1-1.1 (type "Dec"))
(expr @1.2-1.9 (type "Dec"))
---
(expr @1.1-1.1 (type "Int(_size)"))
(expr @1.2-1.5 (type "Int(_size)"))
---
(expr @1.1-1.1 (type "Int(_size)"))
(expr @1.2-1.5 (type "Int(_size)"))
---
(expr @1.1-1.1 (type "Int(_size)"))
(expr @1.2-1.6 (type "Int(_size)"))
---
(expr @1.1-1.1 (type "Int(_size)"))
(expr @1.2-1.9 (type "Int(_size)"))
---
(expr @1.1-1.1 (type "Int(_size)"))
(expr @1.2-1.10 (type "Int(_size)"))
---
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.4 (type "Num(_size)"))
---
(expr @1.1-1.1 (type "Frac(_size)"))
(expr @1.2-1.6 (type "Frac(_size)"))
---
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.8 (type "Num(_size)"))
---
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.12 (type "Num(_size)"))
---
(expr @1.1-1.1 (type "Frac(_size)"))
(expr @1.2-1.5 (type "Frac(_size)"))
---
(expr @1.1-1.1 (type "Frac(_size)"))
(expr @1.2-1.6 (type "Frac(_size)"))
---
(expr @1.1-1.1 (type "Frac(_size)"))
(expr @1.2-1.5 (type "Frac(_size)"))
---
(expr @1.1-1.1 (type "Frac(_size)"))
(expr @1.2-1.5 (type "Frac(_size)"))
---
(expr @1.1-1.1 (type "Frac(_size)"))
(expr @1.2-1.9 (type "Frac(_size)"))
~~~
