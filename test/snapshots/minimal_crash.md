# META
~~~ini
description=Minimal reproduction of segfault
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

a1 = 1
a2 = 2
a3 = 3
a4 = 4
a5 = 5
a6 = 6
a7 = 7
a8 = 8
a9 = 9
a10 = 10
a11 = 11
a12 = 12
a13 = 13
a14 = 14
a15 = 15
a16 = 16
a17 = 17
a18 = 18
a19 = 19
a20 = 20
a21 = 21
a22 = 22
a23 = 23
a24 = 24
a25 = 25
a26 = 26
a27 = 27
a28 = 28
a29 = 29
a30 = 30
a31 = 31
a32 = 32
a33 = 33
a34 = 34
a35 = 35
a36 = 36
a37 = 37
a38 = 38
a39 = 39
a40 = 40
a41 = 41
a42 = 42
a43 = 43
a44 = 44
a45 = 45
a46 = 46
a47 = 47
a48 = 48
a49 = 49
a50 = 50
a51 = 51
a52 = 52
a53 = 53
a54 = 54
a55 = 55
a56 = 56
a57 = 57
a58 = 58
a59 = 59
a60 = 60
a61 = 61
a62 = 62
a63 = 63
a64 = 64
a65 = 65
a66 = 66
a67 = 67
a68 = 68
a69 = 69
a70 = 70
a71 = 71
a72 = 72
a73 = 73
a74 = 74
a75 = 75
a76 = 76
a77 = 77
a78 = 78
a79 = 79
a80 = 80
a81 = 81
a82 = 82
a83 = 83
a84 = 84
a85 = 85
a86 = 86
a87 = 87
a88 = 88
a89 = 89
a90 = 90
a91 = 91
a92 = 92
a93 = 93
a94 = 94
a95 = 95
a96 = 96
a97 = 97
a98 = 98
a99 = 99
a100 = 100
a101 = 101
a102 = 102
a103 = 103
a104 = 104
a105 = 105
a106 = 106
a107 = 107
a108 = 108
a109 = 109
a110 = 110
a111 = 111
a112 = 112
a113 = 113
a114 = 114
a115 = 115
a116 = 116
a117 = 117
a118 = 118
a119 = 119
a120 = 120
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

a1 = 1
a2 = 2
a3 = 3
a4 = 4
a5 = 5
a6 = 6
a7 = 7
a8 = 8
a9 = 9
a10 = 10
a11 = 11
a12 = 12
a13 = 13
a14 = 14
a15 = 15
a16 = 16
a17 = 17
a18 = 18
a19 = 19
a20 = 20
a21 = 21
a22 = 22
a23 = 23
a24 = 24
a25 = 25
a26 = 26
a27 = 27
a28 = 28
a29 = 29
a30 = 30
a31 = 31
a32 = 32
a33 = 33
a34 = 34
a35 = 35
a36 = 36
a37 = 37
a38 = 38
a39 = 39
a40 = 40
a41 = 41
a42 = 42
a43 = 43
a44 = 44
a45 = 45
a46 = 46
a47 = 47
a48 = 48
a49 = 49
a50 = 50
a51 = 51
a52 = 52
a53 = 53
a54 = 54
a55 = 55
a56 = 56
a57 = 57
a58 = 58
a59 = 59
a60 = 60
a61 = 61
a62 = 62
a63 = 63
a64 = 64
a65 = 65
a66 = 66
a67 = 67
a68 = 68
a69 = 69
a70 = 70
a71 = 71
a72 = 72
a73 = 73
a74 = 74
a75 = 75
a76 = 76
a77 = 77
a78 = 78
a79 = 79
a80 = 80
a81 = 81
a82 = 82
a83 = 83
a84 = 84
a85 = 85
a86 = 86
a87 = 87
a88 = 88
a89 = 89
a90 = 90
a91 = 91
a92 = 92
a93 = 93
a94 = 94
a95 = 95
a96 = 96
a97 = 97
a98 = 98
a99 = 99
a100 = 100
a101 = 101
a102 = 102
a103 = 103
a104 = 104
a105 = 105
a106 = 106
a107 = 107
a108 = 108
a109 = 109
a110 = 110
a111 = 111
a112 = 112
a113 = 113
a114 = 114
a115 = 115
a116 = 116
a117 = 117
a118 = 118
a119 = 119
a120 = 120
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "a1"))
    (Expr.num_literal_i32 1)
  )
  (Stmt.assign
    (pattern (Patt.ident "a2"))
    (Expr.num_literal_i32 2)
  )
  (Stmt.assign
    (pattern (Patt.ident "a3"))
    (Expr.num_literal_i32 3)
  )
  (Stmt.assign
    (pattern (Patt.ident "a4"))
    (Expr.num_literal_i32 4)
  )
  (Stmt.assign
    (pattern (Patt.ident "a5"))
    (Expr.num_literal_i32 5)
  )
  (Stmt.assign
    (pattern (Patt.ident "a6"))
    (Expr.num_literal_i32 6)
  )
  (Stmt.assign
    (pattern (Patt.ident "a7"))
    (Expr.num_literal_i32 7)
  )
  (Stmt.assign
    (pattern (Patt.ident "a8"))
    (Expr.num_literal_i32 8)
  )
  (Stmt.assign
    (pattern (Patt.ident "a9"))
    (Expr.num_literal_i32 9)
  )
  (Stmt.assign
    (pattern (Patt.ident "a10"))
    (Expr.num_literal_i32 10)
  )
  (Stmt.assign
    (pattern (Patt.ident "a11"))
    (Expr.num_literal_i32 11)
  )
  (Stmt.assign
    (pattern (Patt.ident "a12"))
    (Expr.num_literal_i32 12)
  )
  (Stmt.assign
    (pattern (Patt.ident "a13"))
    (Expr.num_literal_i32 13)
  )
  (Stmt.assign
    (pattern (Patt.ident "a14"))
    (Expr.num_literal_i32 14)
  )
  (Stmt.assign
    (pattern (Patt.ident "a15"))
    (Expr.num_literal_i32 15)
  )
  (Stmt.assign
    (pattern (Patt.ident "a16"))
    (Expr.num_literal_i32 16)
  )
  (Stmt.assign
    (pattern (Patt.ident "a17"))
    (Expr.num_literal_i32 17)
  )
  (Stmt.assign
    (pattern (Patt.ident "a18"))
    (Expr.num_literal_i32 18)
  )
  (Stmt.assign
    (pattern (Patt.ident "a19"))
    (Expr.num_literal_i32 19)
  )
  (Stmt.assign
    (pattern (Patt.ident "a20"))
    (Expr.num_literal_i32 20)
  )
  (Stmt.assign
    (pattern (Patt.ident "a21"))
    (Expr.num_literal_i32 21)
  )
  (Stmt.assign
    (pattern (Patt.ident "a22"))
    (Expr.num_literal_i32 22)
  )
  (Stmt.assign
    (pattern (Patt.ident "a23"))
    (Expr.num_literal_i32 23)
  )
  (Stmt.assign
    (pattern (Patt.ident "a24"))
    (Expr.num_literal_i32 24)
  )
  (Stmt.assign
    (pattern (Patt.ident "a25"))
    (Expr.num_literal_i32 25)
  )
  (Stmt.assign
    (pattern (Patt.ident "a26"))
    (Expr.num_literal_i32 26)
  )
  (Stmt.assign
    (pattern (Patt.ident "a27"))
    (Expr.num_literal_i32 27)
  )
  (Stmt.assign
    (pattern (Patt.ident "a28"))
    (Expr.num_literal_i32 28)
  )
  (Stmt.assign
    (pattern (Patt.ident "a29"))
    (Expr.num_literal_i32 29)
  )
  (Stmt.assign
    (pattern (Patt.ident "a30"))
    (Expr.num_literal_i32 30)
  )
  (Stmt.assign
    (pattern (Patt.ident "a31"))
    (Expr.num_literal_i32 31)
  )
  (Stmt.assign
    (pattern (Patt.ident "a32"))
    (Expr.num_literal_i32 32)
  )
  (Stmt.assign
    (pattern (Patt.ident "a33"))
    (Expr.num_literal_i32 33)
  )
  (Stmt.assign
    (pattern (Patt.ident "a34"))
    (Expr.num_literal_i32 34)
  )
  (Stmt.assign
    (pattern (Patt.ident "a35"))
    (Expr.num_literal_i32 35)
  )
  (Stmt.assign
    (pattern (Patt.ident "a36"))
    (Expr.num_literal_i32 36)
  )
  (Stmt.assign
    (pattern (Patt.ident "a37"))
    (Expr.num_literal_i32 37)
  )
  (Stmt.assign
    (pattern (Patt.ident "a38"))
    (Expr.num_literal_i32 38)
  )
  (Stmt.assign
    (pattern (Patt.ident "a39"))
    (Expr.num_literal_i32 39)
  )
  (Stmt.assign
    (pattern (Patt.ident "a40"))
    (Expr.num_literal_i32 40)
  )
  (Stmt.assign
    (pattern (Patt.ident "a41"))
    (Expr.num_literal_i32 41)
  )
  (Stmt.assign
    (pattern (Patt.ident "a42"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.assign
    (pattern (Patt.ident "a43"))
    (Expr.num_literal_i32 43)
  )
  (Stmt.assign
    (pattern (Patt.ident "a44"))
    (Expr.num_literal_i32 44)
  )
  (Stmt.assign
    (pattern (Patt.ident "a45"))
    (Expr.num_literal_i32 45)
  )
  (Stmt.assign
    (pattern (Patt.ident "a46"))
    (Expr.num_literal_i32 46)
  )
  (Stmt.assign
    (pattern (Patt.ident "a47"))
    (Expr.num_literal_i32 47)
  )
  (Stmt.assign
    (pattern (Patt.ident "a48"))
    (Expr.num_literal_i32 48)
  )
  (Stmt.assign
    (pattern (Patt.ident "a49"))
    (Expr.num_literal_i32 49)
  )
  (Stmt.assign
    (pattern (Patt.ident "a50"))
    (Expr.num_literal_i32 50)
  )
  (Stmt.assign
    (pattern (Patt.ident "a51"))
    (Expr.num_literal_i32 51)
  )
  (Stmt.assign
    (pattern (Patt.ident "a52"))
    (Expr.num_literal_i32 52)
  )
  (Stmt.assign
    (pattern (Patt.ident "a53"))
    (Expr.num_literal_i32 53)
  )
  (Stmt.assign
    (pattern (Patt.ident "a54"))
    (Expr.num_literal_i32 54)
  )
  (Stmt.assign
    (pattern (Patt.ident "a55"))
    (Expr.num_literal_i32 55)
  )
  (Stmt.assign
    (pattern (Patt.ident "a56"))
    (Expr.num_literal_i32 56)
  )
  (Stmt.assign
    (pattern (Patt.ident "a57"))
    (Expr.num_literal_i32 57)
  )
  (Stmt.assign
    (pattern (Patt.ident "a58"))
    (Expr.num_literal_i32 58)
  )
  (Stmt.assign
    (pattern (Patt.ident "a59"))
    (Expr.num_literal_i32 59)
  )
  (Stmt.assign
    (pattern (Patt.ident "a60"))
    (Expr.num_literal_i32 60)
  )
  (Stmt.assign
    (pattern (Patt.ident "a61"))
    (Expr.num_literal_i32 61)
  )
  (Stmt.assign
    (pattern (Patt.ident "a62"))
    (Expr.num_literal_i32 62)
  )
  (Stmt.assign
    (pattern (Patt.ident "a63"))
    (Expr.num_literal_i32 63)
  )
  (Stmt.assign
    (pattern (Patt.ident "a64"))
    (Expr.num_literal_i32 64)
  )
  (Stmt.assign
    (pattern (Patt.ident "a65"))
    (Expr.num_literal_i32 65)
  )
  (Stmt.assign
    (pattern (Patt.ident "a66"))
    (Expr.num_literal_i32 66)
  )
  (Stmt.assign
    (pattern (Patt.ident "a67"))
    (Expr.num_literal_i32 67)
  )
  (Stmt.assign
    (pattern (Patt.ident "a68"))
    (Expr.num_literal_i32 68)
  )
  (Stmt.assign
    (pattern (Patt.ident "a69"))
    (Expr.num_literal_i32 69)
  )
  (Stmt.assign
    (pattern (Patt.ident "a70"))
    (Expr.num_literal_i32 70)
  )
  (Stmt.assign
    (pattern (Patt.ident "a71"))
    (Expr.num_literal_i32 71)
  )
  (Stmt.assign
    (pattern (Patt.ident "a72"))
    (Expr.num_literal_i32 72)
  )
  (Stmt.assign
    (pattern (Patt.ident "a73"))
    (Expr.num_literal_i32 73)
  )
  (Stmt.assign
    (pattern (Patt.ident "a74"))
    (Expr.num_literal_i32 74)
  )
  (Stmt.assign
    (pattern (Patt.ident "a75"))
    (Expr.num_literal_i32 75)
  )
  (Stmt.assign
    (pattern (Patt.ident "a76"))
    (Expr.num_literal_i32 76)
  )
  (Stmt.assign
    (pattern (Patt.ident "a77"))
    (Expr.num_literal_i32 77)
  )
  (Stmt.assign
    (pattern (Patt.ident "a78"))
    (Expr.num_literal_i32 78)
  )
  (Stmt.assign
    (pattern (Patt.ident "a79"))
    (Expr.num_literal_i32 79)
  )
  (Stmt.assign
    (pattern (Patt.ident "a80"))
    (Expr.num_literal_i32 80)
  )
  (Stmt.assign
    (pattern (Patt.ident "a81"))
    (Expr.num_literal_i32 81)
  )
  (Stmt.assign
    (pattern (Patt.ident "a82"))
    (Expr.num_literal_i32 82)
  )
  (Stmt.assign
    (pattern (Patt.ident "a83"))
    (Expr.num_literal_i32 83)
  )
  (Stmt.assign
    (pattern (Patt.ident "a84"))
    (Expr.num_literal_i32 84)
  )
  (Stmt.assign
    (pattern (Patt.ident "a85"))
    (Expr.num_literal_i32 85)
  )
  (Stmt.assign
    (pattern (Patt.ident "a86"))
    (Expr.num_literal_i32 86)
  )
  (Stmt.assign
    (pattern (Patt.ident "a87"))
    (Expr.num_literal_i32 87)
  )
  (Stmt.assign
    (pattern (Patt.ident "a88"))
    (Expr.num_literal_i32 88)
  )
  (Stmt.assign
    (pattern (Patt.ident "a89"))
    (Expr.num_literal_i32 89)
  )
  (Stmt.assign
    (pattern (Patt.ident "a90"))
    (Expr.num_literal_i32 90)
  )
  (Stmt.assign
    (pattern (Patt.ident "a91"))
    (Expr.num_literal_i32 91)
  )
  (Stmt.assign
    (pattern (Patt.ident "a92"))
    (Expr.num_literal_i32 92)
  )
  (Stmt.assign
    (pattern (Patt.ident "a93"))
    (Expr.num_literal_i32 93)
  )
  (Stmt.assign
    (pattern (Patt.ident "a94"))
    (Expr.num_literal_i32 94)
  )
  (Stmt.assign
    (pattern (Patt.ident "a95"))
    (Expr.num_literal_i32 95)
  )
  (Stmt.assign
    (pattern (Patt.ident "a96"))
    (Expr.num_literal_i32 96)
  )
  (Stmt.assign
    (pattern (Patt.ident "a97"))
    (Expr.num_literal_i32 97)
  )
  (Stmt.assign
    (pattern (Patt.ident "a98"))
    (Expr.num_literal_i32 98)
  )
  (Stmt.assign
    (pattern (Patt.ident "a99"))
    (Expr.num_literal_i32 99)
  )
  (Stmt.assign
    (pattern (Patt.ident "a100"))
    (Expr.num_literal_i32 100)
  )
  (Stmt.assign
    (pattern (Patt.ident "a101"))
    (Expr.num_literal_i32 101)
  )
  (Stmt.assign
    (pattern (Patt.ident "a102"))
    (Expr.num_literal_i32 102)
  )
  (Stmt.assign
    (pattern (Patt.ident "a103"))
    (Expr.num_literal_i32 103)
  )
  (Stmt.assign
    (pattern (Patt.ident "a104"))
    (Expr.num_literal_i32 104)
  )
  (Stmt.assign
    (pattern (Patt.ident "a105"))
    (Expr.num_literal_i32 105)
  )
  (Stmt.assign
    (pattern (Patt.ident "a106"))
    (Expr.num_literal_i32 106)
  )
  (Stmt.assign
    (pattern (Patt.ident "a107"))
    (Expr.num_literal_i32 107)
  )
  (Stmt.assign
    (pattern (Patt.ident "a108"))
    (Expr.num_literal_i32 108)
  )
  (Stmt.assign
    (pattern (Patt.ident "a109"))
    (Expr.num_literal_i32 109)
  )
  (Stmt.assign
    (pattern (Patt.ident "a110"))
    (Expr.num_literal_i32 110)
  )
  (Stmt.assign
    (pattern (Patt.ident "a111"))
    (Expr.num_literal_i32 111)
  )
  (Stmt.assign
    (pattern (Patt.ident "a112"))
    (Expr.num_literal_i32 112)
  )
  (Stmt.assign
    (pattern (Patt.ident "a113"))
    (Expr.num_literal_i32 113)
  )
  (Stmt.assign
    (pattern (Patt.ident "a114"))
    (Expr.num_literal_i32 114)
  )
  (Stmt.assign
    (pattern (Patt.ident "a115"))
    (Expr.num_literal_i32 115)
  )
  (Stmt.assign
    (pattern (Patt.ident "a116"))
    (Expr.num_literal_i32 116)
  )
  (Stmt.assign
    (pattern (Patt.ident "a117"))
    (Expr.num_literal_i32 117)
  )
  (Stmt.assign
    (pattern (Patt.ident "a118"))
    (Expr.num_literal_i32 118)
  )
  (Stmt.assign
    (pattern (Patt.ident "a119"))
    (Expr.num_literal_i32 119)
  )
  (Stmt.assign
    (pattern (Patt.ident "a120"))
    (Expr.num_literal_i32 120)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Num(_size)")
~~~
# TYPES
~~~roc
~~~
