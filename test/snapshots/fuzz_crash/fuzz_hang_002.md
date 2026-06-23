# META
~~~ini
description=Deeply nested braces - regression test for issue #9109
type=file
~~~
# SOURCE
~~~roc
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
~~~
# EXPECTED
PARSE ERROR - fuzz_hang_002.md:1:1:1:2
PARSE ERROR - fuzz_hang_002.md:1:2:1:3
PARSE ERROR - fuzz_hang_002.md:1:3:1:4
PARSE ERROR - fuzz_hang_002.md:1:4:1:5
PARSE ERROR - fuzz_hang_002.md:1:5:1:6
PARSE ERROR - fuzz_hang_002.md:1:6:1:7
PARSE ERROR - fuzz_hang_002.md:1:7:1:8
PARSE ERROR - fuzz_hang_002.md:1:8:1:9
PARSE ERROR - fuzz_hang_002.md:1:9:1:10
PARSE ERROR - fuzz_hang_002.md:1:10:1:11
PARSE ERROR - fuzz_hang_002.md:1:11:1:12
PARSE ERROR - fuzz_hang_002.md:1:12:1:13
PARSE ERROR - fuzz_hang_002.md:1:13:1:14
PARSE ERROR - fuzz_hang_002.md:1:14:1:15
PARSE ERROR - fuzz_hang_002.md:1:15:1:16
PARSE ERROR - fuzz_hang_002.md:1:16:1:17
PARSE ERROR - fuzz_hang_002.md:1:17:1:18
PARSE ERROR - fuzz_hang_002.md:1:18:1:19
PARSE ERROR - fuzz_hang_002.md:1:19:1:20
PARSE ERROR - fuzz_hang_002.md:1:20:1:21
PARSE ERROR - fuzz_hang_002.md:1:21:1:22
PARSE ERROR - fuzz_hang_002.md:1:22:1:23
PARSE ERROR - fuzz_hang_002.md:1:23:1:24
PARSE ERROR - fuzz_hang_002.md:1:24:1:25
PARSE ERROR - fuzz_hang_002.md:1:25:1:26
PARSE ERROR - fuzz_hang_002.md:1:26:1:27
PARSE ERROR - fuzz_hang_002.md:1:27:1:28
PARSE ERROR - fuzz_hang_002.md:1:28:1:29
PARSE ERROR - fuzz_hang_002.md:1:29:1:30
PARSE ERROR - fuzz_hang_002.md:1:30:1:31
PARSE ERROR - fuzz_hang_002.md:1:31:1:32
PARSE ERROR - fuzz_hang_002.md:1:32:1:33
PARSE ERROR - fuzz_hang_002.md:1:33:1:34
PARSE ERROR - fuzz_hang_002.md:1:34:1:35
PARSE ERROR - fuzz_hang_002.md:1:35:1:36
PARSE ERROR - fuzz_hang_002.md:1:36:1:37
PARSE ERROR - fuzz_hang_002.md:1:37:1:38
PARSE ERROR - fuzz_hang_002.md:1:38:1:39
PARSE ERROR - fuzz_hang_002.md:1:39:1:40
PARSE ERROR - fuzz_hang_002.md:1:40:1:41
PARSE ERROR - fuzz_hang_002.md:1:41:1:42
PARSE ERROR - fuzz_hang_002.md:1:42:1:43
PARSE ERROR - fuzz_hang_002.md:1:43:1:44
PARSE ERROR - fuzz_hang_002.md:1:44:1:45
PARSE ERROR - fuzz_hang_002.md:1:45:1:46
PARSE ERROR - fuzz_hang_002.md:1:46:1:47
PARSE ERROR - fuzz_hang_002.md:1:47:1:48
PARSE ERROR - fuzz_hang_002.md:1:48:1:49
PARSE ERROR - fuzz_hang_002.md:1:49:1:50
PARSE ERROR - fuzz_hang_002.md:1:50:1:51
PARSE ERROR - fuzz_hang_002.md:1:51:1:52
PARSE ERROR - fuzz_hang_002.md:1:52:1:53
PARSE ERROR - fuzz_hang_002.md:1:53:1:54
PARSE ERROR - fuzz_hang_002.md:1:54:1:55
PARSE ERROR - fuzz_hang_002.md:1:55:1:56
PARSE ERROR - fuzz_hang_002.md:1:56:1:57
PARSE ERROR - fuzz_hang_002.md:1:57:1:58
PARSE ERROR - fuzz_hang_002.md:1:58:1:59
PARSE ERROR - fuzz_hang_002.md:1:59:1:60
PARSE ERROR - fuzz_hang_002.md:1:60:1:61
PARSE ERROR - fuzz_hang_002.md:1:61:1:62
PARSE ERROR - fuzz_hang_002.md:1:62:1:63
PARSE ERROR - fuzz_hang_002.md:1:63:1:64
PARSE ERROR - fuzz_hang_002.md:1:64:1:65
PARSE ERROR - fuzz_hang_002.md:1:65:1:66
PARSE ERROR - fuzz_hang_002.md:1:66:1:67
PARSE ERROR - fuzz_hang_002.md:1:67:1:68
PARSE ERROR - fuzz_hang_002.md:1:68:1:69
PARSE ERROR - fuzz_hang_002.md:1:69:1:70
PARSE ERROR - fuzz_hang_002.md:1:70:1:71
PARSE ERROR - fuzz_hang_002.md:1:71:1:72
PARSE ERROR - fuzz_hang_002.md:1:72:1:73
PARSE ERROR - fuzz_hang_002.md:1:73:1:74
PARSE ERROR - fuzz_hang_002.md:1:74:1:75
PARSE ERROR - fuzz_hang_002.md:1:75:1:76
PARSE ERROR - fuzz_hang_002.md:1:76:1:77
PARSE ERROR - fuzz_hang_002.md:1:77:1:78
PARSE ERROR - fuzz_hang_002.md:1:78:1:79
PARSE ERROR - fuzz_hang_002.md:1:79:1:80
PARSE ERROR - fuzz_hang_002.md:1:80:1:81
PARSE ERROR - fuzz_hang_002.md:1:81:1:82
PARSE ERROR - fuzz_hang_002.md:1:82:1:83
PARSE ERROR - fuzz_hang_002.md:1:83:1:84
PARSE ERROR - fuzz_hang_002.md:1:84:1:85
PARSE ERROR - fuzz_hang_002.md:1:85:1:86
PARSE ERROR - fuzz_hang_002.md:1:86:1:87
PARSE ERROR - fuzz_hang_002.md:1:87:1:88
PARSE ERROR - fuzz_hang_002.md:1:88:1:89
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{… │
 │  ‾                                                                         │
 └────────────────────────────────────────────────────── fuzz_hang_002.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{… │
 │   ‾                                                                        │
 └────────────────────────────────────────────────────── fuzz_hang_002.md:1:2 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{… │
 │    ‾                                                                       │
 └────────────────────────────────────────────────────── fuzz_hang_002.md:1:3 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{… │
 │     ‾                                                                      │
 └────────────────────────────────────────────────────── fuzz_hang_002.md:1:4 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{… │
 │      ‾                                                                     │
 └────────────────────────────────────────────────────── fuzz_hang_002.md:1:5 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{… │
 │       ‾                                                                    │
 └────────────────────────────────────────────────────── fuzz_hang_002.md:1:6 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{… │
 │        ‾                                                                   │
 └────────────────────────────────────────────────────── fuzz_hang_002.md:1:7 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{… │
 │         ‾                                                                  │
 └────────────────────────────────────────────────────── fuzz_hang_002.md:1:8 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{… │
 │          ‾                                                                 │
 └────────────────────────────────────────────────────── fuzz_hang_002.md:1:9 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:10 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:11 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:12 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:13 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:14 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:15 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:16 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:17 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:18 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:19 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:20 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:21 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:22 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{…│
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:24 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:25 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{  │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:26 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{   │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:27 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{    │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{     │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{      │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{       │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{        │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:32 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{         │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:33 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{          │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:34 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{           │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:35 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{            │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:36 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{             │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:37 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{              │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:38 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{               │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:39 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:40 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                 │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:41 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                  │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:42 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                   │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:43 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                    │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:44 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                     │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:45 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                      │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:46 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                       │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:47 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                        │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:48 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                         │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:49 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                          │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:50 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                           │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:51 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                            │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:52 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                             │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:53 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                              │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:54 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                               │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:55 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:56 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                 │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:57 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                  │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:58 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                   │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:59 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                    │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:60 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                     │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:61 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                      │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:62 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                       │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:63 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                        │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:64 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                         │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:65 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                          │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:66 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                           │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:67 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                            │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:68 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{{                                             │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:69 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{{                                              │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:70 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{{                                               │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:71 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{{                                                │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:72 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{{                                                 │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:73 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{{                                                  │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:74 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{{                                                   │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:75 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{{                                                    │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:76 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{{                                                     │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:77 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{{                                                      │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:78 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{{                                                       │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:79 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{{                                                        │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:80 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{{                                                         │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:81 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{{                                                          │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:82 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{{                                                           │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:83 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{{                                                            │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:84 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{{                                                             │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:85 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{{                                                              │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:86 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{{                                                               │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:87 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  …{{{{{{{{{                                                                │
 │           ‾                                                                │
 └───────────────────────────────────────────────────── fuzz_hang_002.md:1:88 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,OpenCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
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
