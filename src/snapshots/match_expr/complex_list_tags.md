# META
~~~ini
description=Match expression with complex list patterns containing tagged values
type=expr
~~~
# SOURCE
~~~roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
    _ => "other event pattern"
}
~~~
# EXPECTED
PARSE ERROR - complex_list_tags.md:3:22:3:23
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:3:53:3:54
PARSE ERROR - complex_list_tags.md:3:54:3:54
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:3:54:3:56
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:3:56:3:58
PARSE ERROR - complex_list_tags.md:3:58:3:58
PARSE ERROR - complex_list_tags.md:3:69:3:69
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:3:69:3:70
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:3:70:3:71
PARSE ERROR - complex_list_tags.md:3:71:3:71
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:3:71:3:72
PARSE ERROR - complex_list_tags.md:4:36:4:37
PARSE ERROR - complex_list_tags.md:4:79:4:79
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:4:83:4:84
PARSE ERROR - complex_list_tags.md:4:84:4:84
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:4:84:4:85
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:4:85:4:97
PARSE ERROR - complex_list_tags.md:4:97:4:97
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:4:97:4:98
PARSE ERROR - complex_list_tags.md:5:53:5:54
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:5:74:5:75
PARSE ERROR - complex_list_tags.md:5:75:5:75
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:5:75:5:76
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:5:76:5:78
PARSE ERROR - complex_list_tags.md:5:78:5:78
PARSE ERROR - complex_list_tags.md:5:90:5:90
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:5:90:5:91
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:5:91:5:97
PARSE ERROR - complex_list_tags.md:5:97:5:97
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:5:97:5:99
PARSE ERROR - complex_list_tags.md:5:109:5:109
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:5:112:5:113
PARSE ERROR - complex_list_tags.md:5:113:5:113
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:5:113:5:114
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:5:114:5:116
PARSE ERROR - complex_list_tags.md:5:116:5:116
PARSE ERROR - complex_list_tags.md:5:129:5:129
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:5:129:5:130
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:5:130:5:130
PARSE ERROR - complex_list_tags.md:5:130:5:130
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:5:130:5:131
PARSE ERROR - complex_list_tags.md:6:55:6:56
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:6:81:6:82
PARSE ERROR - complex_list_tags.md:6:82:6:82
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:6:82:6:97
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:6:97:6:99
PARSE ERROR - complex_list_tags.md:6:99:6:99
PARSE ERROR - complex_list_tags.md:6:110:6:110
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:6:110:6:111
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:6:111:6:112
PARSE ERROR - complex_list_tags.md:6:112:6:112
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:6:112:6:114
PARSE ERROR - complex_list_tags.md:6:124:6:124
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:6:125:6:126
PARSE ERROR - complex_list_tags.md:6:126:6:126
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:6:126:6:126
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:6:126:6:127
PARSE ERROR - complex_list_tags.md:7:5:7:5
UNEXPECTED TOKEN IN EXPRESSION - complex_list_tags.md:7:5:7:6
UNEXPECTED TOKEN IN PATTERN - complex_list_tags.md:7:7:7:9
PARSE ERROR - complex_list_tags.md:7:10:7:10
UNDEFINED VARIABLE - complex_list_tags.md:1:7:1:13
UNKNOWN OPERATOR - complex_list_tags.md:3:22:3:53
UNUSED VARIABLE - complex_list_tags.md:3:15:3:16
UNUSED VARIABLE - complex_list_tags.md:3:12:3:13
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:3:54:3:56
INVALID PATTERN - :0:0:0:0
UNDEFINED VARIABLE - complex_list_tags.md:3:58:3:67
UNKNOWN OPERATOR - complex_list_tags.md:3:69:3:70
UNUSED VARIABLE - complex_list_tags.md:3:68:3:69
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:3:71:3:72
UNKNOWN OPERATOR - complex_list_tags.md:4:36:4:70
UNUSED VARIABLE - complex_list_tags.md:4:15:4:18
UNUSED VARIABLE - complex_list_tags.md:1:1:1:1
UNDEFINED VARIABLE - complex_list_tags.md:4:79:4:83
UNUSED VARIABLE - complex_list_tags.md:4:70:4:78
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:4:84:4:85
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:4:97:4:98
UNKNOWN OPERATOR - complex_list_tags.md:5:53:5:74
UNUSED VARIABLE - complex_list_tags.md:1:1:1:1
UNUSED VARIABLE - complex_list_tags.md:5:30:5:33
UNUSED VARIABLE - complex_list_tags.md:5:25:5:28
UNUSED VARIABLE - complex_list_tags.md:5:11:5:13
UNUSED VARIABLE - complex_list_tags.md:5:15:5:17
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:5:75:5:76
INVALID PATTERN - :0:0:0:0
UNDEFINED VARIABLE - complex_list_tags.md:5:78:5:87
UNKNOWN OPERATOR - complex_list_tags.md:5:90:5:91
UNUSED VARIABLE - complex_list_tags.md:5:88:5:90
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:5:97:5:99
UNDEFINED VARIABLE - complex_list_tags.md:5:109:5:112
UNUSED VARIABLE - complex_list_tags.md:5:99:5:108
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:5:113:5:114
INVALID PATTERN - :0:0:0:0
UNDEFINED VARIABLE - complex_list_tags.md:5:116:5:125
UNKNOWN OPERATOR - complex_list_tags.md:5:129:5:130
UNUSED VARIABLE - complex_list_tags.md:5:126:5:129
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:5:130:5:131
UNKNOWN OPERATOR - complex_list_tags.md:6:55:6:81
UNUSED VARIABLE - complex_list_tags.md:1:1:1:1
UNUSED VARIABLE - complex_list_tags.md:6:13:6:19
UNUSED VARIABLE - complex_list_tags.md:6:31:6:32
UNUSED VARIABLE - complex_list_tags.md:6:28:6:29
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:6:82:6:97
INVALID PATTERN - :0:0:0:0
UNDEFINED VARIABLE - complex_list_tags.md:6:99:6:108
UNKNOWN OPERATOR - complex_list_tags.md:6:110:6:111
UNUSED VARIABLE - complex_list_tags.md:6:109:6:110
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:6:112:6:114
UNDEFINED VARIABLE - complex_list_tags.md:6:124:6:125
UNUSED VARIABLE - complex_list_tags.md:6:114:6:123
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:6:126:6:126
INVALID PATTERN - :0:0:0:0
UNKNOWN OPERATOR - complex_list_tags.md:7:5:7:6
INVALID PATTERN - :0:0:0:0
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:3:22:3:23:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                     ^


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:3:53:3:54:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                    ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:3:54:3:54:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                     


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:3:54:3:56:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                     ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **${** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:3:56:3:58:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                       ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:3:58:3:58:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                         


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:3:69:3:69:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                    


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:3:69:3:70:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                    ^


**UNEXPECTED TOKEN IN PATTERN**
The token **)** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:3:70:3:71:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                     ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:3:71:3:71:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                      


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:3:71:3:72:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                      ^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:4:36:4:37:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                   ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:4:79:4:79:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                              


**UNEXPECTED TOKEN IN PATTERN**
The token **)** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:4:83:4:84:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                  ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:4:84:4:84:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                   


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:4:84:4:85:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                   ^


**UNEXPECTED TOKEN IN PATTERN**
The token ** more events** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:4:85:4:97:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                    ^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:4:97:4:97:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                                


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:4:97:4:98:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                                ^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:53:5:54:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                    ^


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:74:5:75:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                         ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:75:5:75:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                          


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:75:5:76:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                          ^


**UNEXPECTED TOKEN IN PATTERN**
The token **${** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:76:5:78:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                           ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:78:5:78:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                             


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:90:5:90:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                         


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:90:5:91:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                         ^


**UNEXPECTED TOKEN IN PATTERN**
The token ** then ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:91:5:97:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                          ^^^^^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:97:5:97:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:97:5:99:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:109:5:109:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                            


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:112:5:113:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                               ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:113:5:113:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:113:5:114:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                ^


**UNEXPECTED TOKEN IN PATTERN**
The token **${** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:114:5:116:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                 ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:116:5:116:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                   


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:129:5:129:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                                


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:129:5:130:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                                ^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:130:5:130:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                                 


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:130:5:130:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                                 


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:130:5:131:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                                 ^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:6:55:6:56:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                      ^


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:81:6:82:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:6:82:6:82:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                 


**UNEXPECTED TOKEN IN EXPRESSION**
The token ** then click at ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:82:6:97:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                 ^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **${** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:97:6:99:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:6:99:6:99:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                  


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:6:110:6:110:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                             


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:110:6:111:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                             ^


**UNEXPECTED TOKEN IN PATTERN**
The token **,** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:111:6:112:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                              ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:6:112:6:112:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                               


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:112:6:114:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                               ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:6:124:6:124:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                           


**UNEXPECTED TOKEN IN PATTERN**
The token **}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:125:6:126:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                            ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:6:126:6:126:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                             


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:126:6:126:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                             


**UNEXPECTED TOKEN IN PATTERN**
The token **"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:126:6:127:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                             ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:7:5:7:5:**
```roc
    _ => "other event pattern"
```
    


**UNEXPECTED TOKEN IN EXPRESSION**
The token **_** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:7:5:7:6:**
```roc
    _ => "other event pattern"
```
    ^


**UNEXPECTED TOKEN IN PATTERN**
The token **=>** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:7:7:7:9:**
```roc
    _ => "other event pattern"
```
      ^^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:7:10:7:10:**
```roc
    _ => "other event pattern"
```
         


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'events' is not defined:
**complex_list_tags.md:1:7:1:13:**
```roc
match events {
```
      ^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:3:22:3:53:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:3:15:3:16:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
              ^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:3:12:3:13:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
           ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:3:54:3:56:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                     ^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'toStr' is not defined:
**complex_list_tags.md:3:58:3:67:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                         ^^^^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:3:69:3:70:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                    ^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:3:68:3:69:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                   ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:3:71:3:72:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                      ^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:4:36:4:70:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `key` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_key` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:4:15:4:18:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
              ^^^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:1:1:1:1:**
```roc
match events {
```



**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'rest' is not defined:
**complex_list_tags.md:4:79:4:83:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                              ^^^^


**UNUSED VARIABLE**
Variable `len` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_len` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:4:70:4:78:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                     ^^^^^^^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:4:84:4:85:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                   ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:4:97:4:98:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                                ^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:5:53:5:74:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                    ^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `others` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_others` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:1:1:1:1:**
```roc
match events {
```



**UNUSED VARIABLE**
Variable `dy2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_dy2` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:5:30:5:33:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                             ^^^


**UNUSED VARIABLE**
Variable `dx2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_dx2` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:5:25:5:28:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                        ^^^


**UNUSED VARIABLE**
Variable `dx` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_dx` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:5:11:5:13:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
          ^^


**UNUSED VARIABLE**
Variable `dy` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_dy` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:5:15:5:17:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
              ^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:5:75:5:76:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                          ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'toStr' is not defined:
**complex_list_tags.md:5:78:5:87:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                             ^^^^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:5:90:5:91:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                         ^


**UNUSED VARIABLE**
Variable `dy` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_dy` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:5:88:5:90:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                       ^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:5:97:5:99:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                ^^


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'dx2' is not defined:
**complex_list_tags.md:5:109:5:112:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                            ^^^


**UNUSED VARIABLE**
Variable `toStr` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_toStr` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:5:99:5:108:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                  ^^^^^^^^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:5:113:5:114:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'toStr' is not defined:
**complex_list_tags.md:5:116:5:125:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                   ^^^^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:5:129:5:130:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                                ^


**UNUSED VARIABLE**
Variable `dy2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_dy2` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:5:126:5:129:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                             ^^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:5:130:5:131:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                                 ^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:6:55:6:81:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `remaining` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_remaining` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:1:1:1:1:**
```roc
match events {
```



**UNUSED VARIABLE**
Variable `amount` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_amount` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:6:13:6:19:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
            ^^^^^^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:6:31:6:32:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                              ^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:6:28:6:29:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                           ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:6:82:6:97:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                 ^^^^^^^^^^^^^^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'toStr' is not defined:
**complex_list_tags.md:6:99:6:108:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                  ^^^^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:6:110:6:111:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                             ^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:6:109:6:110:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                            ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:6:112:6:114:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                               ^^


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'y' is not defined:
**complex_list_tags.md:6:124:6:125:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                           ^


**UNUSED VARIABLE**
Variable `toStr` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_toStr` to suppress this warning.
The unused variable is declared here:
**complex_list_tags.md:6:114:6:123:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                 ^^^^^^^^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:6:126:6:126:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                             


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**complex_list_tags.md:7:5:7:6:**
```roc
    _ => "other event pattern"
```
    ^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),
OpenSquare(2:5-2:6),CloseSquare(2:6-2:7),OpFatArrow(2:8-2:10),StringStart(2:11-2:12),StringPart(2:12-2:21),StringEnd(2:21-2:22),
OpenSquare(3:5-3:6),UpperIdent(3:6-3:11),NoSpaceOpenRound(3:11-3:12),LowerIdent(3:12-3:13),Comma(3:13-3:14),LowerIdent(3:15-3:16),CloseRound(3:16-3:17),CloseSquare(3:17-3:18),OpFatArrow(3:19-3:21),StringStart(3:22-3:23),StringPart(3:23-3:40),OpenStringInterpolation(3:40-3:42),UpperIdent(3:42-3:45),NoSpaceDotLowerIdent(3:45-3:51),LowerIdent(3:52-3:53),CloseStringInterpolation(3:53-3:54),StringPart(3:54-3:56),OpenStringInterpolation(3:56-3:58),UpperIdent(3:58-3:61),NoSpaceDotLowerIdent(3:61-3:67),LowerIdent(3:68-3:69),CloseStringInterpolation(3:69-3:70),StringPart(3:70-3:71),StringEnd(3:71-3:72),
OpenSquare(4:5-4:6),UpperIdent(4:6-4:14),NoSpaceOpenRound(4:14-4:15),LowerIdent(4:15-4:18),CloseRound(4:18-4:19),Comma(4:19-4:20),DoubleDot(4:21-4:23),KwAs(4:24-4:26),LowerIdent(4:27-4:31),CloseSquare(4:31-4:32),OpFatArrow(4:33-4:35),StringStart(4:36-4:37),StringPart(4:37-4:41),OpenStringInterpolation(4:41-4:43),LowerIdent(4:43-4:46),CloseStringInterpolation(4:46-4:47),StringPart(4:47-4:57),OpenStringInterpolation(4:57-4:59),UpperIdent(4:59-4:62),NoSpaceDotLowerIdent(4:62-4:68),OpenRound(4:69-4:70),UpperIdent(4:70-4:74),NoSpaceDotLowerIdent(4:74-4:78),LowerIdent(4:79-4:83),CloseRound(4:83-4:84),CloseStringInterpolation(4:84-4:85),StringPart(4:85-4:97),StringEnd(4:97-4:98),
OpenSquare(5:5-5:6),UpperIdent(5:6-5:10),NoSpaceOpenRound(5:10-5:11),LowerIdent(5:11-5:13),Comma(5:13-5:14),LowerIdent(5:15-5:17),CloseRound(5:17-5:18),Comma(5:18-5:19),UpperIdent(5:20-5:24),NoSpaceOpenRound(5:24-5:25),LowerIdent(5:25-5:28),Comma(5:28-5:29),LowerIdent(5:30-5:33),CloseRound(5:33-5:34),Comma(5:34-5:35),DoubleDot(5:36-5:38),KwAs(5:39-5:41),LowerIdent(5:42-5:48),CloseSquare(5:48-5:49),OpFatArrow(5:50-5:52),StringStart(5:53-5:54),StringPart(5:54-5:60),OpenStringInterpolation(5:60-5:62),UpperIdent(5:62-5:65),NoSpaceDotLowerIdent(5:65-5:71),LowerIdent(5:72-5:74),CloseStringInterpolation(5:74-5:75),StringPart(5:75-5:76),OpenStringInterpolation(5:76-5:78),UpperIdent(5:78-5:81),NoSpaceDotLowerIdent(5:81-5:87),LowerIdent(5:88-5:90),CloseStringInterpolation(5:90-5:91),StringPart(5:91-5:97),OpenStringInterpolation(5:97-5:99),UpperIdent(5:99-5:102),NoSpaceDotLowerIdent(5:102-5:108),LowerIdent(5:109-5:112),CloseStringInterpolation(5:112-5:113),StringPart(5:113-5:114),OpenStringInterpolation(5:114-5:116),UpperIdent(5:116-5:119),NoSpaceDotLowerIdent(5:119-5:125),LowerIdent(5:126-5:129),CloseStringInterpolation(5:129-5:130),StringPart(5:130-5:130),StringEnd(5:130-5:131),
OpenSquare(6:5-6:6),UpperIdent(6:6-6:12),NoSpaceOpenRound(6:12-6:13),LowerIdent(6:13-6:19),CloseRound(6:19-6:20),Comma(6:20-6:21),UpperIdent(6:22-6:27),NoSpaceOpenRound(6:27-6:28),LowerIdent(6:28-6:29),Comma(6:29-6:30),LowerIdent(6:31-6:32),CloseRound(6:32-6:33),Comma(6:33-6:34),DoubleDot(6:35-6:37),KwAs(6:38-6:40),LowerIdent(6:41-6:50),CloseSquare(6:50-6:51),OpFatArrow(6:52-6:54),StringStart(6:55-6:56),StringPart(6:56-6:63),OpenStringInterpolation(6:63-6:65),UpperIdent(6:65-6:68),NoSpaceDotLowerIdent(6:68-6:74),LowerIdent(6:75-6:81),CloseStringInterpolation(6:81-6:82),StringPart(6:82-6:97),OpenStringInterpolation(6:97-6:99),UpperIdent(6:99-6:102),NoSpaceDotLowerIdent(6:102-6:108),LowerIdent(6:109-6:110),CloseStringInterpolation(6:110-6:111),StringPart(6:111-6:112),OpenStringInterpolation(6:112-6:114),UpperIdent(6:114-6:117),NoSpaceDotLowerIdent(6:117-6:123),LowerIdent(6:124-6:125),CloseStringInterpolation(6:125-6:126),StringPart(6:126-6:126),StringEnd(6:126-6:127),
Underscore(7:5-7:6),OpFatArrow(7:7-7:9),StringStart(7:10-7:11),StringPart(7:11-7:30),StringEnd(7:30-7:31),
CloseCurly(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (raw "events"))
	(branches
		(branch @2.5-2.22
			(p-list @2.5-2.7)
			(e-string @2.11-2.22
				(e-string-part @2.12-2.21 (raw "no events"))))
		(branch @3.5-3.53
			(p-list @3.5-3.18
				(p-tag @3.6-3.17 (raw "Click")
					(p-ident @3.12-3.13 (raw "x"))
					(p-ident @3.15-3.16 (raw "y"))))
			(e-malformed @3.22-3.53 (reason "string_expected_close_interpolation")))
		(branch @3.53-3.56
			(p-malformed @3.53-3.54 (tag "pattern_unexpected_token"))
			(e-malformed @3.54-3.56 (reason "expr_unexpected_token")))
		(branch @3.56-3.67
			(p-malformed @3.56-3.58 (tag "pattern_unexpected_token"))
			(e-ident @3.58-3.67 (raw "Num.toStr")))
		(branch @3.68-3.70
			(p-ident @3.68-3.69 (raw "y"))
			(e-malformed @3.69-3.70 (reason "expr_unexpected_token")))
		(branch @3.70-3.72
			(p-malformed @3.70-3.71 (tag "pattern_unexpected_token"))
			(e-malformed @3.71-3.72 (reason "expr_unexpected_token")))
		(branch @4.5-4.70
			(p-list @4.5-4.32
				(p-tag @4.6-4.19 (raw "KeyPress")
					(p-ident @4.15-4.18 (raw "key")))
				(p-list-rest @4.21-4.31 (name "rest")))
			(e-malformed @4.36-4.70 (reason "string_expected_close_interpolation")))
		(branch @4.70-4.83
			(p-ident @4.70-4.78 (raw ".len"))
			(e-ident @4.79-4.83 (raw "rest")))
		(branch @4.83-4.85
			(p-malformed @4.83-4.84 (tag "pattern_unexpected_token"))
			(e-malformed @4.84-4.85 (reason "expr_unexpected_token")))
		(branch @4.85-4.98
			(p-malformed @4.85-4.97 (tag "pattern_unexpected_token"))
			(e-malformed @4.97-4.98 (reason "expr_unexpected_token")))
		(branch @5.5-5.74
			(p-list @5.5-5.49
				(p-tag @5.6-5.18 (raw "Move")
					(p-ident @5.11-5.13 (raw "dx"))
					(p-ident @5.15-5.17 (raw "dy")))
				(p-tag @5.20-5.34 (raw "Move")
					(p-ident @5.25-5.28 (raw "dx2"))
					(p-ident @5.30-5.33 (raw "dy2")))
				(p-list-rest @5.36-5.48 (name "others")))
			(e-malformed @5.53-5.74 (reason "string_expected_close_interpolation")))
		(branch @5.74-5.76
			(p-malformed @5.74-5.75 (tag "pattern_unexpected_token"))
			(e-malformed @5.75-5.76 (reason "expr_unexpected_token")))
		(branch @5.76-5.87
			(p-malformed @5.76-5.78 (tag "pattern_unexpected_token"))
			(e-ident @5.78-5.87 (raw "Num.toStr")))
		(branch @5.88-5.91
			(p-ident @5.88-5.90 (raw "dy"))
			(e-malformed @5.90-5.91 (reason "expr_unexpected_token")))
		(branch @5.91-5.99
			(p-malformed @5.91-5.97 (tag "pattern_unexpected_token"))
			(e-malformed @5.97-5.99 (reason "expr_unexpected_token")))
		(branch @5.99-5.112
			(p-ident @5.99-5.108 (raw ".toStr"))
			(e-ident @5.109-5.112 (raw "dx2")))
		(branch @5.112-5.114
			(p-malformed @5.112-5.113 (tag "pattern_unexpected_token"))
			(e-malformed @5.113-5.114 (reason "expr_unexpected_token")))
		(branch @5.114-5.125
			(p-malformed @5.114-5.116 (tag "pattern_unexpected_token"))
			(e-ident @5.116-5.125 (raw "Num.toStr")))
		(branch @5.126-5.130
			(p-ident @5.126-5.129 (raw "dy2"))
			(e-malformed @5.129-5.130 (reason "expr_unexpected_token")))
		(branch @5.130-5.131
			(p-malformed @5.130-5.130 (tag "pattern_unexpected_token"))
			(e-malformed @5.130-5.131 (reason "expr_unexpected_token")))
		(branch @6.5-6.81
			(p-list @6.5-6.51
				(p-tag @6.6-6.20 (raw "Scroll")
					(p-ident @6.13-6.19 (raw "amount")))
				(p-tag @6.22-6.33 (raw "Click")
					(p-ident @6.28-6.29 (raw "x"))
					(p-ident @6.31-6.32 (raw "y")))
				(p-list-rest @6.35-6.50 (name "remaining")))
			(e-malformed @6.55-6.81 (reason "string_expected_close_interpolation")))
		(branch @6.81-6.97
			(p-malformed @6.81-6.82 (tag "pattern_unexpected_token"))
			(e-malformed @6.82-6.97 (reason "expr_unexpected_token")))
		(branch @6.97-6.108
			(p-malformed @6.97-6.99 (tag "pattern_unexpected_token"))
			(e-ident @6.99-6.108 (raw "Num.toStr")))
		(branch @6.109-6.111
			(p-ident @6.109-6.110 (raw "x"))
			(e-malformed @6.110-6.111 (reason "expr_unexpected_token")))
		(branch @6.111-6.114
			(p-malformed @6.111-6.112 (tag "pattern_unexpected_token"))
			(e-malformed @6.112-6.114 (reason "expr_unexpected_token")))
		(branch @6.114-6.125
			(p-ident @6.114-6.123 (raw ".toStr"))
			(e-ident @6.124-6.125 (raw "y")))
		(branch @6.125-6.126
			(p-malformed @6.125-6.126 (tag "pattern_unexpected_token"))
			(e-malformed @6.126-6.126 (reason "expr_unexpected_token")))
		(branch @6.126-7.6
			(p-malformed @6.126-6.127 (tag "pattern_unexpected_token"))
			(e-malformed @7.5-7.6 (reason "expr_unexpected_token")))
		(branch @7.7-7.31
			(p-malformed @7.7-7.9 (tag "pattern_unexpected_token"))
			(e-string @7.10-7.31
				(e-string-part @7.11-7.30 (raw "other event pattern"))))))
~~~
# FORMATTED
~~~roc
match events {
	[] => "no events"
	[Click(x, y)] => 
	 => 
	 => Num.toStr
	y => 
	 => 
	[KeyPress(key), .. as rest] => 
	len => rest
	 => 
	 => 
	[Move(dx, dy), Move(dx2, dy2), .. as others] => 
	 => 
	 => Num.toStr
	dy => 
	 => 
	toStr => dx2
	 => 
	 => Num.toStr
	dy2 => 
	 => 
	[Scroll(amount), Click(x, y), .. as remaining] => 
	 => 
	 => Num.toStr
	x => 
	 => 
	toStr => y
	 => 
	 =>
		
	 => "other event pattern"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-8.2
	(match @1.1-8.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @2.5-2.7
							(patterns))))
				(value
					(e-string @2.11-2.22
						(e-literal @2.12-2.21 (string "no events")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.18
							(patterns
								(p-applied-tag @3.6-3.17)))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @3.68-3.69 (ident "y"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @4.5-4.32
							(patterns
								(p-applied-tag @4.6-4.19))
							(rest-at (index 1)
								(p-assign @1.1-1.1 (ident "rest"))))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @4.70-4.78 (ident "len"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @5.5-5.49
							(patterns
								(p-applied-tag @5.6-5.18)
								(p-applied-tag @5.20-5.34))
							(rest-at (index 2)
								(p-assign @1.1-1.1 (ident "others"))))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @5.88-5.90 (ident "dy"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @5.99-5.108 (ident "toStr"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @5.126-5.129 (ident "dy2"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @6.5-6.51
							(patterns
								(p-applied-tag @6.6-6.20)
								(p-applied-tag @6.22-6.33))
							(rest-at (index 2)
								(p-assign @1.1-1.1 (ident "remaining"))))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @6.109-6.110 (ident "x"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign @6.114-6.123 (ident "toStr"))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-runtime-error (tag "expr_not_canonicalized"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error @1.1-1.1 (tag "pattern_not_canonicalized"))))
				(value
					(e-string @7.10-7.31
						(e-literal @7.11-7.30 (string "other event pattern"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "Error"))
~~~
