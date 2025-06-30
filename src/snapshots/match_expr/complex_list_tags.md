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
# PROBLEMS
**UNEXPECTED TOKEN IN PATTERN**
The token **match events {
    [] => "no events"
    [** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:1:1:3:6:**
```roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:3:19:3:23:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                  ^^^^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:3:22:3:40:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                     ^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **}, ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:3:53:3:56:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:3:54:3:58:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                     ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **${Num** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:3:56:3:61:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                       ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **})** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:3:69:3:71:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                    ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **)"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:3:70:3:72:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                     ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:3:71:3:71:**
```roc
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
```
                                                                      


**UNEXPECTED TOKEN IN PATTERN**
The token **match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:1:1:4:6:**
```roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.. as** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:4:21:4:26:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                    ^^^^^


**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**complex_list_tags.md:4:31:4:35:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                              ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:4:33:4:37:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                ^^^^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:4:36:4:41:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                   ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.len rest** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:4:74:4:83:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                         ^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:4:83:4:85:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                  ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **} more events** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:4:84:4:97:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                   ^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token ** more events"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:4:85:4:98:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                    ^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:4:97:4:97:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
```
                                                                                                


**UNEXPECTED TOKEN IN EXPRESSION**
The token **match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
    [** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:1:1:5:6:**
```roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:5:53:5:60:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                    ^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **},** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:74:5:76:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                         ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:75:5:78:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                          ^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **${Num** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:76:5:81:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                           ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} then ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:90:5:97:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                         ^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token ** then ${** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:91:5:99:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                          ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${Num** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:97:5:102:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.toStr dx2** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:102:5:112:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                     ^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **},** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:112:5:114:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                               ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **,${** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:113:5:116:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${Num** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:114:5:119:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                 ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.toStr dy2** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:119:5:129:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                      ^^^^^^^^^^


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
The token **"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:5:130:5:131:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                                 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:5:130:5:130:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
```
                                                                                                                                 


**UNEXPECTED TOKEN IN PATTERN**
The token **match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
    [** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:1:1:6:6:**
```roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.. as** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:35:6:40:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                  ^^^^^


**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**complex_list_tags.md:6:50:6:54:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                 ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:52:6:56:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                   ^^^^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:6:55:6:63:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                      ^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **} then click at ** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:81:6:97:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                ^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token ** then click at ${** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:82:6:99:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                 ^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **${Num** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:97:6:102:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **},** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:110:6:112:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                             ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **,${** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:111:6:114:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **${Num** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:112:6:117:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                               ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.toStr y** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:117:6:125:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                    ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:125:6:126:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                            ^


**UNEXPECTED TOKEN IN PATTERN**
The token **"** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:6:126:6:127:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:6:126:6:126:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
```
                                                                                                                             


**UNEXPECTED TOKEN IN PATTERN**
The token **match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
    _** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:1:1:7:6:**
```roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
    _ => "other event pattern"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **_ =>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:7:5:7:9:**
```roc
    _ => "other event pattern"
```
    ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **=> "** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:7:7:7:11:**
```roc
    _ => "other event pattern"
```
      ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
    _ => "other event pattern"
}** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**complex_list_tags.md:1:1:8:2:**
```roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr x}, ${Num.toStr y})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr (List.len rest)} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr dx},${Num.toStr dy} then ${Num.toStr dx2},${Num.toStr dy2}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr amount} then click at ${Num.toStr x},${Num.toStr y}"
    _ => "other event pattern"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**complex_list_tags.md:8:1:8:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `expected_close_curly_at_end_of_match`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**complex_list_tags.md:8:2:8:2:**
```roc
}
```
 


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
OpenSquare(2:5-2:6),CloseSquare(2:6-2:7),OpFatArrow(2:8-2:10),StringStart(2:11-2:12),StringPart(2:12-2:21),StringEnd(2:21-2:22),Newline(1:1-1:1),
OpenSquare(3:5-3:6),UpperIdent(3:6-3:11),NoSpaceOpenRound(3:11-3:12),LowerIdent(3:12-3:13),Comma(3:13-3:14),LowerIdent(3:15-3:16),CloseRound(3:16-3:17),CloseSquare(3:17-3:18),OpFatArrow(3:19-3:21),StringStart(3:22-3:23),StringPart(3:23-3:40),OpenStringInterpolation(3:40-3:42),UpperIdent(3:42-3:45),NoSpaceDotLowerIdent(3:45-3:51),LowerIdent(3:52-3:53),CloseStringInterpolation(3:53-3:54),StringPart(3:54-3:56),OpenStringInterpolation(3:56-3:58),UpperIdent(3:58-3:61),NoSpaceDotLowerIdent(3:61-3:67),LowerIdent(3:68-3:69),CloseStringInterpolation(3:69-3:70),StringPart(3:70-3:71),StringEnd(3:71-3:72),Newline(1:1-1:1),
OpenSquare(4:5-4:6),UpperIdent(4:6-4:14),NoSpaceOpenRound(4:14-4:15),LowerIdent(4:15-4:18),CloseRound(4:18-4:19),Comma(4:19-4:20),DoubleDot(4:21-4:23),KwAs(4:24-4:26),LowerIdent(4:27-4:31),CloseSquare(4:31-4:32),OpFatArrow(4:33-4:35),StringStart(4:36-4:37),StringPart(4:37-4:41),OpenStringInterpolation(4:41-4:43),LowerIdent(4:43-4:46),CloseStringInterpolation(4:46-4:47),StringPart(4:47-4:57),OpenStringInterpolation(4:57-4:59),UpperIdent(4:59-4:62),NoSpaceDotLowerIdent(4:62-4:68),OpenRound(4:69-4:70),UpperIdent(4:70-4:74),NoSpaceDotLowerIdent(4:74-4:78),LowerIdent(4:79-4:83),CloseRound(4:83-4:84),CloseStringInterpolation(4:84-4:85),StringPart(4:85-4:97),StringEnd(4:97-4:98),Newline(1:1-1:1),
OpenSquare(5:5-5:6),UpperIdent(5:6-5:10),NoSpaceOpenRound(5:10-5:11),LowerIdent(5:11-5:13),Comma(5:13-5:14),LowerIdent(5:15-5:17),CloseRound(5:17-5:18),Comma(5:18-5:19),UpperIdent(5:20-5:24),NoSpaceOpenRound(5:24-5:25),LowerIdent(5:25-5:28),Comma(5:28-5:29),LowerIdent(5:30-5:33),CloseRound(5:33-5:34),Comma(5:34-5:35),DoubleDot(5:36-5:38),KwAs(5:39-5:41),LowerIdent(5:42-5:48),CloseSquare(5:48-5:49),OpFatArrow(5:50-5:52),StringStart(5:53-5:54),StringPart(5:54-5:60),OpenStringInterpolation(5:60-5:62),UpperIdent(5:62-5:65),NoSpaceDotLowerIdent(5:65-5:71),LowerIdent(5:72-5:74),CloseStringInterpolation(5:74-5:75),StringPart(5:75-5:76),OpenStringInterpolation(5:76-5:78),UpperIdent(5:78-5:81),NoSpaceDotLowerIdent(5:81-5:87),LowerIdent(5:88-5:90),CloseStringInterpolation(5:90-5:91),StringPart(5:91-5:97),OpenStringInterpolation(5:97-5:99),UpperIdent(5:99-5:102),NoSpaceDotLowerIdent(5:102-5:108),LowerIdent(5:109-5:112),CloseStringInterpolation(5:112-5:113),StringPart(5:113-5:114),OpenStringInterpolation(5:114-5:116),UpperIdent(5:116-5:119),NoSpaceDotLowerIdent(5:119-5:125),LowerIdent(5:126-5:129),CloseStringInterpolation(5:129-5:130),StringPart(5:130-5:130),StringEnd(5:130-5:131),Newline(1:1-1:1),
OpenSquare(6:5-6:6),UpperIdent(6:6-6:12),NoSpaceOpenRound(6:12-6:13),LowerIdent(6:13-6:19),CloseRound(6:19-6:20),Comma(6:20-6:21),UpperIdent(6:22-6:27),NoSpaceOpenRound(6:27-6:28),LowerIdent(6:28-6:29),Comma(6:29-6:30),LowerIdent(6:31-6:32),CloseRound(6:32-6:33),Comma(6:33-6:34),DoubleDot(6:35-6:37),KwAs(6:38-6:40),LowerIdent(6:41-6:50),CloseSquare(6:50-6:51),OpFatArrow(6:52-6:54),StringStart(6:55-6:56),StringPart(6:56-6:63),OpenStringInterpolation(6:63-6:65),UpperIdent(6:65-6:68),NoSpaceDotLowerIdent(6:68-6:74),LowerIdent(6:75-6:81),CloseStringInterpolation(6:81-6:82),StringPart(6:82-6:97),OpenStringInterpolation(6:97-6:99),UpperIdent(6:99-6:102),NoSpaceDotLowerIdent(6:102-6:108),LowerIdent(6:109-6:110),CloseStringInterpolation(6:110-6:111),StringPart(6:111-6:112),OpenStringInterpolation(6:112-6:114),UpperIdent(6:114-6:117),NoSpaceDotLowerIdent(6:117-6:123),LowerIdent(6:124-6:125),CloseStringInterpolation(6:125-6:126),StringPart(6:126-6:126),StringEnd(6:126-6:127),Newline(1:1-1:1),
Underscore(7:5-7:6),OpFatArrow(7:7-7:9),StringStart(7:10-7:11),StringPart(7:11-7:30),StringEnd(7:30-7:31),Newline(1:1-1:1),
CloseCurly(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-malformed @8.2-8.2 (reason "expected_close_curly_at_end_of_match"))
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
