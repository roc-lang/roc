# Comments

Comments always begin with a `#` character and extend to the end of the line.

Roc supports single-line comments as well as [doc comments](#doc-comments). There is no multi-line comment syntax.

## Single-line Comments

Here are some examples of single-line comments:

```roc
# This comment takes up a whole line.
# So does this one.
# There is no dedicated multi-line comment syntax.

x = 5 # end-of-line comment
```

By design, Roc's compiler never derives any semantic meaning from comments. Modifying comments should
never affect the behavior of your running program, aside from source code locations in stack traces.

### Shebang Comments

Some shells look for a `#!` (known as a shebang) at the start of an executable text file. For example:

```roc
#!/usr/bin/env roc
```

Roc doesn't have special support for shebangs, but since `#` begins an ordinary Roc comment, 
this example would be treated as a comment by Roc's compiler, while potentially being read
as a shebang by a shell.

## Doc Comments

Doc comments add documentation to an assignment. They have a special comment syntax:

- Each line of a doc comment begins with `"## "` - so, two `#`s and then a space, at the very beginning of the line.
- Each consecutive line that begins with `"## "` continues the doc comment.
- The next line after a doc comment's final `"## "` line must begin with an [assignment statement](statements#assignment).
    - If one or more lines beginning with `"## "` are not followed immediately by an assignment statement at the beginning of the next line, then none of them are considered a doc comment; they are instead treated as an ordinary comment.

For example:

````roc
## Returns the given number unmodified if it's even, 
## and negated if it's odd.
##
## ```roc
## expect negate_if_odd(1) == -1
## expect negate_if_odd(2) == 2
## ```
negate_if_odd = |num| if num.is_odd() {
	num.negate()
} else {
	num
}
````

TODO

<!-- notes:
explain code blocks in here.
explain [autolink] syntax inside doc comments.
-->

## Generating Docs with `roc docs`

TODO

<!-- notes:
explain `roc docs`
explain `roc docs --serve`
-->
