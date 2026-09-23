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

The text of a doc comment is Markdown. Headings, lists, tables, blockquotes, `inline code`,
_emphasis_, and links all work the way they do in other Markdown documents.

### Code Blocks in Doc Comments

A fenced code block (a line of three backticks, then some lines of code, then another line of
three backticks) is rendered as a code block in the generated documentation. The example above
uses one to show how `negate_if_odd` behaves.

Code blocks in doc comments are for readers only. The compiler does not type-check them or run
them, so an `expect` inside a doc comment's code block will not be run by [`roc test`](statements#expect).
If you want an example to be checked, write it as a top-level `expect` next to the function too.

### Autolinks

Writing a name inside square brackets, such as `[Str]` or `[Str.concat]`, creates an _autolink_
to that item's documentation. For example:

```roc
## Converts a [Greeting] to a [Str].
##
## To go the other direction, use [Greeting.from_str].
to_str : Greeting -> Str
```

In the generated documentation, `[Greeting]`, `[Str]`, and `[Greeting.from_str]` all become links
pointing to the documentation for those items. Autolinks can refer to items in the current
module, to other modules in the same package, and to builtin types and their associated items.

An autolink is only created when the brackets contain nothing but a name (optionally with
dot-separated parts). Ordinary Markdown links, like `[the Roc website](https://roc-lang.org)`,
continue to work as normal links.

## Generating Docs with `roc docs`

The `roc docs` command generates HTML documentation for a package or platform, using the
doc comments and type annotations in its modules:

```sh
roc docs main.roc
```

This writes a static website to a directory named `generated-docs` (use `--output=some/other/dir`
to write it somewhere else). The site has a page for each module the package exposes, and each
page lists that module's exposed items along with their types and doc comments. It also includes
a search box for finding items by name.

Since the result is a static website, you can publish it anywhere that can host static files.

### Viewing Docs Locally with `--serve`

To preview the documentation while you are writing it, pass `--serve`:

```sh
roc docs --serve main.roc
```

This generates the documentation as usual and then starts a local HTTP server
(at `http://localhost:8080`) that serves the generated files until you stop it with Ctrl+C.
