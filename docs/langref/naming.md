# Naming

## Rules

Valid names in Roc have different rules depending on what they are used for.

### Lowercase Names

Lowercase names, used by [patterns](pattern-matching) (which include [assignments](statements#assignment)), [type variables](#type-variables), [record fields](records#fields), and [package shorthands](packages#shorthands), must follow these rules:

- The name is a combination of ASCII letters, numbers, and underscores.
    - Consecutive underscores are allowed, but discouraged stylistically.
- It must begin with either `_`, `$`, or a lowercase ASCII letter.
  - The `$` prefix is only for [reassignment with `var`](statements#reassignment), and must be followed by an ASCII lowercase letter.
  - The `_` prefix is only for naming things that don't actually get used, and must be followed by an ASCII lowercase letter.
    - The compiler will give a warning if a name begins with `_` and is referenced in the same scope.
    - Note that [the `_` pattern](pattern-matching#underscore) is not a name and doesn't actually name anything.
- It can optionally end with `!` if it's naming an [effectful function](functions#effectful-functions).

[Type variables](#type-variables), [record fields](records#fields), and [package shorthands](packages#shorthands) not only follow these rules, but also have the additional restriction that they may not include `$` or `!` anywhere. (All of them may still begin with an underscore to indicate that they are unused.)

### Uppercase Names

Uppercase names, used by [type](types) names and [tag](tag-unions) names, have the same rules as [lowercase names](#lowercase-names) except:
- They must begin with an ASCII uppercase letter (which implies they may not begin with an underscore)
- They may not include `$` or `!`
- Stylistically, they should not include any underscores

## Unused Names

The compiler gives a warning when a name is introduced but never used. For example:

```roc
total = |price, quantity| price * 2 # Warning: quantity is never used
```

To tell the compiler that a name is intentionally unused, begin it with an underscore:

```roc
total = |price, _quantity| price * 2 # No warning
```

The reverse is also true: if a name begins with `_` but is used after all, the compiler
warns that the underscore is misleading and suggests removing it.

If you don't want to give a name to the value at all, you can use the
[`_` pattern](pattern-matching#underscore) instead:

```roc
total = |price, _| price * 2
```

Naming the unused value (as in `_quantity`) can still be worthwhile, because
the name documents what the value would have been.

This warning applies to names inside functions and blocks, including function arguments
and names introduced by patterns. Top-level names in a module don't get unused warnings.

## Shadowing

_Shadowing_ is when a new name has the same spelling as a name that's already in scope.
Roc gives a warning for shadowing:

```roc
count = 1

increment = |count| count + 1 # Warning: count shadows the count above
```

It's usually easier to understand code when each name refers to exactly one thing
in a given scope, so the warning suggests picking a different name. Shadowing is
a warning rather than an error, so the program can still be run; the new name
refers to the new value until the end of its scope.

This applies to redefining a name in the same scope too:

```roc
answer = 1
answer = 2 # Warning: answer is already defined
```

If you want a name whose value changes over time, use [`var`](#variables-with-var) instead.

## Constants

A name introduced with an ordinary [assignment](statements#assignment) is a _constant_. Once it
has been given a value, that value never changes.

```roc
pi = 3.14159

area = |radius| pi * radius * radius
```

Constants defined at the top level of a module are evaluated at
[compile time](compile-time) whenever possible, so using them has no runtime cost beyond
reading the already-computed value.

Constants are the default in Roc; the only names whose values can change are
[variables](#variables-with-var).

## Variables (with `var`)

A _variable_ is a name whose value can be changed after it has been assigned.

```roc
var $count = 0

for item in items {
    $count = $count + 1
}
```

### `var` keyword

Variables are declared by writing `var` before their first assignment. After that,
the variable can be [reassigned](statements#reassignment) with an ordinary `=`, without writing `var` again.

Variables may only be declared inside a function body or block, not at the top level of a module:

```roc
var $total = 0 # Error: var is not allowed at the top level

sum = |numbers| {
    var $total = 0 # OK

    for num in numbers {
        $total = $total + num
    }

    $total
}
```

A variable can only be reassigned by the function that declared it. A nested function
(such as a lambda defined inside the function) can read the variable, but it cannot
reassign it:

```roc
count_items = |items| {
    var $count = 0

    add_one = || {
        $count = $count + 1 # Error: can't reassign $count from a nested function
    }

    …
}
```

When a nested function reads a variable, it sees the value the variable had
when the nested function was defined, not whatever value the variable may have later.

Unlike constants, variables are never generalized: a variable has exactly one type,
even if it's given a type annotation with type variables in it. (See
[Generalization](types#generalization) for more on this.)

### `$` prefix

Variable names must begin with `$`. The `$` is part of the name, so `$count` and `count` are
different names.

The `$` prefix makes it easy to tell at a glance which names might have their values
change over time, and which names are guaranteed to be constant. For example, when reading
`$count + count`, you can tell that `$count` might have a different value on the next
line, whereas `count` never will.

The compiler warns if a name declared with `var` does not begin with `$`, and also if a
constant's name begins with `$`:

```roc
var count = 0 # Warning: should be named $count

$total = 0 # Warning: $total is not declared with var
```

## Type Variables

A _type variable_ is a lowercase name used in a type annotation, which stands for
"any type." For example:

```roc
first : List(elem) -> Try(elem, [ListWasEmpty])
```

Here, `elem` is a type variable. It means `first` works on lists of any element type, and
whatever that element type is, the `Ok` payload will have the same type.

Type variables follow the rules for [lowercase names](#lowercase-names), except that they can't
include `$` or `!`. They can be longer than one letter, and descriptive names like `elem`, `key`,
or `state` often make annotations easier to read than single letters do.

Using the same type variable more than once in an annotation means those types must be the same.
To indicate that a type can be anything and doesn't need to be the same as any other type,
you can use a type variable that begins with an underscore (such as `_elem`), or just
an underscore by itself:

```roc
len : List(_elem) -> U64

is_empty : List(_) -> Bool
```

See [Types](types) for more on how type variables work.

## Type Aliases

A _type alias_ gives a new name to an existing type, using `:`. Type alias names follow
the rules for [uppercase names](#uppercase-names).

```roc
Point : { x : F64, y : F64 }
```

The alias and the type it names are interchangeable; see [Type Aliases](types#type-aliases)
for details.

### Parameterized Type Aliases

A type alias can have type parameters, which are written in parentheses after its name,
just like the type variables in a type annotation:

```roc
Pair(a) : (a, a)

swap : Pair(a) -> Pair(a)
swap = |(x, y)| (y, x)
```

When the alias is used, the arguments given for its parameters are substituted into its
definition. For example, `Pair(Str)` means `(Str, Str)`.

Type parameters of an alias must be named. Underscores are not allowed in type
declarations, because a type declaration needs to say exactly what type it is.

## Module Names

[Type modules](modules#type-modules) are named after the type they define. For example,
the type module that defines the `Url` type must be in a file named `Url.roc`. This means
type module names follow the same rules as [uppercase names](#uppercase-names), plus the
`.roc` file extension.

Other modules, such as a package's `main.roc` or an application's `main.roc`, don't define
a type with the module's name, so their file names are not restricted in this way.

When you [import](modules#import-statements) a module from a package, the module's name is
qualified with the package's shorthand, as in `import json.Parser`. The shorthand
(`json` here) is a lowercase name chosen by the importing module; see
[package shorthands](packages#shorthands).

## `as`

The `as` keyword gives an additional name to something. It's used in a few places:

- In [imports](modules#renaming-imported-modules-with-as), `import json.Parser as JP` makes
  the imported type available under the name `JP` instead of `Parser`.
- In [patterns](pattern-matching#naming-the-whole-value-with-as), `Ok(n) as result` matches
  an `Ok` and names its payload `n`, and also names the whole matched value `result`.
- In [list patterns](pattern-matching#list-patterns), `[first, .. as rest]` names the
  remaining elements of the list `rest`.

The name after `as` follows the usual rules for its position: an [uppercase name](#uppercase-names)
for an import, and a [lowercase name](#lowercase-names) in a pattern.
