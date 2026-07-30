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

TODO

## Shadowing

TODO

## Constants

TODO

## Variables (with `var`)

### `var` keyword

TODO

### `$` prefix

TODO

## Type Variables

TODO

## Type Aliases

### Parameterized Type Aliases

TODO

## Module Names

TODO

## `as`

TODO
