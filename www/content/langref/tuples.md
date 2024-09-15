# Tuples

### [Low-level details](#low-level) {#low-level}

Tuples and [records](records) have identical in-memory representations. (They both compile to the equivalent of [C structs](https://en.wikipedia.org/wiki/Struct_(C_programming_language))).

An easy way to figure out how a tuple will be represented at runtime is to pretend that it's a record with field names that correspond to tuple positions. For example, consider this tuple:

```roc
(Str, U16, Str)
```

This will be treated by the compiler as essentially equivalent to this record (if using numbers for record field names were allowed):

```roc
{ 0: Str, 1: U16, 2: Str }
```

First the fields will be sorted by alignment (so the two `Str`s will appear first, followed by the `U16`), and then "alphabetically by field name" (which in the case of tuples just means sorting by position).

All the other information discussed in the [low-level details of records](records#low-level) applies to tuples as well—alignment padding, closures, and so on.
