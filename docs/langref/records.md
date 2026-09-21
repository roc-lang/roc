# Records

A record is a collection of values that each have a unique _field name_ associated with them.

```roc
user = { name: "Sam", email: "sam@example.com", age: 30 }
```

## Fields

Each value in a record is stored in a _field_, which has a name and a value. In the record above,
`name`, `email`, and `age` are field names. Field names are [lowercase names](naming#lowercase-names),
and a record can't have two fields with the same name.

The order in which fields are written doesn't matter: `{ x: 1, y: 2 }` and `{ y: 2, x: 1 }` are the
same record.

### Accessing Fields

Use a `.` followed by the field name to access a field's value:

```roc
user.name # "Sam"
```

It's a type error to access a field the record doesn't have, so if a program compiles without
errors, every field access will succeed at runtime.

### Field Shorthand

When building a record, if a field's value is a name that's the same as the field name,
you can write the name just once. For example, these two records are the same:

```roc
name = "Sam"
age = 30

with_shorthand = { name, age }
without_shorthand = { name: name, age: age }
```

One thing to watch out for: a single name in braces, like `{ name }`, is a
[block expression](expressions#block-expressions) that evaluates to `name`, not a record.
To make a record with a single field, write the field out in full, as in `{ name: name }`.

The same shorthand works in [record patterns](pattern-matching#destructuring):
`{ name, age } = user` is short for `{ name: name, age: age } = user`.

### Record Update Syntax

To make a new record that's the same as an existing one except for some fields, write
`..` followed by the existing record, then the fields that should be different:

```roc
older_user = { ..user, age: user.age + 1 }
```

This doesn't modify `user`. It produces a new record which has all the same field values
as `user`, except for `age`. (Under the hood, if nothing else is referencing `user`
anymore, Roc's compiler may reuse its memory instead of copying it; see
[opportunistic mutation](expressions#opportunistic-mutation).)

Record update syntax can only change the values of fields the record already has. It can't
add new fields or remove existing ones.


## Compared to Dictionaries

Records are different from [dictionaries](dictionaries-and-sets) in several ways:

- A dictionary's values must all have the same type, whereas a record's values can have completely different types.
- Record field names are a compile-time concept, and their strings are not stored at runtime by default. Instead, records have the same in-memory representation as C structs: unlabeled adjacent memory locations. (Roc's compiler translates the field names into the appropriate memory locations automatically.)
  - Field name strings may end up being available at runtime if something like a [parser](parsers) uses a record's field names to decide what to do at runtime.
  - In contrast, dictionaries always store all of their keys at runtime. Dictionaries can also have different types of keys, whereas record fields are always [lowercase names](naming#lowercase-names).
    - Record field names may not contain `$`, unlike [reassignable `var` identifiers](statements#reassignment).
  - This means that if you change a record field name in Roc, this will not change the amount of memory that record takes up at runtime, whereas if you change the size of a dictionary key, it can change the amount of memory the dictionary uses.
- Records are stack-allocated, so putting a group of values in a record does not introduce a heap allocation. Dictionaries heap-allocate space for their stored keys and values.
- A record's set of fields (including both their names and their types) is fixed at compile time and can't change at runtime.
    - You can make a new record using the contents of an old one, but the new one will also need a set of fields that's fixed at compile time.
    - In contrast, dictionaries can have arbitrary size at runtime. For example, you could parse the entire contents of a file into a dictionary, and its number of keys could vary at runtime based on the contents of the file. For a record, you couldn't do that because the record's exact number of fields is fixed at compile time and can't change at runtime.
- Reading a key out of a dictionary always returns a [`Try`](../Try), because that key might not be present at runtime. The same is true for [optional record fields](#optional-fields), but normal record fields can be accessed without a [`Try`](../Try) because they are guaranteed to be present at runtime.

## Structural Records

Record literals like `{ name: "Sam", age: 30 }` are _structural_, meaning their type is
determined entirely by their shape: the names and types of their fields. The type of that
record is `{ name : Str, age : U64 }` (assuming `age` is a `U64`), and any other record with
exactly those fields and field types has the same type. There's no need to declare the type anywhere
before using it.

A record type can end in `..` to indicate that it may have additional fields. This is how you
write a function that accepts any record which has _at least_ certain fields:

```roc
full_name : { first : Str, last : Str, .. } -> Str
full_name = |person| "${person.first} ${person.last}"
```

This `full_name` function can be called with `{ first: "Sam", last: "Lee" }`, but also with
`{ first: "Sam", last: "Lee", age: 30 }` or any other record that has `first` and `last` fields
which are strings.

If you want to refer to "the other fields" elsewhere in the annotation, you can give them a name,
like `..others`. See [Structural Types](types#structural-types) for details.

## Optional Fields

A record type can declare a field as _optional_ by writing `?:` instead of `:` after its name.
An optional field may or may not be present at runtime:

```roc
greet : { name : Str, greeting ?: Str } -> Str
greet = |args| {
    greeting = args.?greeting ?? "Hello"
    "${greeting}, ${args.name}!"
}
```

When a record literal is passed where a record with optional fields is expected, it can leave out
any of the optional fields:

```roc
greet({ name: "Sam" }) # "Hello, Sam!"
greet({ name: "Sam", greeting: "Howdy" }) # "Howdy, Sam!"
```

### Accessing Optional Fields with `.?`

Since an optional field might not be there, it can't be accessed with an ordinary `.`. (Trying to
do that gives a compile-time error.) Instead, use `.?`, which evaluates to a [`Try`](../Try):
`Ok` with the field's value if the field is present, or `Err(MissingField)` if it isn't.

```roc
args.?greeting # Ok("Howdy") or Err(MissingField)
```

This works well with the [`??` operator](operators#-default-value-on-err), as in the
`greet` example above, which uses `"Hello"` as the greeting if the field is missing.

A chain of field accesses containing `.?`, such as `config.?server.port`, produces a single
`Try` for the entire chain. If any optional field along the way is missing, the whole chain
evaluates to `Err(MissingField)`.

Using `.?` on a field that's always present is an error, because the `Err` case could never happen.
Use `.` for those fields instead.

### Destructuring Optional Fields

When you destructure an optional field, the name it introduces has a `Try` type, the same way
a `.?` access would:

```roc
greet : { name : Str, greeting ?: Str } -> Str
greet = |{ name, greeting }| match greeting {
    Ok(custom) => "${custom}, ${name}!"
    Err(MissingField) => "Hi, ${name}!"
}
```

### Unsetting Optional Fields

To make a record where an optional field is missing, write `_` as that field's value.
This is most useful with [record update syntax](#record-update-syntax):

```roc
without_greeting = { ..args, greeting: _ }
```

### Optional Fields Are Not Interchangeable with Required Fields

A record with a required field `{ name : Str }` and a record with an optional field
`{ name ?: Str }` are different types, because they are stored differently in memory
(an optional field also needs to track whether it's present). So a record whose type
says a field is always present can't be passed where the field is expected to be optional,
or vice versa. Record literals are the exception, because their fields can become either
required or optional depending on how they're used.

Platforms can't send records with optional fields to or from their host.

## Nominal Records

A _nominal record_ is a [nominal type](types#nominal-types) whose backing type is a record:

```roc
Point := { x : F64, y : F64 }
```

Unlike a structural record type, `Point` is a distinct type. A function that takes a `Point` won't
accept an ordinary `{ x : F64, y : F64 }` record which has already been given that structural type,
and vice versa. (A record _literal_ can become a `Point` if it's used where a `Point` is expected.)

To create a nominal record explicitly, write the type's name, then a `.`, then a record literal:

```roc
origin = Point.{ x: 0, y: 0 }
```

Fields of a nominal record can be accessed with `.` just like any other record,
as in `origin.x`. (If the type is [opaque](types#opaque-nominal-types), only its
own module can see its fields.)

Nominal records can have [methods](static-dispatch#methods), and they can
also have [defaulted fields](#defaulted-fields), which structural records can't.

## Defaulted Fields

A field in a nominal record's type declaration can be given a _default value_ by
writing `??` and then an expression after the field's type:

```roc
Config := { host : Str, port : U16 ?? 8080, retries : U8 ?? 3 }
```

When constructing a `Config`, any defaulted field can be left out, and it will be set to its
default value:

```roc
config = Config.{ host: "localhost" }

config.port # 8080
config.retries # 3
```

A defaulted field is always present once the record has been constructed, so it's accessed
with an ordinary `.`, not `.?`. That's the difference between a defaulted field and an
[optional field](#optional-fields): an omitted optional field is missing at runtime, whereas an
omitted defaulted field is filled in with its default when the record is constructed.

The default value can be any expression that doesn't call [effectful functions](functions#effectful-functions),
including one that references other top-level values or calls functions:

```roc
Settings := { timeout_ms : U64 ?? default_timeout * 2, name : Str ?? "untitled" }
```

A default value can't depend on itself (for example, by constructing the same record while omitting that
field), and a default for a field whose type is a type parameter can't be a literal, since the
literal wouldn't work for every possible type the parameter could be.

Defaults can only be declared on the fields of a nominal record's type declaration. They aren't
allowed in structural record types, including type aliases, and records nested inside a nominal
record's fields. This is because a default belongs to a specific named type; a structural record
type can be written in many different places, so there would be no single place its default could
come from.

## The Empty Record (`{}`) {#empty-record}

`{}` is a record with no fields. It's the only value of its type (which is also written `{}`),
so it carries no information at runtime, and it takes up no memory.

This makes `{}` useful when a value is required but there's no meaningful information to put in it.
For example:

- Functions that are only called for their [side effects](functions#side-effects) often return `{}`,
  like an `echo!` function which prints a string and returns `{}`.
- `Ok({})` is a common way to represent success when there's no other value to return.
- An `if` without an `else` must evaluate to `{}`; see [`if` / `else`](if-else).
- A [`Dict`](dictionaries-and-sets) whose values are `{}` works as a set of its keys.
  (Roc's `Set` type is implemented this way.)
