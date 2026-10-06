# Records

A _record_ is a group of values where each value has a name. For example:

```roc
person = { name: "Sam", age: 32 }
```

Here, `person` is a record with two _fields_: `name` (whose value is the string `"Sam"`)
and `age` (whose value is the number `32`).

Records are a good fit when you know in advance exactly which values you'll have, and
each one means something different. If you don't know the names until runtime, you
want a [dictionary](dictionaries-and-sets) instead.

## Fields

### Record Literals

A record literal is a list of fields between `{` and `}`, separated by commas. Each
field is a lowercase name, a `:`, and an expression:

```roc
book = {
    title: "The Left Hand of Darkness",
    pages: 304,
    available: True,
}
```

Fields can hold any expression, including other records:

```roc
user = {
    name: "Kim",
    address: { city: "Oslo", postal_code: "0150" },
}
```

The trailing comma is optional, but it affects formatting: if there's a trailing comma,
`roc fmt` puts each field on its own line. If there isn't one, it puts the whole record
on one line.

If you already have a name in scope that matches the field name, you can write just
the name. `{ name, age }` is syntax sugar for `{ name: name, age: age }`:

```roc
make_person = |name, age| { name, age }
```

There's one exception. `{ name }` is a [block expression](expressions#block-expressions)
that evaluates to `name`, not a record. To make a single-field record this way, add a
trailing comma: `{ name, }`.

> Note that this exception exists because `{ x }` comes up all the time as a block,
> for example in `else { x }`, whereas single-field records written with the
> shorthand are rare. Making the common case the convenient one seemed like the
> better tradeoff.

### Accessing Fields

You can access a field with `.` followed by the field's name:

```roc
city = user.address.city
```

If the record doesn't have that field, you get an error at compile time. There's no
such thing as a runtime "field not found" error, because the compiler always knows
exactly which fields a record has.

You can also [destructure](pattern-matching#destructuring) a record to name several of
its fields at once:

```roc
full_name = |person| {
    { first, last } = person
    "${first} ${last}"
}
```

A record pattern like `{ first, last }` only matches records with exactly those fields. To
match a record that might have other fields too, put `..` at the end:

```roc
first_name = |person| {
    { first, .. } = person
    first
}
```

### Record Types

A record type looks like a record literal, except with types instead of values, and
`:` with spaces around it:

```roc
Person : { name : Str, age : U64 }

alice : Person
alice = { name: "Alice", age: 30 }
```

Here, `Person` is a [type alias](types#type-aliases). It's just another name for
`{ name : Str, age : U64 }`, so any record with those fields (and those field types)
is a `Person`.

The order of fields doesn't matter. These are the same type:

```roc
PersonA : { name : Str, age : U64 }
PersonB : { age : U64, name : Str }
```

### Record Equality

Two records are equal (according to `==`) if all of their fields are equal. Field order
doesn't matter here either, so `{ a: 1, b: "x" } == { b: "x", a: 1 }` is `True`.

[Nominal records](#nominal-records) are different. They don't get `==` automatically;
you either define an `is_eq` method yourself or ask the compiler to
[derive one](static-dispatch#compiler-derived-methods).

### Updating Records

Records are [values](expressions#values), so you can't change one. What you can do is
make a new record that's the same as an old one, except for some fields. The syntax
for this is `..` followed by the original record:

```roc
have_birthday = |person| {
    ..person,
    age: person.age + 1,
}
```

This returns a new record with all the fields of `person`, except with `age` replaced.
The original `person` is unaffected. (Behind the scenes, the compiler can often reuse
the original's memory; see [Performance](#performance).)

Record updates can only replace fields that already exist, and the new value has to
have the same type as the old one. Both of these give errors at compile time:

```roc
renamed = { ..person, nickname: "Sam" } # ERROR! person has no nickname field
aged = { ..person, age: "old" }         # ERROR! age is a number, not a Str
```

If you want a record with different fields, write it out:

```roc
with_display_name = |person| {
    name: person.name,
    age: person.age,
    display_name: "${person.name} (${person.age.to_str()})",
}
```

This restriction means that when you see `{ ..record, field: value }`, you always know
the result has the same type as `record`.

## Structural Records

Records are _structural_ by default, which means:

- You don't need to declare a record type before using it. `{ x: 0, y: 0 }` works on its own.
- Two record types are the same if they have the same field names and field types.

So a function that takes `{ x : F64, y : F64 }` accepts any record with exactly
those two fields, including a record literal written right there in the call:

```roc
distance_squared : { x : F64, y : F64 } -> F64
distance_squared = |point| point.x * point.x + point.y * point.y

answer = distance_squared({ x: 3, y: 4 })
```

### Open Record Types

The record type `{ name : Str }` means "a record with a `name` field, and no other
fields." If you want to say "a record with a `name` field, and possibly others," put
`..` at the end:

```roc
get_name : { name : Str, .. } -> Str
get_name = |record| record.name

name = get_name({ name: "Ari", age: 41 }) # This is fine, even though it has an age field.
```

`{ name : Str }` is a _closed_ record type, and `{ name : Str, .. }` is an _open_
record type.

If you need to refer to "the other fields" elsewhere in the type, give them a name
after the `..`:

```roc
set_name : { name : Str, ..others }, Str -> { name : Str, ..others }
set_name = |record, name| { ..record, name }
```

This says that whatever other fields came in also go out. (A plain `..` is the same
thing, except there's no name to refer to.)

Note that open record types describe what a function _accepts_. They don't make records
themselves flexible at runtime. Every record that actually gets created still has a
specific set of fields known at compile time, and the compiler makes a separate copy of
`get_name` for each of those sets of fields.

## Optional Fields

An _optional field_ is a field that might not be there at runtime. You write it with
`?:` instead of `:` in the record type:

```roc
Attributes : { count : U64, label ?: Str }

labeled : Attributes
labeled = { count: 3, label: "new" }

unlabeled : Attributes
unlabeled = { count: 3 } # This is fine, because label is optional.
```

Since the field might be missing, you can't access it with `.` like a normal field.
Instead, you use `.?`, which gives you a [`Try`](../builtins/Try):

```roc
read_label : Attributes -> Try(Str, [MissingField])
read_label = |attributes| attributes.?label
```

If the field is there, you get `Ok` with its value. If it isn't, you get
`Err(MissingField)`. The [`??` operator](operators#-default-value-on-err) is a
convenient way to provide a value for the missing case:

```roc
display_label = |attributes| attributes.?label ?? "untitled"
```

You can keep accessing fields after a `.?`. If any optional field along the way is
missing, the whole thing is `Err(MissingField)`:

```roc
city : { address ?: { city : Str } } -> Try(Str, [MissingField])
city = |person| person.?address.city
```

Optional fields are for when it matters at runtime whether the field was provided,
such as when a field might or might not appear in some JSON you're [parsing](parsers).
If you just want a field to have a default value when someone doesn't provide it,
use a [defaulted field](#defaulted-fields) instead.

## Nominal Records

A _nominal_ record type has a name, and no other type is considered the same as it,
even a record type with exactly the same fields. You declare one with `:=`:

```roc
Point := { x : F64, y : F64 }
```

To create a `Point`, you can write the type's name before the record:

```roc
origin = Point.{ x: 0, y: 0 }
```

If the compiler already knows a `Point` is expected (for example, because of a type
annotation), you can leave off the name:

```roc
unit_x : Point
unit_x = { x: 1, y: 0 }
```

Since a nominal record has a name, it can also have [methods](static-dispatch#methods):

```roc
Point := { x : F64, y : F64 }.{
    origin : Point
    origin = Point.{ x: 0, y: 0 }

    translate : Point, F64, F64 -> Point
    translate = |point, dx, dy| {
        ..point,
        x: point.x + dx,
        y: point.y + dy,
    }
}
```

Code outside the declaration can use these as `Point.origin` and
`point.translate(dx, dy)`. There's more about nominal types in general on the
[types](types#nominal-types) page.

### Opaque Nominal Records

If you declare a nominal record with `::` instead of `:=`, it's _opaque_. Outside the
module where it's declared, nobody can see its fields. That means they can't access
them, create the record with a record literal, or destructure it. All they can do is
use the functions you expose:

```roc
Account :: { balance : U64 }.{
    new : U64 -> Account
    new = |balance| Account.{ balance }

    balance : Account -> U64
    balance = |Account.{ balance }| balance
}
```

This is useful when you want to enforce rules about what's in a record. For example,
if an `Account` should never be created with a balance above some limit, `new` is the
only way to create one, so `new` is the only place that needs to check the limit.
(The [types](types#opaque-nominal-types) page covers opaque types in general.)

## Defaulted Fields

A _defaulted field_ is a field in a nominal record that you can leave out when creating
the record, in which case it gets a default value. You write the default after `??`:

```roc
RequestOptions := {
    retries : U8 ?? 3,
    timeout_ms : U64 ?? 5000,
}
```

Now you can leave out either field (or both):

```roc
standard = RequestOptions.{}                     # retries is 3, timeout_ms is 5000
patient = RequestOptions.{ timeout_ms: 30000 }   # retries is 3, timeout_ms is 30000
```

Once the record has been created, a defaulted field is just a normal field. It's always
there, so you access it with `.` like any other field:

```roc
attempts = standard.retries
```

The default can be any expression, as long as it doesn't call any
[effectful functions](functions#effectful-functions):

```roc
CacheOptions := {
    capacity : U64 ?? {
        kibibytes = 1024
        8 * kibibytes
    },
}
```

The default gets evaluated each time a record is created without that field.

Defaults can't depend on each other in a cycle, and they can't affect what the
nominal type's type parameters are (if it has any).

> Note that defaulted fields and [optional fields](#optional-fields) answer different
> questions. A defaulted field asks "what should this be if nobody says?" and the
> answer is always a value. An optional field asks "did anybody say?" and the answer
> might be no, which is why accessing it gives you a `Try`.

## The Empty Record (`{}`) {#empty-record}

`{}` is a record with no fields. It's also the name of its own type:

```roc
empty : {}
empty = {}
```

There's exactly one value of type `{}`, which is `{}`. That makes it useful when you
need to provide a value, but there's no information to provide.

Note that `{}` is a closed record type with no fields, whereas `{ .. }` is an open
record type that accepts any record at all.

If you want a module that's just a namespace for some functions, and you never want
anyone to create a value of the module's type, use the empty tag union `[]` as the
backing type instead of `{}`. There are no values of type `[]`, so there can be no
values of your type either:

```roc
Math :: [].{
    double : U64 -> U64
    double = |n| n * 2
}

answer = Math.double(21)
```

## Performance

### Memory Layout

A record's fields are stored next to each other in memory, much like a
[struct](https://en.wikipedia.org/wiki/Struct_(C_programming_language)) in C. The record
doesn't get its own heap allocation and doesn't have a
[reference count](expressions#reference-counting). It takes up exactly as much space
as its fields do, plus any padding needed for alignment.

None of the record's field names exist at runtime. When you write `user.name`, the
compiler already knows exactly how many bytes from the start of the record `name` is,
so accessing a field is as fast as reading memory at a known offset. (This is one of
the big performance differences between records and dictionaries. Looking something
up in a [dictionary](dictionaries-and-sets) means hashing the key and comparing it
against the keys stored in the dictionary.)

Fields that are themselves heap-allocated, like strings and lists, work the same way
they do anywhere else. For example, on a 64-bit target, a `Str` field takes up 24
bytes in the record. If the string is 23 bytes or shorter, its contents fit right
there in those 24 bytes; otherwise, its contents live in a separate heap allocation. The record holds the string, but the
string manages its own memory.

Here are the sizes of some common field types:

| Type | 32-bit size and alignment | 64-bit size and alignment |
| --- | --- | --- |
| `U8` | 1 byte, aligned to 1 | 1 byte, aligned to 1 |
| `U32` | 4 bytes, aligned to 4 | 4 bytes, aligned to 4 |
| `U64` | 8 bytes, aligned to 8 | 8 bytes, aligned to 8 |
| `Str` | 12 bytes, aligned to 4 | 24 bytes, aligned to 8 |
| `List(a)` | 12 bytes, aligned to 4 | 24 bytes, aligned to 8 |
| `Box(a)` | 4 bytes, aligned to 4 | 8 bytes, aligned to 8 |

### Field Order and Padding

The order you write fields in has no effect on how they're laid out in memory. The
compiler sorts them by alignment, from most strictly aligned to least, and sorts fields
with the same alignment alphabetically by name. This puts as little padding between
fields as possible.

For example, `{ flag : Bool, count : U64, id : U32 }` is laid out as `count`, then
`id`, then `flag`. That's 16 bytes total (8 + 4 + 1, rounded up to a multiple of 8).
If the fields were stored in the order they were written, it would be 24 bytes, because
`count` would need 7 bytes of padding before it.

This means you never need to reorder a record's fields to make it smaller. The
compiler has already done it.

### Optional Fields Take Up More Space

At runtime, an optional field is stored like a [tag union](tag-unions) with two tags:
one for when the field is present (holding its value) and one for when it's missing.
That takes the space of the value, plus one byte to say which tag it is, plus padding
to keep the next value aligned.

For example, on a 64-bit target, `label : Str` takes 24 bytes, but `label ?: Str` takes
32. If you're storing a lot of records in a [list](../builtins/List) and you don't
actually need to know whether a field was provided, a [defaulted field](#defaulted-fields)
takes no extra space.

### Copying and Updating

Since records are stored inline, passing a record around means its bytes get copied
around. For small records, that's cheap. It's also cheap for records whose fields are
strings or lists, because only the small fixed-size part of each string or list gets
copied, not its contents.

When a record contains heap-allocated fields and you use the record somewhere it's
still needed later, those fields' reference counts get incremented. This works the
same way as using a string or list directly.

Updating a record with `{ ..record, field: value }` creates a new record. If the
original record isn't used again afterward, the fields that didn't change (including
any strings and lists in them) get moved into the new record without touching their
reference counts.

### Records at the Host Boundary

Most of the time, the compiler's field order is exactly what you want. The exception
is when a [platform](platforms) needs a record's memory to match a struct in some other
language, which expects fields in the order they were written.

For this, a nominal record can include a field named `_`. This makes the compiler lay
out the record's fields in the order you wrote them, with padding inserted the way C
would insert it:

```roc
Header := {
    tag : U8,
    _ : {},
    value : U32,
}
```

The `_ : {}` field takes up no space; it's only there to say "use the order I wrote."
So `tag` is at offset 0, followed by three bytes of padding, and then `value` is at
offset 4. (Without the `_` field, `value` would come first, because it has the stricter
alignment.)

A `_` field with a type other than `{}` reserves that many bytes of padding. These
bytes don't hold a value and can't be accessed:

```roc
Padded := {
    tag : U32,
    _ : U32,
    value : U32,
}
```

Here, `tag` is at offset 0, four reserved bytes are at offset 4, and `value` is at
offset 8. Note that a reserved field is always aligned to 1 byte, regardless of its
type. You can also give reserved fields names that start with `_`, like `_reserved`.

Only nominal records can have `_` fields.

> Note that platform authors shouldn't compute these offsets by hand. `roc glue`
> generates host code using the exact layout the compiler chose for each target, on
> both 32-bit and 64-bit targets.
