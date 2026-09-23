# Records

A record is a collection of values identified by unique field names. Records are useful when each value has a distinct role and naming those roles makes the code clearer.

```roc
person = { name: "Sam", age: 32 }
```

Here, `person` has two fields: `name`, whose value is a `Str`, and `age`, whose value is a number.

## Fields

### Record Literals

A record literal puts comma-separated fields between `{` and `}`. Each field has a lowercase name, a colon, and a value:

```roc
book = {
    title: "The Left Hand of Darkness",
    pages: 304,
    available: True,
}
```

The trailing comma controls how `roc fmt` lays out the record. With a trailing comma, the formatter puts the fields on separate lines. A field written with its name and value does not need a trailing comma, even when it is the only field:

```roc
settings = { theme: "dark" }
```

If an identifier with the field's name is already in scope, writing just that identifier as a record field is shorthand for `name: name`:

```roc
make_person = |name, age| { name, age }
```

A single field pun is the exception. It needs a comma because `{ name }` is a block containing the expression `name`, while `{ name, }` is a record:

```roc
make_name_record = |name| {
    name,
}
```

Record literals can contain arbitrary expressions, and records can be nested:

```roc
user = {
    name: "Kim",
    address: { city: "Oslo", postal_code: "0150" },
}
```

### Accessing Fields

Use a dot followed by the field name to read a required field:

```roc
city = user.address.city
```

The compiler checks that the record has the field and that each access has the expected type. Field access does not perform a string lookup at runtime; the compiler already knows which field is being accessed.

Records can also be destructured to give names to several fields at once:

```roc
full_name = |person| {
    { first, last } = person
    "${first} ${last}"
}
```

### Record Types

A record type gives a type to each field:

```roc
Person : { name : Str, age : U64 }

alice : Person
alice = { name: "Alice", age: 30 }
```

`Person` is a type alias. It is another name for the structural record type, so a record with the same fields and field types is a `Person` without an explicit conversion.

Field order is not part of a record's type. These annotations describe the same type:

```roc
PersonA : { name : Str, age : U64 }
PersonB : { age : U64, name : Str }
```

### Updating Records

Use `..` in a record literal to make a new record from an existing one while replacing fields:

```roc
have_birthday = |person| {
    ..person,
    age: person.age + 1,
}
```

The original record is unchanged. The update expression creates the result using all its fields, with the listed replacements.

Record update syntax can only replace fields that already exist, and each replacement must have the same type as the old field. If the new record needs an additional field or a field with a different type, construct it explicitly:

```roc
with_display_name = |person| {
    name: person.name,
    age: person.age,
    display_name: "${person.name} (${person.age.to_str()})",
}
```

This restriction makes `{ ..record, field: value }` unambiguously an update of the record's existing shape.

## Compared to Dictionaries

Records and dictionaries both associate names or keys with values, but they serve different purposes:

- A record's field names and field types are fixed at compile time. A dictionary can gain and lose entries at runtime.
- Fields in one record can have different types. All values in one dictionary have the same type.
- Record fields always have lowercase names known to the compiler. Dictionary keys are runtime values and can have types other than strings.
- Required record field access returns the field value directly. A dictionary lookup returns a `Try`, because the requested key might not be present.
- Record field names normally need no runtime storage. The compiler selects fields from their known layout. A dictionary stores its keys so it can compare them at runtime.

Creating a record does not by itself imply either stack or heap allocation. The compiler chooses a representation based on how the record is used, and a record can contain values such as strings and lists that manage separate storage of their own. The useful guarantee is that ordinary field access does not require a dictionary-style key lookup.

Use a record when the program knows the fields in advance and each field has a particular meaning. Use a [dictionary](dictionaries-and-sets) when the set of keys varies at runtime.

## Structural Records

A structural record is identified by its fields rather than by a declared type name. No declaration is needed:

```roc
origin = { x: 0, y: 0 }
```

Two structural record types are the same when they have the same field names and compatible field types. This lets functions accept record literals directly:

```roc
distance_squared : { x : F64, y : F64 } -> F64
distance_squared = |point| point.x * point.x + point.y * point.y
```

### Open Record Types

A closed record type lists its complete set of fields. An open record type ends with `..` and requires at least the listed fields while allowing others:

```roc
get_name : { name : Str, .. } -> Str
get_name = |record| record.name

name = get_name({ name: "Ari", age: 41 })
```

Use a named extension variable when the rest of the record's shape must be related across more than one type:

```roc
get_name_named : { name : Str, ..rest } -> Str
get_name_named = |record| record.name
```

Here, `rest` stands for the remaining fields. An anonymous `..` introduces a fresh extension variable without giving it a name.

Open record types express the fields a function needs. They are a form of structural polymorphism, not runtime-sized records: every concrete record still has a shape known at compile time.

## Optional Fields

An optional field may be present or absent at runtime. Put `?` after its name in a record type:

```roc
Attributes : { count : U64, label ?: Str }

labeled : Attributes
labeled = { count: 3, label: "new" }

unlabeled : Attributes
unlabeled = { count: 3 }
```

Because the field may be absent, query it with `.?` instead of ordinary field access:

```roc
read_label : Attributes -> Try(Str, [MissingField])
read_label = |attributes| attributes.?label
```

The query returns `Ok(value)` when the field is present and `Err(MissingField)` when it is absent. The `??` operator can supply a fallback for that `Try`:

```roc
display_label = |attributes| attributes.?label ?? "untitled"
```

Optional access can continue through required fields. The whole access still returns a `Try` if an optional segment is absent:

```roc
city : { address ?: { city : Str } } -> Try(Str, [MissingField])
city = |person| person.?address.city
```

Optional fields are useful when presence itself is runtime data, such as a field decoded from an external format. If omission should instead produce a definite value during construction, use a defaulted field.

## Nominal Records

A nominal record has a declared identity in addition to its fields. Declare one with `:=`:

```roc
Point := { x : F64, y : F64 }
```

Construct it explicitly by putting the type name before its record literal:

```roc
origin = Point.{ x: 0, y: 0 }
```

A bare record literal can also lift into a nominal record through structural unification when the surrounding context expects that nominal type. Its fields must match the nominal type's backing record:

```roc
unit_x : Point
unit_x = { x: 1, y: 0 }
```

Nominal identity prevents unrelated record types with identical fields from being used interchangeably. It also gives the type a place for associated methods:

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

Outside the declaration, use `Point.origin` and `point.translate(dx, dy)`. See [Nominal Types](types#nominal-types) for opaque types, explicit construction, and nesting nominal types.

Declaring with `::` makes a nominal record opaque. Outside its defining module, other code knows the nominal type but cannot access its backing fields, construct it with a record literal, or destructure it as a record. The defining module can expose the operations that preserve the type's invariants:

```roc
Account :: { balance : U64 }.{
    new : U64 -> Account
    new = |balance| Account.{ balance }

    balance : Account -> U64
    balance = |Account.{ balance }| balance
}
```

Code in other modules uses `Account.new` and `Account.balance` without depending on the backing record. See [Opaque Nominal Types](types#opaque-nominal-types) for the same abstraction rule with other backing types.

## Defaulted Fields

A defaulted field is a required field whose value can be omitted when constructing a nominal record. Write `??` after its type, followed by the default expression:

```roc
RequestOptions := {
    retries : U8 ?? 3,
    timeout_ms : U64 ?? 5000,
}
```

Omitting either field from the nominal constructor evaluates its default:

```roc
standard = RequestOptions.{}
patient = RequestOptions.{ timeout_ms: 30000 }
```

After construction, both values have both fields. Ordinary access therefore returns the value directly:

```roc
attempts = standard.retries
```

A supplied field overrides its default. Default expressions can be more than literals; they can use blocks and call pure functions, and they are evaluated for a construction that omits the field:

```roc
CacheOptions := {
    capacity : U64 ?? {
        kibibytes = 1024
        8 * kibibytes
    },
}
```

A default cannot perform effects or constrain any of the nominal type's type parameters. Defaults must also be acyclic: materializing one default cannot require another omitted default that eventually leads back to the first.

Defaulted and optional fields answer different questions. A defaulted field is always present in the constructed value. An optional field preserves whether a value was supplied, so querying it returns a `Try`.

## Runtime Layout

This section describes Roc's current native layout and host ABI. Most Roc programs should rely on field names and types rather than byte offsets. Code that exchanges records with a host should use generated glue as the source of truth for the selected target.

A record is represented as one inline aggregate, similar to a C struct. The record does not carry its field names or type annotation at runtime; the compiler has already turned each field access into an access at a known offset. The record's fields still have their own representations. For example, an inline `Str` field contains the string's runtime representation, which may refer to separately allocated string data. Explicitly putting a record in a `Box` also gives it a boxed representation.

Each field starts at an offset suitable for its alignment. The compiler inserts unused bytes when the next field needs a stricter alignment, then rounds the record's total size up to its strictest alignment. Pointer-sized values differ between targets, so the same record can have different offsets and a different total size on 32-bit and 64-bit targets. Some common current representations are:

| Type | 32-bit size and alignment | 64-bit size and alignment |
| --- | --- | --- |
| `U8` | 1 byte, aligned to 1 | 1 byte, aligned to 1 |
| `U32` | 4 bytes, aligned to 4 | 4 bytes, aligned to 4 |
| `U64` | 8 bytes, aligned to 8 | 8 bytes, aligned to 8 |
| `Str` | 12 bytes, aligned to 4 | 24 bytes, aligned to 8 |
| `List(a)` | 12 bytes, aligned to 4 | 24 bytes, aligned to 8 |
| `Box(a)` | 4 bytes, aligned to 4 | 8 bytes, aligned to 8 |

For structural records, source order does not determine memory order. The compiler first orders fields by decreasing alignment class. Fields in the same class are ordered alphabetically by name. This produces a compact, deterministic layout. Nominal records use the same rule by default.

An unnamed field in a nominal record switches that record to declared-order layout. This is useful for a host-facing type intended to mirror a C struct:

```roc
Header := {
    tag : U8,
    _ : {},
    value : U32,
}
```

Here, `_ : {}` occupies no bytes; its presence selects declared order. `tag` is therefore at offset 0, the compiler inserts three bytes to align `value`, and `value` is at offset 4. Without the unnamed field, the default layout would put `value` before `tag`.

An unnamed field with a nonempty type reserves that many bytes as explicit padding. It stores no value, cannot be accessed, and has alignment 1 regardless of its type:

```roc
Padded := {
    tag : U32,
    _ : U32,
    value : U32,
}
```

This layout has `tag` at offset 0, four reserved bytes at offset 4, and `value` at offset 8. Names beginning with `_`, such as `_reserved`, can be used for the same purpose. Unnamed fields are only allowed in nominal record declarations.

These layout rules matter at interoperability boundaries, but they are implementation and ABI details rather than part of structural record equality. Generated glue records the committed field order, offsets, sizes, and alignments for both pointer widths so that host code can use the exact layout chosen by the compiler.

## The Empty Record (`{}`) {#empty-record}

`{}` is both the empty record literal and the spelling of its type. It has exactly one value and carries no fields:

```roc
empty : {}
empty = {}
```

The empty record is useful when an API needs a value but has no information to carry. It is inhabited: its one possible value is `{}`.

For a utility or namespace module that only defines associated items, prefer the empty tag union `[]` as an opaque backing type. No value of `[]` can be constructed, so no value of the module type can be constructed either:

```roc
Math :: [].{
    double : U64 -> U64
    double = |n| n * 2
}

answer = Math.double(21)
```

`{}` is a closed record with no fields. By contrast, `{ .. }` is an open record type that accepts any record shape.
