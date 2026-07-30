# Records

A record is a collection of values that each have a unique _field name_ associated with them.

## Fields

TODO


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

TODO

## Optional Fields

TODO

## Nominal Records

TODO

## Defaulted Fields

TODO

## The Empty Record (`{}`) {#empty-record}

TODO
