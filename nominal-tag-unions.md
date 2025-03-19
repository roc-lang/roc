
## Nominal Tag Unions

Here's an example of how to define one:

```
FileErr := [
    NotFound,
    PermissionDenied,
    AlreadyExists,
]
```

When I write this declaration in a particular scope, it introduces a new type to the scope,
named `FileErr`. This is not a type alias! Just like how `Str` is not a type alias for anything,
but rather is only compatible with other `Str` types, this `FileErr` type is only compatible
with other `FileErr` types.

These compatibility rules make it a [nominal type](https://en.wikipedia.org/wiki/Nominal_type_system),
as opposed  to a [structural type](https://en.wikipedia.org/wiki/Structural_type_system) such
as a tag union or record. Structural types are compatible if they have the same structure,
whereas nominal types must have the same name (and )
* There are new tags in scope, `NotFound`, `PermissionDenied`, and `AlreadyExists`, which
