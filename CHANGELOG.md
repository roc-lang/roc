# Changelog

## Unreleased

### Table literals

Table literals are row-oriented sugar for a list of records. Column names are
written once; each body line is a row. After parsing, the compiler desugars
them to `List({ ... })`, so later compiler stages never see a table type.

The [homepage](https://www.roc-lang.org/) examples currently build that list by
repeating every field name on every row:

```roc
print_remaining! = |todos|
    todos
        .keep_if(|todo| todo.status != Done)
        .for_each!(|todo| echo!("- ${todo.name}\n"))

main! = |_args| {
    todos = [
        { name: "Learn Roc",       status: Done },
        { name: "Buy groceries",   status: Done },
        { name: "Write blog post", status: InProgress },
        { name: "Call mom",        status: NotStarted },
    ]
    print_remaining!(todos)
    Ok({})
}
```

The same program with a table literal keeps the filtering and printing, and
writes the data as a grid:

```roc
print_remaining! = |todos|
    todos
        .keep_if(|todo| todo.status != Done)
        .for_each!(|todo| echo!("- ${todo.name}\n"))

main! = |_args| {
    todos = table name, status {
        "Learn Roc",       Done,
        "Buy groceries",   Done,
        "Write blog post", InProgress,
        "Call mom",        NotStarted,
    }
    print_remaining!(todos)
    Ok({})
}
```

Columns can optionally be typed (`table name : Str, status { ... }`) so numeric
literals pick up the column type instead of defaulting to `Dec`. `table` is a
contextual keyword: `table(x)`, `table = …`, and `foo.table` stay ordinary
names.

See [Table literals](docs/langref/expressions.md#table-literals) in the language
reference.
