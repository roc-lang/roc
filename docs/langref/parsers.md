# Parsers

Roc has a built-in system for parsing data (such as JSON) into Roc values, and for encoding Roc values
back into data. It's based on [static dispatch](static-dispatch), and it separates two concerns:

- The _format_ (such as JSON) knows how to read and write basic things like strings, numbers, lists, and records.
- The _type_ being parsed (such as `{ name : Str, age : U64 }`) knows which of those basic things it's made of.

This means any format can work with any type, without either one needing to know about the other in advance.

## Parsing JSON

The builtin `Json` module parses JSON into any type that supports it, which is determined by type inference:

```roc
parse_user : Str -> Try({ name : Str, age : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
parse_user = |json| Json.parse(json)

parse_user("{\"name\": \"Sam\", \"age\": 30}") # Ok({ name: "Sam", age: 30 })

parse_user("{\"name\": \"Sam\"}") # Err(MissingRequiredField("age"))

parse_user("{\"name\": 5}") # Err(InvalidJson("Invalid JSON"))
```

The parser is determined entirely by the type. Here, since `parse_user` returns a record with a `name : Str` field
and an `age : U64` field, `Json.parse` expects a JSON object with a string field named `"name"` and a numeric field
named `"age"`.

`Json.to_str` goes the other direction, encoding a value as JSON:

```roc
Json.to_str({ name: "Sam", age: 30.U64 }) # {"age":30,"name":"Sam"}
```

The `Json` module also has variations for different conventions. For example, `Json.parser_camel()` returns a
parser which expects JSON fields in `camelCase` and converts them to Roc's `snake_case` field names (so a Roc field
named `user_id` is read from a JSON field named `"userId"`), and `Json.parse_trailing_commas` allows trailing
commas in arrays and objects.

## Which Types Can Be Parsed

Parsing and encoding work on these types automatically, as long as all the types they contain can be parsed or encoded too:

- Strings, numbers, and `Bool`
- [Records](records)
- [Tuples](tuples)
- [Tag unions](tag-unions)
- Lists, [dictionaries, and sets](dictionaries-and-sets)

How each of these is represented depends on the format. For example, in JSON, a record is an object,
and a tag without a payload (like `Active`) is a string (like `"Active"`).

### Optional Fields

When a record field might be missing from the input, you can make it an [optional field](records#optional-fields),
or you can give it a `Try` type whose error is `[Missing]`:

```roc
parse_user : Str -> Try({ name : Str, nickname ?: Str, email : Try(Str, [Missing]) }, [InvalidJson(Str), MissingRequiredField(Str)])
parse_user = |json| Json.parse(json)

parse_user("{\"name\": \"Sam\"}") # Ok({ name: "Sam", nickname: <missing>, email: Err(Missing) })
```

Fields which are neither optional nor a `Try` of `[Missing]` are required, and it's an error
(`MissingRequiredField`) if they're missing from the input.

### Nominal Types

[Nominal types](types#nominal-types) aren't parseable or encodable automatically, because their authors may
not want them to be. (For example, an [opaque type](types#opaque-nominal-types) might have internal details
that shouldn't be exposed in encoded data, or invariants that parsing arbitrary data could violate.)

To opt into the compiler's derived parser and encoder, a nominal type declares `parser_for : _` and `encoder_for : _`:

```roc
Point := { x : I64, y : I64 }.{
    parser_for : _
    encoder_for : _
}
```

Now `Json.parse` and `Json.to_str` work with `Point`, using the same representation as its backing record.
Alternatively, a type can implement `parser_for` and `encoder_for` itself, to use a custom representation. See
[Parsing and Encoding](static-dispatch#parsing-and-encoding) for an example.

## Parsers at Compile Time

Building a parser for a type is done once per type, and when it happens in a
[top-level constant](naming#constants), it happens at [compile time](compile-time). For example:

```roc
parse_request : Str -> Try(Request, [InvalidJson(Str), MissingRequiredField(Str)])
parse_request = Json.parser_camel()
```

Here, all the work of assembling a parser specialized for the `Request` type, including converting its
field names to `camelCase`, happens during compilation. At runtime, the program just runs the finished parser.

## Writing Generic Parsing Functions

A function which works with any JSON-parseable type can express that requirement using the `Json.Parseable`
[where alias](static-dispatch#aliases). Similarly, `Json.Encodable` describes any type which can be encoded as JSON:

```roc
from_json : Str -> Try(a, [InvalidJson(Str), ..errs]) where [a.Json.Parseable([InvalidJson(Str), ..errs])]
from_json = |src| Json.parse(src)

to_json : a -> Str where [a.Json.Encodable([])]
to_json = |value| Json.to_str(value)
```

## Other Formats

JSON isn't special. Any format can work with any parseable type, as long as the format implements
the methods that the type's parser needs, such as `parse_str` for reading strings and `parse_record_start`
for reading records. A format only needs to implement methods for the shapes it supports; using a type
whose shape a format doesn't support (for example, a tuple, with a format that has no tuple syntax) is a
compile-time error rather than a runtime one.

For example, besides `Json`, Roc's builtins also include `Encoding.HttpHeader`, which parses HTTP headers
into records:

```roc
parse_headers : Str -> Try({ content_length : U64, x_auth_token : Try(Str, [Missing]) }, [BadHeader, MissingRequiredField(Str)])
parse_headers = Encoding.HttpHeader.parser_for()
```

Formats can be implemented in packages, too, without any changes to the types they parse.
