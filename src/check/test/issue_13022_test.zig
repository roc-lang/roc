//! Regression tests for https://github.com/roc-lang/roc/issues/13022.
//!
//! An alias of a nominal format type names that nominal type, so the format
//! methods that generated codecs call (`encode_record`, `parse_record_start`,
//! ...) resolve on the nominal type the alias names, exactly as they do when
//! the format is written through the nominal type's own name.
const TestEnv = @import("TestEnv.zig");

const internal_format_source =
    \\Internal := [].{
    \\    Format :: [Fields].{
    \\        rename_field : Format, Str -> Str
    \\        rename_field = |_, name| name
    \\
    \\        encode_str : Str, U64 -> Try(U64, [])
    \\        encode_str = |_text, written| Ok(written + 1)
    \\
    \\        encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)
    \\        encode_record = |written, _fields, write_fields| write_fields(written, |current, _name, write_value| write_value(current))
    \\    }
    \\
    \\    count : a -> U64 where [a.encoder_for : Format -> (a, U64 -> Try(U64, []))]
    \\    count = |value| {
    \\        Shape : a
    \\        encode = Shape.encoder_for(Format.Fields)
    \\        match encode(value, 0) {
    \\            Ok(total) => total
    \\        }
    \\    }
    \\}
;

test "issue 13022: record encoder resolves encode_record through an imported alias of the format" {
    var internal = try TestEnv.init("Internal", internal_format_source);
    defer internal.deinit();
    try internal.assertNoErrors();

    var public = try TestEnv.initWithImport("Public",
        \\import Internal
        \\
        \\Public := [].{
        \\    Format : Internal.Format
        \\
        \\    count : a -> U64 where [a.encoder_for : Format -> (a, U64 -> Try(U64, []))]
        \\    count = |value| Internal.count(value)
        \\}
        \\
        \\total : U64
        \\total = Public.count({ message: "hello" })
    , "Internal", &internal);
    defer public.deinit();
    try public.assertNoErrors();
}

test "issue 13022: record parser resolves parse_record_start through a local alias of the format" {
    var env = try TestEnv.init("FormatAlias",
        \\FormatAlias :: [].{
        \\    Format : FormatValue
        \\}
        \\
        \\FormatValue :: {}.{
        \\    rename_field : FormatAlias.Format, Str -> Str
        \\    rename_field = |_, field_name| field_name
        \\
        \\    missing_field : FormatAlias.Format, Str -> [Bad, MissingRequiredField(Str)]
        \\    missing_field = |_, field_name| MissingRequiredField(field_name)
        \\
        \\    parse_str : FormatAlias.Format, U64 -> Try({ value : Str, rest : U64 }, [Bad, MissingRequiredField(Str)])
        \\    parse_str = |_, position| Ok({ value: "read", rest: position })
        \\
        \\    parse_record_start : FormatAlias.Format, U64 -> Try([Counted({ len : U64, rest : U64 }), Uncounted(U64)], [Bad, MissingRequiredField(Str)])
        \\    parse_record_start = |_, _| Err(Bad)
        \\
        \\    parse_record_field : FormatAlias.Format, Encoding.FieldName.FieldNames(shape), U64 -> Try([Field({ field : Encoding.FieldName(shape), rest : U64 }), TryField({ name : Str, rest : U64 }), TryFieldCaseless({ name : Str, rest : U64 }), Continue(U64), Done(U64)], [Bad, MissingRequiredField(Str)])
        \\    parse_record_field = |_, _, position| Ok(Done(position))
        \\
        \\    parse_record_after_field : FormatAlias.Format, U64 -> Try([Continue(U64), Done(U64)], [Bad, MissingRequiredField(Str)])
        \\    parse_record_after_field = |_, position| Ok(Done(position))
        \\
        \\    skip_record_field : FormatAlias.Format, U64 -> Try(U64, [Bad, MissingRequiredField(Str)])
        \\    skip_record_field = |_, position| Ok(position)
        \\
        \\    invalid_value : FormatAlias.Format, U64 -> [Bad, MissingRequiredField(Str)]
        \\    invalid_value = |_, _| Bad
        \\}
        \\
        \\expect {
        \\    Item : { name : Str }
        \\    parse_item = Item.parser_for(FormatAlias.Format.{})
        \\    parse_item(0.U64) == Err(Bad)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13022: a format reached through an alias still needs the format method on its nominal type" {
    var env = try TestEnv.init("Test",
        \\Test :: [].{
        \\    Format : FormatValue
        \\}
        \\
        \\FormatValue :: [Fields].{
        \\    rename_field : Test.Format, Str -> Str
        \\    rename_field = |_, name| name
        \\
        \\    encode_str : Str, U64 -> Try(U64, [])
        \\    encode_str = |_text, written| Ok(written + 1)
        \\}
        \\
        \\count : a -> U64 where [a.encoder_for : Test.Format -> (a, U64 -> Try(U64, []))]
        \\count = |value| {
        \\    Shape : a
        \\    encode = Shape.encoder_for(FormatValue.Fields)
        \\    match encode(value, 0) {
        \\        Ok(total) => total
        \\    }
        \\}
        \\
        \\total : U64
        \\total = count({ message: "hello" })
    );
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "issue 13022: an alias of a nominal type with its own encoder_for encodes through a generic helper" {
    var env = try TestEnv.init("Test",
        \\Kind := [Basic].{
        \\    encoder_for : encoding -> (Kind, state -> Try(state, err))
        \\    encoder_for = |_encoding| |_kind, state| Ok(state)
        \\}
        \\
        \\K : Kind
        \\
        \\wrap = |value| Json.to_str(value)
        \\
        \\direct : K -> Str
        \\direct = |kind| Json.to_str(kind)
        \\
        \\through_helper : K -> Str
        \\through_helper = |kind| wrap(kind)
        \\
        \\in_record : K -> Str
        \\in_record = |kind| Json.to_str({ kind })
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13022: methods dispatch through an alias of a nominal type and an alias of a record" {
    var env = try TestEnv.init("Test",
        \\Named := [V].{
        \\    name : Named -> Str
        \\    name = |_| "v"
        \\}
        \\
        \\N : Named
        \\
        \\Rec : { n : U64 }
        \\
        \\describe : a -> Str where [a.name : a -> Str]
        \\describe = |value| value.name()
        \\
        \\through_generic : N -> Str
        \\through_generic = |named| describe(named)
        \\
        \\direct : N -> Str
        \\direct = |named| named.name()
        \\
        \\encode_record : Rec -> Str
        \\encode_record = |record| Json.to_str(record)
        \\
        \\same : Rec -> Bool
        \\same = |record| record == record
    );
    defer env.deinit();
    try env.assertNoErrors();
}
