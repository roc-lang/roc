app "rust-glue"
    packages { pf: "RocType.roc" }
    imports []
    provides [makeGlue] to pf

makeGlue = \types ->
    modFileContent =
        List.walk types "" \content, { target } ->
            archStr = archName target.architecture

            Str.concat
                content
                """
                #[cfg(target_arch = "\(archStr)")]
                mod \(archStr);
                #[cfg(target_arch = "\(archStr)")]
                pub use \(archStr)::*;
                
                """

    types
    |> List.map typesWithDict
    |> List.map convertTypesToFile
    |> List.append { name: "mod.rs", content: modFileContent }
    |> Ok

convertTypesToFile = \types ->
    content =
        walkWithIndex types.types fileHeader \buf, id, type ->
            when type is
                Struct { name, fields } ->
                    generateStruct buf types id name fields Public

                TagUnionPayload { name, fields } ->
                    generateStruct buf types id name (nameTagUnionPayloadFields fields) Private

                TagUnion _ ->
                    # TODO: tag union impl.
                    buf

                Function _ ->
                    # TODO: actually generate glue functions.
                    buf

                RecursivePointer _ ->
                    # This is recursively pointing to a type that should already have been added,
                    # so no extra work needs to happen.
                    buf

                Unit
                | EmptyTagUnion
                | Num _
                | Bool
                | RocResult _ _
                | RocStr
                | RocDict _ _
                | RocSet _
                | RocList _
                | RocBox _ ->
                    # These types don't need to be declared in Rust.
                    # TODO: Eventually we want to generate roc_std. So these types will need to be emitted.
                    buf
    archStr = archName types.target.architecture

    {
        name: "\(archStr).rs",
        content,
    }

generateStruct = \buf, types, id, name, fields, visibility ->
    escapedName = escapeKW name
    repr =
        if List.len fields == 1 then
            "transparent"
        else
            "C"
    pub =
        when visibility is
            Public -> "pub"
            Private -> ""

    structType = getType types id

    buf
    |> addDeriveStr types structType IncludeDebug
    |> Str.concat "#[repr(\(repr))]\n\(pub) struct \(escapedName) {\n"
    |> \b -> List.walk fields b (generateStructFields types)
    |> Str.concat "}\n\n"

generateStructFields = \types ->
    \accum, { name: fieldName, id } ->
        typeStr = typeName types id
        escapedFieldName = escapeKW fieldName

        Str.concat accum "\(indent)pub \(escapedFieldName): \(typeStr),\n"

nameTagUnionPayloadFields = \fields ->
    # Tag union payloads have numbered fields, so we prefix them
    # with an "f" because Rust doesn't allow struct fields to be numbers.
    List.map fields \{ discriminant, id } ->
        discStr = Num.toStr discriminant

        { name: "f\(discStr)", id }

addDeriveStr = \buf, types, type, includeDebug ->
    # TODO: full derive impl porting.
    buf
    |> Str.concat "#[derive(Clone, "
    |> \b ->
        when includeDebug is
            IncludeDebug ->
                Str.concat b "Debug, "

            ExcludeDebug ->
                b
    |> \b ->
        if !(cannotDeriveCopy types type) then
            Str.concat b "Copy, "
        else
            b
    |> Str.concat "PartialEq, PartialOrd)]\n"

cannotDeriveCopy = \types, type ->
    when type is
        Unit | EmptyTagUnion | Bool | Num _ | TagUnion (Enumeration _) | Function _ -> Bool.false
        RocStr | RocList _ | RocDict _ _ | RocSet _ | RocBox _ | TagUnion ( NullableUnwrapped _ ) | TagUnion ( NullableWrapped _ ) | TagUnion ( Recursive _ ) | TagUnion ( NonNullableUnwrapped _) | RecursivePointer _ -> Bool.true
        TagUnion (SingleTagStruct { payloadFields }) ->
             List.any payloadFields \id -> cannotDeriveCopy types (getType types id)
        TagUnion (NonRecursive {tags}) ->
            List.any tags \{payload} ->
                when payload is
                    Some id -> cannotDeriveCopy types (getType types id)
                    None -> Bool.false
        RocResult okId errId ->
            cannotDeriveCopy types (getType types okId)
            || cannotDeriveCopy types (getType types errId)
        Struct { fields} ->
             List.any fields \{ id } -> cannotDeriveCopy types (getType types id)
        TagUnionPayload { fields} ->
             List.any fields \{ id } -> cannotDeriveCopy types (getType types id)
        _ -> crash "ugh"


typeName = \types, id ->
    when getType types id is
        Unit -> "()"
        EmptyTagUnion -> "std::convert::Infallible"
        RocStr -> "roc_std::RocStr"
        Bool -> "bool"
        Num U8 -> "u8"
        Num U16 -> "u16"
        Num U32 -> "u32"
        Num U64 -> "u64"
        Num U128 -> "u128"
        Num I8 -> "i8"
        Num I16 -> "i16"
        Num I32 -> "i32"
        Num I64 -> "i64"
        Num I128 -> "i128"
        Num F32 -> "f32"
        Num F64 -> "f64"
        Num F128 -> "roc_std::f128"
        Num Dec -> "roc_std:RocDec"
        RocDict key value ->
            keyName = typeName types key
            valueName = typeName types value

            "roc_std::RocDict<\(keyName), \(valueName)>"

        RocSet elem ->
            elemName = typeName types elem

            "roc_std::RocSet<\(elemName)>"

        RocList elem ->
            elemName = typeName types elem

            "roc_std::RocList<\(elemName)>"

        RocBox elem ->
            elemName = typeName types elem

            "roc_std::RocBox<\(elemName)>"

        RocResult ok err ->
            okName = typeName types ok
            errName = typeName types err

            "roc_std::RocResult<\(okName), \(errName)>"

        RecursivePointer content ->
            typeName types content

        Struct { name } -> escapeKW name
        TagUnionPayload { name } -> escapeKW name
        TagUnion (NonRecursive { name }) -> escapeKW name
        TagUnion (Recursive { name }) -> escapeKW name
        TagUnion (Enumeration { name }) -> escapeKW name
        TagUnion (NullableWrapped { name }) -> escapeKW name
        TagUnion (NullableUnwrapped { name }) -> escapeKW name
        TagUnion (NonNullableUnwrapped { name }) -> escapeKW name
        TagUnion (SingleTagStruct { name }) -> escapeKW name
        Function { name } -> escapeKW name

getType = \types, id ->
    when List.get types.types id is
        Ok type -> type
        Err _ -> crash "unreachable"

walkWithIndex = \list, originalState, f ->
    stateWithId =
        List.walk list { id: 0nat, state: originalState } \{ id, state }, elem ->
            nextState = f state id elem

            { id: id + 1, state: nextState }

    stateWithId.state

archName = \arch ->
    when arch is
        Aarch32 ->
            "arm"

        Aarch64 ->
            "aarch64"

        Wasm32 ->
            "wasm32"

        X86x32 ->
            "x86"

        X86x64 ->
            "x86_64"

fileHeader =
"""
// ⚠️ GENERATED CODE ⚠️ - this entire file was generated by the `roc glue` CLI command

#![allow(unused_unsafe)]
#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]
#![allow(clippy::undocumented_unsafe_blocks)]
#![allow(clippy::redundant_static_lifetimes)]
#![allow(clippy::unused_unit)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::let_and_return)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::redundant_static_lifetimes)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::clone_on_copy)]



"""

indent = "    "

reservedKeywords = Set.fromList [
    "try",
    "abstract",
    "become",
    "box",
    "do",
    "final",
    "macro",
    "override",
    "priv",
    "typeof",
    "unsized",
    "virtual",
    "yield",
    "async",
    "await",
    "dyn",
    "as",
    "break",
    "const",
    "continue",
    "crate",
    "else",
    "enum",
    "extern",
    "false",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mod",
    "move",
    "mut",
    "pub",
    "ref",
    "return",
    "self",
    "Self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "unsafe",
    "use",
    "where",
    "while",
]

escapeKW = \input ->
    # use a raw identifier for this, to prevent a syntax error due to using a reserved keyword.
    # https://doc.rust-lang.org/rust-by-example/compatibility/raw_identifiers.html
    # another design would be to add an underscore after it; this is an experiment!
    if Set.contains reservedKeywords input then
        "r#\(input)"
    else
        input

# This is a temporary helper until roc_std::roc_dict is update.
# after that point, Dict will be passed in directly.
typesWithDict = \{ types, sizes, aligns, typesByName, deps, target } -> {
    types,
    sizes,
    aligns,
    typesByName: Dict.fromList typesByName,
    deps: Dict.fromList deps,
    target,
}
