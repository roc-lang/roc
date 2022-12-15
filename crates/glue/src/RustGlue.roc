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

                TagUnion (Enumeration { name, tags, size }) ->
                    generateEnumeration buf types type name tags size

                TagUnion (NonRecursive { name, tags, discriminantSize, discriminantOffset }) ->
                    if !(List.isEmpty tags) then
                        generateTagUnion buf types id name tags discriminantSize discriminantOffset NonRecursive None
                    else
                        buf

                TagUnion (Recursive { name, tags, discriminantSize, discriminantOffset }) ->
                    if !(List.isEmpty tags) then
                        generateTagUnion buf types id name tags discriminantSize discriminantOffset Recursive None
                    else
                        buf

                TagUnion (NullableWrapped { name, indexOfNullTag, tags, discriminantSize, discriminantOffset }) ->
                    generateTagUnion buf types id name tags discriminantSize discriminantOffset Recursive (Some indexOfNullTag)

                TagUnion (NullableUnwrapped { name, nullTag, nonNullTag, nonNullPayload, whichTagIsNull }) ->
                    generateNullableUnwrapped buf types id name nullTag nonNullTag nonNullPayload whichTagIsNull

                TagUnion (SingleTagStruct { name, tagName, payloadFields }) ->
                    generateSingleTagStruct buf types name tagName payloadFields

                TagUnion (NonNullableUnwrapped { name, tagName, payload }) ->
                    generateTagUnion buf types id name [{ name: tagName, payload: Some payload }] 0 0 Recursive None

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
    |> generateDeriveStr types structType IncludeDebug
    |> Str.concat "#[repr(\(repr))]\n\(pub) struct \(escapedName) {\n"
    |> \b -> List.walk fields b (generateStructFields types Public)
    |> Str.concat "}\n\n"

generateStructFields = \types, visibility ->
    \accum, { name: fieldName, id } ->
        typeStr = typeName types id
        escapedFieldName = escapeKW fieldName

        pub =
            when visibility is
                Public -> "pub"
                Private -> ""

        Str.concat accum "\(indent)\(pub) \(escapedFieldName): \(typeStr),\n"

nameTagUnionPayloadFields = \fields ->
    # Tag union payloads have numbered fields, so we prefix them
    # with an "f" because Rust doesn't allow struct fields to be numbers.
    List.map fields \{ discriminant, id } ->
        discStr = Num.toStr discriminant

        { name: "f\(discStr)", id }

generateEnumeration = \buf, types, enumType, name, tags, tagBytes ->
    escapedName = escapeKW name

    reprBits = tagBytes * 8 |> Num.toStr

    buf
    |> generateDeriveStr types enumType ExcludeDebug
    |> Str.concat "#[repr(u\(reprBits))]\npub enum \(escapedName) {\n"
    |> \b -> walkWithIndex tags b generateEnumTags
    |>
    # Enums require a custom debug impl to ensure naming is identical on all platforms.
    Str.concat
        """
        }

        impl core::fmt::Debug for \(escapedName) {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                match self {
        
        """
    |> \b -> List.walk tags b (generateEnumTagsDebug name)
    |> Str.concat "\(indent)\(indent)}\n\(indent)}\n}\n\n"

generateEnumTags = \accum, index, name ->
    indexStr = Num.toStr index

    Str.concat accum "\(indent)\(name) = \(indexStr),\n"

generateEnumTagsDebug = \name ->
    \accum, tagName ->
        Str.concat accum "\(indent)\(indent)\(indent)Self::\(tagName) => f.write_str(\"\(name)::\(tagName)\"),\n"

generateTagUnion = \buf, _types, _id, _name, _tags, _discriminantSize, _discriminantOffset, _recursiveness, _nullTagIndex ->
    Str.concat buf "// TODO: TagUnion\n\n"

generateNullableUnwrapped = \buf, _types, _id, _name, _nullTag, _nonNullTag, _nonNullPayload, _whichTagIsNull ->
    Str.concat buf "// TODO: TagUnion NullableUnwrapped\n\n"

generateSingleTagStruct = \buf, types, name, tagName, payloadFields ->
    # Store single-tag unions as structs rather than enums,
    # because they have only one alternative. However, still
    # offer the usual tag union APIs.
    escapedName = escapeKW name
    repr =
        if List.len payloadFields <= 1 then
            "transparent"
        else
            "C"

    asStructFields =
        List.mapWithIndex payloadFields \id, index ->
            indexStr = Num.toStr index

            { name: "f\(indexStr)", id }
    asStructType =
        Struct {
            name,
            fields: asStructFields,
        }

    buf
    |> generateDeriveStr types asStructType ExcludeDebug
    |> Str.concat "#[repr(\(repr))]\npub struct \(escapedName) "
    |> \b ->
        if List.isEmpty payloadFields then
            generateZeroElementSingleTagStruct b name tagName
        else
            generateMultiElementSingleTagStruct b types name tagName payloadFields asStructFields

generateMultiElementSingleTagStruct = \buf, types, name, tagName, payloadFields, asStructFields ->
    buf
    |> Str.concat "{\n"
    |> \b -> List.walk asStructFields b (generateStructFields types Private)
    |> Str.concat "}\n\n"
    |> Str.concat
        """
        impl \(name) {
        
        """
    |> \b ->
        fieldTypes =
            payloadFields
            |> List.map \id ->
                typeName types id
        args =
            fieldTypes
            |> List.mapWithIndex \fieldTypeName, index ->
                indexStr = Num.toStr index

                "f\(indexStr): \(fieldTypeName)"
        fields =
            payloadFields
            |> List.mapWithIndex \_, index ->
                indexStr = Num.toStr index

                "f\(indexStr),"

        fieldAccesses =
            fields
            |> List.map \field ->
                "self.\(field)"

        {
            b,
            args,
            fields,
            fieldTypes,
            fieldAccesses,
        }
    |> \{ b, args, fields, fieldTypes, fieldAccesses } ->
        argsStr = Str.joinWith args ", "
        fieldsStr = Str.joinWith fields "\n\(indent)\(indent)\(indent)"

        {
            b: Str.concat
                b
                """
                \(indent)/// A tag named ``\(tagName)``, with the given payload.
                \(indent)pub fn \(tagName)(\(argsStr)) -> Self {
                \(indent)    Self {
                \(indent)        \(fieldsStr)
                \(indent)    }
                \(indent)}

                
                """,
            fieldTypes,
            fieldAccesses,
        }
    |> \{ b, fieldTypes, fieldAccesses } ->
        retType = asRustTuple fieldTypes
        retExpr = asRustTuple fieldAccesses

        {
            b: Str.concat
                b
                """
                \(indent)/// Since `\(name)` only has one tag (namely, `\(tagName)`),
                \(indent)/// convert it to `\(tagName)`'s payload.
                \(indent)pub fn into_\(tagName)(self) -> \(retType) {
                \(indent)    \(retExpr)
                \(indent)}

                 
                """,
            fieldTypes,
            fieldAccesses,
        }
    |> \{ b, fieldTypes, fieldAccesses } ->
        retType =
            fieldTypes
            |> List.map \ft -> "&\(ft)"
            |> asRustTuple
        retExpr =
            fieldAccesses
            |> List.map \fa -> "&\(fa)"
            |> asRustTuple

        Str.concat
            b
            """
            \(indent)/// Since `\(name)` only has one tag (namely, `\(tagName)`),
            \(indent)/// convert it to `\(tagName)`'s payload.
            \(indent)pub fn as_\(tagName)(self) -> \(retType) {
            \(indent)    \(retExpr)
            \(indent)}
            
            """
    |> Str.concat
        """
        }


        impl core::fmt::Dbg for \(name) {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.debug_tuple("\(name)::\(tagName)")
        
        """
    |> \b ->
        payloadFields
        |> List.mapWithIndex \_, index ->
            indexStr = Num.toStr index

            "\(indent)\(indent)\(indent)\(indent).field(&self.f\(indexStr))\n"
        |> List.walk b Str.concat
    |> Str.concat
        """
                        .finish()
            }
        }

        
        """

asRustTuple = \list ->
    # If there is 1 element in the list we just return it
    # Otherwise, we make a proper tuple string.
    joined = Str.joinWith list ", "

    if List.len list == 1 then
        joined
    else
        "(\(joined))"

generateZeroElementSingleTagStruct = \buf, name, tagName ->
    # A single tag with no payload is a zero-sized unit type, so
    # represent it as a zero-sized struct (e.g. "struct Foo()").
    buf
    |> Str.concat "();\n\n"
    |> Str.concat
        """
        impl \(name) {
            /// A tag named \(tagName), which has no payload.
            pub const \(tagName): Self = Self();

            /// Other `into_` methods return a payload, but since \(tagName) tag
            /// has no payload, this does nothing and is only here for completeness.
            pub fn into_\(tagName)(self) {
                ()
            }

            /// Other `as_` methods return a payload, but since \(tagName) tag
            /// has no payload, this does nothing and is only here for completeness.
            pub fn as_\(tagName)(&self) {
                ()
            }
        }

        impl core::fmt::Dbg for \(name) {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.write_str("\(name)::\(tagName)")
            }
        }

        
        """

generateDeriveStr = \buf, types, type, includeDebug ->
    buf
    |> Str.concat "#[derive(Clone, "
    |> \b ->
        if !(cannotDeriveCopy types type) then
            Str.concat b "Copy, "
        else
            b
    |> \b ->
        if !(cannotDeriveDefault types type) then
            Str.concat b "Default, "
        else
            b
    |> \b ->
        when includeDebug is
            IncludeDebug ->
                Str.concat b "Debug, "

            ExcludeDebug ->
                b
    |> \b ->
        if !(hasFloat types type) then
            Str.concat b "Eq, Ord, Hash, "
        else
            b
    |> Str.concat "PartialEq, PartialOrd)]\n"

cannotDeriveCopy = \types, type ->
    when type is
        Unit | EmptyTagUnion | Bool | Num _ | TagUnion (Enumeration _) | Function _ -> Bool.false
        RocStr | RocList _ | RocDict _ _ | RocSet _ | RocBox _ | TagUnion (NullableUnwrapped _) | TagUnion (NullableWrapped _) | TagUnion (Recursive _) | TagUnion (NonNullableUnwrapped _) | RecursivePointer _ -> Bool.true
        TagUnion (SingleTagStruct { payloadFields }) ->
            List.any payloadFields \id -> cannotDeriveCopy types (getType types id)

        TagUnion (NonRecursive { tags }) ->
            List.any tags \{ payload } ->
                when payload is
                    Some id -> cannotDeriveCopy types (getType types id)
                    None -> Bool.false

        RocResult okId errId ->
            cannotDeriveCopy types (getType types okId)
            || cannotDeriveCopy types (getType types errId)

        Struct { fields } ->
            List.any fields \{ id } -> cannotDeriveCopy types (getType types id)

        TagUnionPayload { fields } ->
            List.any fields \{ id } -> cannotDeriveCopy types (getType types id)

cannotDeriveDefault = \types, type ->
    when type is
        Unit | EmptyTagUnion | TagUnion _ | RocResult _ _ | RecursivePointer _ | Function _ -> Bool.true
        RocStr | Bool | Num _ -> Bool.false
        RocList id | RocSet id | RocBox id ->
            cannotDeriveDefault types (getType types id)

        RocDict keyId valId ->
            cannotDeriveCopy types (getType types keyId)
            || cannotDeriveCopy types (getType types valId)

        Struct { fields } ->
            List.any fields \{ id } -> cannotDeriveDefault types (getType types id)

        TagUnionPayload { fields } ->
            List.any fields \{ id } -> cannotDeriveDefault types (getType types id)

hasFloat = \types, type ->
    hasFloatHelp types type Set.empty

hasFloatHelp = \types, type, doNotRecurse ->
    # TODO: is doNotRecurse problematic? Do we need an updated doNotRecurse for calls up the tree?
    # I think there is a change it really only matters for RecursivePointer, so it may be fine.
    # Otherwise we need to deal with threading through updates to doNotRecurse
    when type is
        Num kind ->
            when kind is
                F32 | F64 | F128 -> Bool.true
                _ -> Bool.false

        Unit | EmptyTagUnion | RocStr | Bool | TagUnion (Enumeration _) | Function _ -> Bool.false
        RocList id | RocSet id | RocBox id ->
            hasFloatHelp types (getType types id) doNotRecurse

        RocDict id0 id1 | RocResult id0 id1 ->
            hasFloatHelp types (getType types id0) doNotRecurse
            || hasFloatHelp types (getType types id1) doNotRecurse

        Struct { fields } ->
            List.any fields \{ id } -> hasFloatHelp types (getType types id) doNotRecurse

        TagUnionPayload { fields } ->
            List.any fields \{ id } -> hasFloatHelp types (getType types id) doNotRecurse

        TagUnion (SingleTagStruct { payloadFields }) ->
            List.any payloadFields \id -> hasFloatHelp types (getType types id) doNotRecurse

        TagUnion (Recursive { tags }) ->
            List.any tags \{ payload } ->
                when payload is
                    Some id -> hasFloatHelp types (getType types id) doNotRecurse
                    None -> Bool.false

        TagUnion (NonRecursive { tags }) ->
            List.any tags \{ payload } ->
                when payload is
                    Some id -> hasFloatHelp types (getType types id) doNotRecurse
                    None -> Bool.false

        TagUnion (NullableWrapped { tags }) ->
            List.any tags \{ payload } ->
                when payload is
                    Some id -> hasFloatHelp types (getType types id) doNotRecurse
                    None -> Bool.false

        TagUnion (NonNullableUnwrapped { payload }) ->
            if Set.contains doNotRecurse payload then
                Bool.false
            else
                nextDoNotRecurse = Set.insert doNotRecurse payload

                hasFloatHelp types (getType types payload) nextDoNotRecurse

        TagUnion (NullableUnwrapped { nonNullPayload }) ->
            if Set.contains doNotRecurse nonNullPayload then
                Bool.false
            else
                nextDoNotRecurse = Set.insert doNotRecurse nonNullPayload

                hasFloatHelp types (getType types nonNullPayload) nextDoNotRecurse

        RecursivePointer payload ->
            if Set.contains doNotRecurse payload then
                Bool.false
            else
                nextDoNotRecurse = Set.insert doNotRecurse payload

                hasFloatHelp types (getType types payload) nextDoNotRecurse

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
