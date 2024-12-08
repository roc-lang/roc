app [makeGlue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.Shape exposing [Shape, RocFn]
import pf.File exposing [File]
import pf.TypeId exposing [TypeId]
import "../static/Cargo.toml" as rocAppCargoToml : Str
import "../../roc_std/Cargo.toml" as rocStdCargoToml : Str
import "../../roc_std/src/lib.rs" as rocStdLib : Str
import "../../roc_std/src/roc_box.rs" as rocStdBox : Str
import "../../roc_std/src/roc_list.rs" as rocStdList : Str
import "../../roc_std/src/roc_str.rs" as rocStdStr : Str
import "../../roc_std/src/storage.rs" as rocStdStorage : Str

makeGlue : List Types -> Result (List File) Str
makeGlue = \typesByArch ->
    modFileContent =
        List.walk typesByArch fileHeader \content, types ->
            arch = (Types.target types).architecture
            archStr = archName arch

            Str.concat
                content
                """
                #[cfg(target_arch = "$(archStr)")]
                mod $(archStr);
                #[cfg(target_arch = "$(archStr)")]
                pub use $(archStr)::*;

                """

    typesByArch
    |> List.map convertTypesToFile
    |> List.append { name: "roc_app/src/lib.rs", content: modFileContent }
    |> List.concat staticFiles
    |> Ok

## These are always included, and don't depend on the specifics of the app.
staticFiles : List File
staticFiles = [
    { name: "roc_app/Cargo.toml", content: rocAppCargoToml },
    { name: "roc_std/Cargo.toml", content: rocStdCargoToml },
    { name: "roc_std/src/lib.rs", content: rocStdLib },
    { name: "roc_std/src/roc_box.rs", content: rocStdBox },
    { name: "roc_std/src/roc_list.rs", content: rocStdList },
    { name: "roc_std/src/roc_str.rs", content: rocStdStr },
    { name: "roc_std/src/storage.rs", content: rocStdStorage },
]

convertTypesToFile : Types -> File
convertTypesToFile = \types ->
    content =
        Types.walkShapes types fileHeader \buf, type, id ->
            when type is
                Struct { name, fields } ->
                    generateStruct buf types id name fields Public

                TagUnionPayload { name, fields } ->
                    generateStruct buf types id name (nameTagUnionPayloadFields fields) Public

                TagUnion (Enumeration { name, tags, size }) ->
                    generateEnumeration buf types type name tags size

                TagUnion (NonRecursive { name, tags, discriminantSize, discriminantOffset }) ->
                    if !(List.isEmpty tags) then
                        generateNonRecursiveTagUnion buf types id name tags discriminantSize discriminantOffset
                    else
                        buf

                TagUnion (Recursive { name, tags, discriminantSize, discriminantOffset }) ->
                    if !(List.isEmpty tags) then
                        generateRecursiveTagUnion buf types id name tags discriminantSize discriminantOffset None
                    else
                        buf

                TagUnion (NullableWrapped { name, indexOfNullTag, tags, discriminantSize, discriminantOffset }) ->
                    # TODO: generate this as `TypeName(*mut u8)` if the payload contains functions / unsized types
                    generateRecursiveTagUnion buf types id name tags discriminantSize discriminantOffset (Some indexOfNullTag)

                TagUnion (NullableUnwrapped { name, nullTag, nonNullTag, nonNullPayload, whichTagIsNull }) ->
                    generateNullableUnwrapped buf types id name nullTag nonNullTag nonNullPayload whichTagIsNull

                TagUnion (SingleTagStruct { name, tagName, payload }) ->
                    generateSingleTagStruct buf types name tagName payload

                TagUnion (NonNullableUnwrapped { name, tagName, payload }) ->
                    generateNonNullableUnwrapped buf types name tagName payload 0 0 None

                Function rocFn ->
                    if rocFn.isToplevel then
                        buf
                    else
                        generateFunction buf types rocFn

                RecursivePointer _ ->
                    # This is recursively pointing to a type that should already have been added,
                    # so no extra work needs to happen.
                    buf

                Unit
                | Unsized
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

    arch = (Types.target types).architecture
    archStr = archName arch

    {
        name: "roc_app/src/$(archStr).rs",
        content: content |> generateEntryPoints types,
    }

generateEntryPoints : Str, Types -> Str
generateEntryPoints = \buf, types ->
    List.walk (Types.entryPoints types) buf \accum, T name id -> generateEntryPoint accum types name id

generateEntryPoint : Str, Types, Str, TypeId -> Str
generateEntryPoint = \buf, types, name, id ->
    publicSignature =
        when Types.shape types id is
            Function rocFn ->
                arguments =
                    toArgStr rocFn.args types \argId, _shape, index ->
                        type = typeName types argId
                        indexStr = Num.toStr index

                        "arg$(indexStr): $(type)"

                ret = typeName types rocFn.ret

                "($(arguments)) -> $(ret)"

            _ ->
                ret = typeName types id
                "() -> $(ret)"

    (externSignature, returnTypeName, returnsFn) =
        when Types.shape types id is
            Function rocFn ->
                arguments =
                    toArgStr rocFn.args types \argId, shape, _index ->
                        type = typeName types argId

                        if canDeriveCopy types shape then
                            "_: $(type)"
                        else
                            "_: &mut core::mem::ManuallyDrop<$(type)>"

                ret = typeName types rocFn.ret
                when Types.shape types rocFn.ret is
                    Function _ ->
                        ("(_: *mut u8, $(arguments))", ret, Bool.true)

                    _ ->
                        ("(_: *mut $(ret), $(arguments))", ret, Bool.false)

            _ ->
                ret = typeName types id
                ("(_: *mut $(ret))", ret, Bool.false)

    externArguments =
        when Types.shape types id is
            Function rocFn ->
                toArgStr rocFn.args types \_argId, shape, index ->
                    indexStr = Num.toStr index

                    if canDeriveCopy types shape then
                        "arg$(indexStr)"
                    else
                        "&mut core::mem::ManuallyDrop::new(arg$(indexStr))"

            _ ->
                ""

    if returnsFn then
        """
        $(buf)

        pub fn $(name)$(publicSignature) {
            extern "C" {
                fn roc__$(name)_1_exposed_generic$(externSignature);
                fn roc__$(name)_1_exposed_size() -> i64;
            }

            unsafe {
                let capacity = roc__$(name)_1_exposed_size() as usize;

                let mut ret = $(returnTypeName) {
                    closure_data: Vec::with_capacity(capacity),
                };
                ret.closure_data.resize(capacity, 0);

                roc__$(name)_1_exposed_generic(ret.closure_data.as_mut_ptr(), $(externArguments));

                ret
            }
        }
        """
    else
        """
        $(buf)

        pub fn $(name)$(publicSignature) {
            extern "C" {
                fn roc__$(name)_1_exposed_generic$(externSignature);
            }

            let mut ret = core::mem::MaybeUninit::uninit();

            unsafe {
                roc__$(name)_1_exposed_generic(ret.as_mut_ptr(), $(externArguments));

                ret.assume_init()
            }
        }
        """

generateFunction : Str, Types, RocFn -> Str
generateFunction = \buf, types, rocFn ->
    name = rocFn.functionName
    externName = rocFn.externName

    publicArguments =
        toArgStr rocFn.args types \argId, _shape, index ->
            type = typeName types argId
            indexStr = Num.toStr index

            "arg$(indexStr): $(type)"

    externDefArguments =
        withoutUnit =
            toArgStr rocFn.args types \argId, _shape, index ->
                type = typeName types argId
                indexStr = Num.toStr index

                "arg$(indexStr): *const $(type)"

        if Str.isEmpty withoutUnit then
            # These always have a first argument that's a pointer, even if it's to nothing.
            "arg0: *const ()"
        else
            withoutUnit

    externCallArguments =
        withoutUnit =
            toArgStr rocFn.args types \_argId, _shape, index ->
                indexStr = Num.toStr index

                "&arg$(indexStr)"

        if Str.isEmpty withoutUnit then
            # These always have a first argument that's a pointer, even if it's to nothing.
            "&()"
        else
            withoutUnit

    publicComma = if Str.isEmpty publicArguments then "" else ", "

    ret = typeName types rocFn.ret

    """
    $(buf)

    #[repr(C)]
    #[derive(Debug)]
    pub struct $(name) {
        closure_data: Vec<u8>,
    }

    impl $(name) {
        pub fn force_thunk(mut self$(publicComma)$(publicArguments)) -> $(ret) {
            extern "C" {
                fn $(externName)($(externDefArguments), closure_data: *mut u8, output: *mut $(ret));
            }

            let mut output = core::mem::MaybeUninit::uninit();

            unsafe {
                $(externName)($(externCallArguments), self.closure_data.as_mut_ptr(), output.as_mut_ptr());

                output.assume_init()
            }
        }
    }
    """
    |> generateRocRefcounted types (Function rocFn) name

generateStruct : Str, Types, TypeId, _, _, _ -> Str
generateStruct = \buf, types, id, name, structFields, visibility ->
    escapedName = escapeKW name
    repr =
        length =
            when structFields is
                HasClosure fields -> List.len fields
                HasNoClosure fields -> List.len fields
        if length <= 1 then
            "transparent"
        else
            "C"

    pub =
        when visibility is
            Public -> "pub "
            Private -> ""

    structType = Types.shape types id

    buf
    |> generateDeriveStr types structType IncludeDebug
    |> Str.concat "#[repr($(repr))]\n$(pub)struct $(escapedName) {\n"
    |> generateStructFields types Public structFields
    |> Str.concat "}\n\n"
    |> generateRocRefcounted types structType escapedName

generateStructFields = \buf, types, visibility, structFields ->
    when structFields is
        HasNoClosure fields ->
            List.walk fields buf (generateStructFieldWithoutClosure types visibility)

        HasClosure fields ->
            List.walk fields buf (generateStructFieldWithoutClosure types visibility)

generateStructFieldWithoutClosure = \types, visibility ->
    \accum, { name: fieldName, id } ->
        typeStr = typeName types id
        escapedFieldName = escapeKW fieldName

        pub =
            when visibility is
                Public -> "pub"
                Private -> ""

        Str.concat accum "$(indent)$(pub) $(escapedFieldName): $(typeStr),\n"

nameTagUnionPayloadFields = \payloadFields ->
    # Tag union payloads have numbered fields, so we prefix them
    # with an "f" because Rust doesn't allow struct fields to be numbers.
    when payloadFields is
        HasNoClosure fields ->
            renamedFields = List.map fields \{ name, id } -> { name: "f$(name)", id }
            HasNoClosure renamedFields

        HasClosure fields ->
            renamedFields = List.map fields \{ name, id, accessors } -> { name: "f$(name)", id, accessors }
            HasClosure renamedFields

generateEnumeration = \buf, types, enumType, name, tags, tagBytes ->
    escapedName = escapeKW name

    reprBits = tagBytes * 8 |> Num.toStr

    buf
    |> generateDeriveStr types enumType ExcludeDebug
    |> Str.concat "#[repr(u$(reprBits))]\npub enum $(escapedName) {\n"
    |> \b -> List.walkWithIndex tags b generateEnumTags
    |>
    # Enums require a custom debug impl to ensure naming is identical on all platforms.
    Str.concat
        """
        }

        impl core::fmt::Debug for $(escapedName) {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                match self {

        """
    |> \b -> List.walk tags b (generateEnumTagsDebug name)
    |> Str.concat "$(indent)$(indent)}\n$(indent)}\n}\n\n"
    |> generateRocRefcounted types enumType escapedName

generateEnumTags = \accum, name, index ->
    indexStr = Num.toStr index

    Str.concat accum "$(indent)$(name) = $(indexStr),\n"

generateEnumTagsDebug = \name ->
    \accum, tagName ->
        Str.concat accum "$(indent)$(indent)$(indent)Self::$(tagName) => f.write_str(\"$(name)::$(tagName)\"),\n"

deriveCloneTagUnion : Str, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
deriveCloneTagUnion = \buf, tagUnionType, tags ->
    clones =
        List.walk tags "" \accum, { name: tagName } ->
            """
            $(accum)
                            $(tagName) => union_$(tagUnionType) {
                                $(tagName): self.payload.$(tagName).clone(),
                            },
            """

    """
    $(buf)

    impl Clone for $(tagUnionType) {
        fn clone(&self) -> Self {
            use discriminant_$(tagUnionType)::*;

            let payload = unsafe {
                match self.discriminant {$(clones)
                }
            };

            Self {
                discriminant: self.discriminant,
                payload,
            }
        }
    }
    """

deriveDebugTagUnion : Str, Types, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
deriveDebugTagUnion = \buf, types, tagUnionType, tags ->
    checks =
        List.walk tags "" \accum, { name: tagName, payload } ->
            type =
                when payload is
                    Some id -> typeName types id
                    None -> "()"

            """
            $(accum)
                            $(tagName) => {
                                let field: &$(type) = &self.payload.$(tagName);
                                f.debug_tuple("$(tagUnionType)::$(tagName)").field(field).finish()
                            },
            """

    """
    $(buf)

    impl core::fmt::Debug for $(tagUnionType) {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            use discriminant_$(tagUnionType)::*;

            unsafe {
                match self.discriminant {$(checks)
                }
            }
        }
    }
    """

deriveEqTagUnion : Str, Types, Shape, Str -> Str
deriveEqTagUnion = \buf, types, shape, tagUnionType ->
    if canSupportEqHashOrd types shape then
        """
        $(buf)

        impl Eq for $(tagUnionType) {}
        """
    else
        buf

derivePartialEqTagUnion : Str, Types, Shape, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
derivePartialEqTagUnion = \buf, types, shape, tagUnionType, tags ->
    if canSupportPartialEqOrd types shape then
        checks =
            List.walk tags "" \accum, { name: tagName } ->
                """
                $(accum)
                                $(tagName) => self.payload.$(tagName) == other.payload.$(tagName),
                """

        """
        $(buf)

        impl PartialEq for $(tagUnionType) {
            fn eq(&self, other: &Self) -> bool {
                use discriminant_$(tagUnionType)::*;

                if self.discriminant != other.discriminant {
                    return false;
                }

                unsafe {
                    match self.discriminant {$(checks)
                    }
                }
            }
        }
        """
    else
        buf

deriveOrdTagUnion : Str, Types, Shape, Str -> Str
deriveOrdTagUnion = \buf, types, shape, tagUnionType ->
    if canSupportEqHashOrd types shape then
        """
        $(buf)

        impl Ord for $(tagUnionType) {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.partial_cmp(other).unwrap()
            }
        }
        """
    else
        buf

derivePartialOrdTagUnion : Str, Types, Shape, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
derivePartialOrdTagUnion = \buf, types, shape, tagUnionType, tags ->
    if canSupportPartialEqOrd types shape then
        checks =
            List.walk tags "" \accum, { name: tagName } ->
                """
                $(accum)
                                    $(tagName) => self.payload.$(tagName).partial_cmp(&other.payload.$(tagName)),
                """

        """
        $(buf)

        impl PartialOrd for $(tagUnionType) {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                use discriminant_$(tagUnionType)::*;

                use std::cmp::Ordering::*;

                match self.discriminant.cmp(&other.discriminant) {
                    Less => Option::Some(Less),
                    Greater => Option::Some(Greater),
                    Equal => unsafe {
                        match self.discriminant {$(checks)
                        }
                    },
                }
            }
        }
        """
    else
        buf

deriveHashTagUnion : Str, Types, Shape, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
deriveHashTagUnion = \buf, types, shape, tagUnionType, tags ->
    if canSupportEqHashOrd types shape then
        checks =
            List.walk tags "" \accum, { name: tagName } ->
                """
                $(accum)
                                $(tagName) => self.payload.$(tagName).hash(state),
                """

        """
        $(buf)

        impl core::hash::Hash for $(tagUnionType) {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                use discriminant_$(tagUnionType)::*;

                unsafe {
                    match self.discriminant {$(checks)
                    }
                }
            }
        }
        """
    else
        buf

generateConstructorFunctions : Str, Types, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
generateConstructorFunctions = \buf, types, tagUnionType, tags ->
    buf
    |> Str.concat "\n\nimpl $(tagUnionType) {"
    |> \b -> List.walk tags b \accum, r -> generateConstructorFunction accum types tagUnionType r.name r.payload
    |> Str.concat "\n}\n\n"

generateConstructorFunction : Str, Types, Str, Str, [Some TypeId, None] -> Str
generateConstructorFunction = \buf, types, tagUnionType, name, optPayload ->
    when optPayload is
        None ->
            """
            $(buf)

                pub fn $(name)() -> Self {
                    Self {
                        discriminant: discriminant_$(tagUnionType)::$(name),
                        payload: union_$(tagUnionType) {
                            $(name): (),
                        }
                    }
                }
            """

        Some payloadId ->
            payloadType = typeName types payloadId
            shape = Types.shape types payloadId

            new =
                if canDeriveCopy types shape then
                    "payload"
                else
                    "core::mem::ManuallyDrop::new(payload)"

            """
            $(buf)

                pub fn $(name)(payload: $(payloadType)) -> Self {
                    Self {
                        discriminant: discriminant_$(tagUnionType)::$(name),
                        payload: union_$(tagUnionType) {
                            $(name): $(new),
                        }
                    }
                }
            """

generateDestructorFunctions : Str, Types, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
generateDestructorFunctions = \buf, types, tagUnionType, tags ->
    buf
    |> Str.concat "\n\nimpl $(tagUnionType) {"
    |> \b -> List.walk tags b \accum, r -> generateDestructorFunction accum types tagUnionType r.name r.payload
    |> Str.concat "\n}\n\n"

generateDestructorFunction : Str, Types, Str, Str, [Some TypeId, None] -> Str
generateDestructorFunction = \buf, types, tagUnionType, name, optPayload ->
    when optPayload is
        None ->
            """
            $(buf)

                pub fn is_$(name)(&self) -> bool {
                    matches!(self.discriminant, discriminant_$(tagUnionType)::$(name))
                }
            """

        Some payloadId ->
            payloadType = typeName types payloadId
            shape = Types.shape types payloadId

            take =
                if canDeriveCopy types shape then
                    "unsafe { self.payload.$(name) }"
                else
                    "unsafe { core::mem::ManuallyDrop::take(&mut self.payload.$(name)) }"

            (borrow, borrowType) =
                if canDeriveCopy types shape then
                    ("unsafe { self.payload.$(name) }", payloadType)
                else
                    (
                        """
                        use core::borrow::Borrow;
                        unsafe { self.payload.$(name).borrow() }
                        """,
                        "&$(payloadType)",
                    )

            (borrowMut, borrowMutType) =
                if canDeriveCopy types shape then
                    ("unsafe { &mut self.payload.$(name) }", "&mut $(payloadType)")
                else
                    (
                        """
                        use core::borrow::BorrowMut;
                        unsafe { self.payload.$(name).borrow_mut() }
                        """,
                        "&mut $(payloadType)",
                    )

            """
            $(buf)

                pub fn unwrap_$(name)(mut self) -> $(payloadType) {
                    debug_assert_eq!(self.discriminant, discriminant_$(tagUnionType)::$(name));
                    $(take)
                }

                pub fn borrow_$(name)(&self) -> $(borrowType) {
                    debug_assert_eq!(self.discriminant, discriminant_$(tagUnionType)::$(name));
                    $(borrow)
                }

                pub fn borrow_mut_$(name)(&mut self) -> $(borrowMutType) {
                    debug_assert_eq!(self.discriminant, discriminant_$(tagUnionType)::$(name));
                    $(borrowMut)
                }

                pub fn is_$(name)(&self) -> bool {
                    matches!(self.discriminant, discriminant_$(tagUnionType)::$(name))
                }
            """

generateNonRecursiveTagUnion : Str, Types, TypeId, Str, List { name : Str, payload : [Some TypeId, None] }, U32, U32 -> Str
generateNonRecursiveTagUnion = \buf, types, id, name, tags, discriminantSize, discriminantOffset ->
    escapedName = escapeKW name
    discriminantName = "discriminant_$(escapedName)"
    unionName = "union_$(escapedName)"
    discriminantOffsetStr = Num.toStr discriminantOffset
    tagNames = List.map tags \{ name: n } -> n
    selfMut = "self"

    max = \a, b -> if a >= b then a else b

    alignOfUnion =
        List.walk tags 1 \accum, { payload } ->
            when payload is
                Some payloadId -> max accum (Types.alignment types payloadId)
                None -> accum

    alignOfUnionStr = Num.toStr alignOfUnion

    sizeOfUnionStr =
        List.walk tags 1 \accum, { payload } ->
            when payload is
                Some payloadId -> max accum (Types.size types payloadId)
                None -> accum
        |> nextMultipleOf alignOfUnion
        |> Num.toStr

    sizeOfSelf = Num.toStr (Types.size types id)
    alignOfSelf = Num.toStr (Types.alignment types id)
    shape = Types.shape types id

    # TODO: this value can be different than the alignment of `id`
    align =
        List.walk tags 1 \accum, { payload } ->
            when payload is
                Some payloadId -> max accum (Types.alignment types payloadId)
                None -> accum
        |> Num.toStr

    unionType = Types.shape types id

    buf
    |> generateDiscriminant types discriminantName tagNames discriminantSize
    |> Str.concat "#[repr(C, align($(align)))]\npub union $(unionName) {\n"
    |> \b -> List.walk tags b (generateUnionField types)
    |> Str.concat
        """
        }

        const _SIZE_CHECK_$(unionName): () = assert!(core::mem::size_of::<$(unionName)>() == $(sizeOfUnionStr));
        const _ALIGN_CHECK_$(unionName): () = assert!(core::mem::align_of::<$(unionName)>() == $(alignOfUnionStr));

        const _SIZE_CHECK_$(escapedName): () = assert!(core::mem::size_of::<$(escapedName)>() == $(sizeOfSelf));
        const _ALIGN_CHECK_$(escapedName): () = assert!(core::mem::align_of::<$(escapedName)>() == $(alignOfSelf));

        impl $(escapedName) {
            $(discriminantDocComment)
            pub fn discriminant(&self) -> $(discriminantName) {
                unsafe {
                    let bytes = core::mem::transmute::<&Self, &[u8; core::mem::size_of::<Self>()]>(self);

                    core::mem::transmute::<u8, $(discriminantName)>(*bytes.as_ptr().add($(discriminantOffsetStr)))
                }
            }

            /// Internal helper
            fn set_discriminant(&mut self, discriminant: $(discriminantName)) {
                let discriminant_ptr: *mut $(discriminantName) = (self as *mut $(escapedName)).cast();

                unsafe {
                    *(discriminant_ptr.add($(discriminantOffsetStr))) = discriminant;
                }
            }
        }


        """
    |> Str.concat
        """
        #[repr(C)]
        pub struct $(escapedName) {
            payload: union_$(escapedName),
            discriminant: discriminant_$(escapedName),
        }
        """
    |> deriveCloneTagUnion escapedName tags
    |> deriveDebugTagUnion types escapedName tags
    |> deriveEqTagUnion types shape escapedName
    |> derivePartialEqTagUnion types shape escapedName tags
    |> deriveOrdTagUnion types shape escapedName
    |> derivePartialOrdTagUnion types shape escapedName tags
    |> deriveHashTagUnion types shape escapedName tags
    |> generateDestructorFunctions types escapedName tags
    |> generateConstructorFunctions types escapedName tags
    |> \b ->
        if cannotSupportCopy types unionType then
            # A custom drop impl is only needed when we can't derive copy.
            b
            |> Str.concat
                """
                impl Drop for $(escapedName) {
                    fn drop(&mut self) {
                        // Drop the payloads

                """
            |> generateTagUnionDropPayload types selfMut tags discriminantName discriminantSize 2
            |> Str.concat
                """
                    }
                }


                """
        else
            b
    |> generateRocRefcounted types unionType escapedName

generateNonNullableUnwrapped = \buf, types, name, tagName, payload, discriminantSize, _discriminantOffset, _nullTagIndex ->
    escapedName = escapeKW name
    discriminantName = "discriminant_$(escapedName)"

    payloadFields =
        when Types.shape types payload is
            TagUnionPayload { fields } ->
                when fields is
                    HasNoClosure xs -> List.map xs .id
                    HasClosure xs -> List.map xs .id

            _ ->
                []

    payloadFieldNames =
        commaSeparated "" payloadFields \_, i ->
            n = Num.toStr i
            "f$(n)"

    constructorArguments =
        commaSeparated "" payloadFields \id, i ->
            n = Num.toStr i
            type = typeName types id
            "f$(n): $(type)"

    debugFields =
        payloadFields
        |> List.mapWithIndex \_, i ->
            n = Num.toStr i
            ".field(&node.f$(n))"
        |> Str.joinWith ""

    buf1 = buf |> generateDiscriminant types discriminantName [tagName] discriminantSize

    unionType = TagUnion (NonNullableUnwrapped { name, tagName, payload })

    """
    $(buf1)

    #[repr(transparent)]
    #[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
    pub struct $(escapedName)(roc_std::RocBox<$(name)_$(tagName)>);

    impl $(escapedName) {
        pub fn $(tagName)($(constructorArguments)) -> Self {
            let payload = $(name)_$(tagName) { $(payloadFieldNames) };

            Self(roc_std::RocBox::new(payload))
        }
    }

    impl core::fmt::Debug for $(escapedName) {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            let node = &self.0;
            f.debug_tuple("$(escapedName)::$(tagName)")$(debugFields).finish()
        }
    }
    """
    |> generateRocRefcounted types unionType escapedName

generateRecursiveTagUnion = \buf, types, id, tagUnionName, tags, discriminantSize, _discriminantOffset, nullTagIndex ->
    escapedName = escapeKW tagUnionName
    discriminantName = "discriminant_$(escapedName)"
    tagNames = List.map tags \{ name: n } -> n
    # self = "(&*self.union_pointer())"
    # selfMut = "(&mut *self.union_pointer())"
    # other = "(&*other.union_pointer())"
    unionName = "union_$(escapedName)"

    discriminants =
        tagNames
        |> Str.joinWith ", "
        |> \b -> "[ $(b) ]"

    nullTagId =
        when nullTagIndex is
            Some index ->
                n = Num.toStr index
                "discriminants[$(n)]"

            None ->
                """
                unreachable!("this pointer cannot be NULL")
                """

    isFunction = \{ name: tagName, payload: optPayload }, index ->
        payloadFields =
            when optPayload is
                Some payload ->
                    when Types.shape types payload is
                        TagUnionPayload { fields } ->
                            when fields is
                                HasNoClosure xs -> List.map xs .id
                                HasClosure xs -> List.map xs .id

                        _ ->
                            []

                None ->
                    []

        fieldGetters =
            List.walk payloadFields { i: 0, accum: "" } \{ i, accum }, fieldTypeId ->
                fieldTypeName = typeName types fieldTypeId
                fieldIndex = Num.toStr i

                {
                    i: i + 1,
                    accum:
                    """
                    $(accum)
                        pub fn get_$(tagName)_f$(fieldIndex)(&self) -> &$(fieldTypeName) {
                            debug_assert!(self.is_$(tagName)());

                            // extern "C" {
                            //     fn foobar(tag_id: u16, field_index: usize) -> usize;
                            // }

                            // let offset = unsafe { foobar($(fieldIndex)) };
                            let offset = 0;
                            unsafe { &*self.unmasked_pointer().add(offset).cast() }
                        }

                    """,
                }
            |> .accum

        payloadFieldNames =
            commaSeparated "" payloadFields \_, i ->
                n = Num.toStr i
                "f$(n)"

        constructorArguments =
            commaSeparated "" payloadFields \payloadId, i ->
                n = Num.toStr i
                type = typeName types payloadId
                "f$(n): $(type)"

        fixManuallyDrop =
            when optPayload is
                Some payload ->
                    shape = Types.shape types payload

                    if canDeriveCopy types shape then
                        "payload"
                    else
                        "core::mem::ManuallyDrop::new(payload)"

                None ->
                    "payload"

        if Some (Num.intCast index) == nullTagIndex then
            """
                pub fn is_$(tagName)(&self) -> bool {
                    matches!(self.discriminant(), discriminant_$(escapedName)::$(tagName))
                }

                pub fn $(tagName)($(constructorArguments)) -> Self {
                    Self(std::ptr::null_mut())
                }
            """
        else
            """
                pub fn is_$(tagName)(&self) -> bool {
                    matches!(self.discriminant(), discriminant_$(escapedName)::$(tagName))
                }

                pub fn $(tagName)($(constructorArguments)) -> Self {
                    let tag_id = discriminant_$(escapedName)::$(tagName);

                    let payload = $(escapedName)_$(tagName) { $(payloadFieldNames) } ;

                    let union_payload = union_$(escapedName) { $(tagName): $(fixManuallyDrop) };

                    let ptr = unsafe { roc_std::RocBox::leak(roc_std::RocBox::new(union_payload)) };

                    Self((ptr as usize | tag_id as usize) as *mut _)
                }
            $(fieldGetters)

                pub fn get_$(tagName)(mut self) -> $(escapedName)_$(tagName) {
                    debug_assert!(self.is_$(tagName)());

                    unsafe { core::mem::ManuallyDrop::take(&mut self.ptr_read_union().$(tagName)) }
                }
            """

    constructors =
        tags
        |> List.mapWithIndex isFunction
        |> Str.joinWith "\n\n"

    cloneCase = \{ name: tagName }, index ->
        if Some (Num.intCast index) == nullTagIndex then
            """
                        $(tagName) => Self::$(tagName)(),
            """
        else
            """
                        $(tagName) => {
                            let tag_id = discriminant_$(escapedName)::$(tagName);

                            let payload_union = unsafe { self.ptr_read_union() };
                            let payload = union_$(escapedName) {
                                $(tagName): unsafe { payload_union.$(tagName).clone() },
                            };

                            let ptr = unsafe { roc_std::RocBox::leak(roc_std::RocBox::new(payload)) };

                            Self((ptr as usize | tag_id as usize) as *mut _)
                        },
            """

    cloneCases =
        tags
        |> List.mapWithIndex cloneCase
        |> Str.joinWith "\n"

    partialEqCase = \{ name: tagName }, index ->
        if Some (Num.intCast index) == nullTagIndex then
            """
                        $(tagName) => true,
            """
        else
            """
                        $(tagName) => {
                            let payload_union1 = unsafe { self.ptr_read_union() };
                            let payload_union2 = unsafe { other.ptr_read_union() };

                            unsafe {
                                payload_union1.$(tagName) == payload_union2.$(tagName)
                            }
                        },
            """

    partialEqCases =
        tags
        |> List.mapWithIndex partialEqCase
        |> Str.joinWith "\n"

    partialEqImpl =
        if canSupportPartialEqOrd types (Types.shape types id) then
            """
            impl PartialEq for $(escapedName) {
                fn eq(&self, other: &Self) -> bool {
                    use discriminant_$(escapedName)::*;

                    if self.discriminant() != other.discriminant() {
                        return false;
                    }

                    match self.discriminant() {
                        $(partialEqCases)
                    }
                }
            }

            impl Eq for $(escapedName) {}
            """
        else
            ""

    debugCase = \{ name: tagName, payload: optPayload }, index ->
        if Some (Num.intCast index) == nullTagIndex then
            """
                        $(tagName) => f.debug_tuple("$(escapedName)::$(tagName)").finish(),
            """
        else
            payloadFields =
                when optPayload is
                    Some payload ->
                        when Types.shape types payload is
                            TagUnionPayload { fields } ->
                                when fields is
                                    HasNoClosure xs -> List.map xs .id
                                    HasClosure xs -> List.map xs .id

                            _ ->
                                []

                    None ->
                        []

            debugFields =
                payloadFields
                |> List.mapWithIndex \_, i ->
                    n = Num.toStr i
                    ".field(&payload_union.$(tagName).f$(n))"
                |> Str.joinWith ""

            """
                        $(tagName) => {
                            let payload_union = unsafe { self.ptr_read_union() };

                            unsafe {
                                f.debug_tuple("$(escapedName)::$(tagName)")$(debugFields).finish()
                            }
                        },
            """

    debugCases =
        tags
        |> List.mapWithIndex debugCase
        |> Str.joinWith "\n"

    hashCase = \{ name: tagName }, index ->
        if Some (Num.intCast index) == nullTagIndex then
            """
                        $(tagName) => {}
            """
        else
            """
                        $(tagName) => {
                            let payload_union = unsafe { self.ptr_read_union() };
                            unsafe { payload_union.$(tagName).hash(state) };
                        },
            """

    hashCases =
        tags
        |> List.mapWithIndex hashCase
        |> Str.joinWith "\n"

    unionType = Types.shape types id
    hashImpl =
        if canSupportPartialEqOrd types unionType then
            """
            impl core::hash::Hash for $(escapedName) {
                fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                    use discriminant_$(escapedName)::*;

                    self.discriminant().hash(state);

                    match self.discriminant() {
                        $(hashCases)
                    }
                }
            }
            """
        else
            ""

    partialOrdCase = \{ name: tagName }, index ->
        if Some (Num.intCast index) == nullTagIndex then
            """
                        $(tagName) => std::cmp::Ordering::Equal,
            """
        else
            """
                        $(tagName) => {
                            let payload_union1 = unsafe { self.ptr_read_union() };
                            let payload_union2 = unsafe { other.ptr_read_union() };

                            unsafe {
                                payload_union1.$(tagName).cmp(&payload_union2.$(tagName))
                            }
                        },
            """

    partialOrdCases =
        tags
        |> List.mapWithIndex partialOrdCase
        |> Str.joinWith "\n"

    partialOrdImpl =
        if canSupportPartialEqOrd types (Types.shape types id) then
            """
            impl PartialOrd for $(escapedName) {
                fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                    Some(<Self as Ord>::cmp(self, other))
                }
            }

            impl Ord for $(escapedName) {
                fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                    use discriminant_$(escapedName)::*;

                    use std::cmp::Ordering::*;

                    match self.discriminant().cmp(&other.discriminant()) {
                        Less => Less,
                        Greater => Greater,
                        Equal => unsafe {
                            match self.discriminant() {
                                $(partialOrdCases)
                            }
                        },
                    }
                }
            }
            """
        else
            ""

    sizeOfSelf = Num.toStr (Types.size types id)
    alignOfSelf = Num.toStr (Types.alignment types id)

    buf
    |> generateDiscriminant types discriminantName tagNames discriminantSize
    |> Str.concat
        """
        #[repr(transparent)]
        pub struct $(escapedName)(*mut $(unionName));

        const _SIZE_CHECK_$(escapedName): () = assert!(core::mem::size_of::<$(escapedName)>() == $(sizeOfSelf));
        const _ALIGN_CHECK_$(escapedName): () = assert!(core::mem::align_of::<$(escapedName)>() == $(alignOfSelf));

        impl $(escapedName) {
            pub fn discriminant(&self) -> discriminant_$(escapedName) {
                let discriminants = {
                    use $(discriminantName)::*;

                    $(discriminants)
                };

                if self.0.is_null() {
                    $(nullTagId)
                } else  {
                    match std::mem::size_of::<usize>() {
                        4 => discriminants[self.0 as usize & 0b011],
                        8 => discriminants[self.0 as usize & 0b111],
                        _ => unreachable!(),
                    }
                }
            }

            fn unmasked_pointer(&self) -> *mut union_$(escapedName) {
                debug_assert!(!self.0.is_null());

                let mask = match std::mem::size_of::<usize>() {
                    4 => !0b011usize,
                    8 => !0b111usize,
                    _ => unreachable!(),
                };

                ((self.0 as usize) & mask) as *mut union_$(escapedName)
            }

            unsafe fn ptr_read_union(&self) -> core::mem::ManuallyDrop<union_$(escapedName)> {
                let ptr = self.unmasked_pointer();

                core::mem::ManuallyDrop::new(unsafe { std::ptr::read(ptr) })
            }

            $(constructors)
        }

        impl Clone for $(escapedName) {
            fn clone(&self) -> Self {
                use discriminant_$(escapedName)::*;

                let discriminant = self.discriminant();

                match discriminant {
                $(cloneCases)
                }
            }
        }

        $(partialEqImpl)

        $(hashImpl)

        $(partialOrdImpl)


        impl core::fmt::Debug for $(escapedName) {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                use discriminant_$(escapedName)::*;

                match self.discriminant() {
                    $(debugCases)
                }
            }
        }


        #[repr(C)]
        union $(unionName) {
        """
    |> \b -> List.walk tags b (generateUnionField types)
    |> Str.concat "}\n\n"
    |> generateRocRefcounted types unionType escapedName

generateTagUnionDropPayload = \buf, types, selfMut, tags, discriminantName, discriminantSize, indents ->
    if discriminantSize == 0 then
        when List.first tags is
            Ok { name } ->
                # There's only one tag, so there's no discriminant and no need to match;
                # just drop the pointer.
                buf
                |> writeIndents indents
                |> Str.concat "unsafe { core::mem::ManuallyDrop::drop(&mut core::ptr::read(self.pointer).$(name)); }"

            Err ListWasEmpty ->
                crash "unreachable"
    else
        buf
        |> writeTagImpls tags discriminantName indents \name, payload ->
            when payload is
                Some id if cannotSupportCopy types (Types.shape types id) ->
                    "unsafe { core::mem::ManuallyDrop::drop(&mut $(selfMut).payload.$(name)) },"

                _ ->
                    # If it had no payload, or if the payload had no pointers,
                    # there's nothing to clean up, so do `=> {}` for the branch.
                    "{}"

writeIndents = \buf, indents ->
    if indents <= 0 then
        buf
    else
        buf
        |> Str.concat indent
        |> writeIndents (indents - 1)

writeTagImpls : Str, List { name : Str, payload : [Some TypeId, None] }, Str, U64, (Str, [Some TypeId, None] -> Str) -> Str
writeTagImpls = \buf, tags, discriminantName, indents, f ->
    buf
    |> writeIndents indents
    |> Str.concat "match self.discriminant() {\n"
    |> \b -> List.walk tags b \accum, { name, payload } ->
            branchStr = f name payload
            accum
            |> writeIndents (indents + 1)
            |> Str.concat "$(discriminantName)::$(name) => $(branchStr)\n"
    |> writeIndents indents
    |> Str.concat "}\n"

generateDiscriminant = \buf, types, name, tags, size ->
    if size > 0 then
        enumType =
            TagUnion
                (
                    Enumeration {
                        name,
                        tags,
                        size,
                    }
                )

        buf
        |> generateEnumeration types enumType name tags size
    else
        buf

generateUnionField = \types ->
    \accum, { name: fieldName, payload } ->
        escapedFieldName = escapeKW fieldName

        when payload is
            Some id ->
                typeStr = typeName types id

                type = Types.shape types id
                fullTypeStr =
                    if cannotSupportCopy types type then
                        # types with pointers need ManuallyDrop
                        # because rust unions don't (and can't)
                        # know how to drop them automatically!
                        "core::mem::ManuallyDrop<$(typeStr)>"
                    else
                        typeStr

                Str.concat accum "$(indent)$(escapedFieldName): $(fullTypeStr),\n"

            None ->
                # use unit as the payload
                Str.concat accum "$(indent)$(escapedFieldName): (),\n"

commaSeparated : Str, List a, (a, U64 -> Str) -> Str
commaSeparated = \buf, items, step ->

    length = List.len items |> Num.intCast

    help : { buf : Str, count : U64 }, a -> { buf : Str, count : U64 }
    help = \accum, item ->
        if accum.count + 1 == length then
            { buf: Str.concat accum.buf (step item accum.count), count: length }
        else
            { buf: Str.concat accum.buf (step item accum.count) |> Str.concat ", ", count: accum.count + 1 }

    items
    |> List.walk { buf, count: 0 } help
    |> .buf

generateNullableUnwrapped : Str, Types, TypeId, Str, Str, Str, TypeId, [FirstTagIsNull, SecondTagIsNull] -> Str
generateNullableUnwrapped = \buf, types, tagUnionid, name, nullTag, nonNullTag, nonNullPayload, whichTagIsNull ->
    payloadFields =
        when Types.shape types nonNullPayload is
            TagUnionPayload { fields } ->
                when fields is
                    HasNoClosure xs -> List.map xs .id
                    HasClosure xs -> List.map xs .id

            _ ->
                []

    payloadFieldNames =
        commaSeparated "" payloadFields \_, i ->
            n = Num.toStr i
            "f$(n)"

    constructorArguments =
        commaSeparated "" payloadFields \id, i ->
            n = Num.toStr i
            type = typeName types id
            "f$(n): $(type)"

    debugFields =
        payloadFields
        |> List.mapWithIndex \_, i ->
            n = Num.toStr i
            ".field(&node.f$(n))"
        |> Str.joinWith ""

    unionType = TagUnion (NullableUnwrapped { name, nullTag, nonNullTag, nonNullPayload, whichTagIsNull })
    discriminant =
        when whichTagIsNull is
            FirstTagIsNull ->
                """
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                pub enum discriminant_$(name) {
                    $(nullTag) = 0,
                    $(nonNullTag) = 1,
                }
                """

            SecondTagIsNull ->
                """
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                pub enum discriminant_$(name) {
                    $(nonNullTag) = 0,
                    $(nullTag) = 1,
                }
                """

    sizeOfSelf = Num.toStr (Types.size types tagUnionid)
    alignOfSelf = Num.toStr (Types.alignment types tagUnionid)

    """
    $(buf)

    #[derive(PartialOrd, Ord)]
    #[repr(C)]
    pub struct $(name)(*mut $(name)_$(nonNullTag));

    $(discriminant)

    const _SIZE_CHECK_$(name): () = assert!(core::mem::size_of::<$(name)>() == $(sizeOfSelf));
    const _ALIGN_CHECK_$(name): () = assert!(core::mem::align_of::<$(name)>() == $(alignOfSelf));

    impl $(name) {
        pub fn $(nullTag)() -> Self {
            Self(core::ptr::null_mut())
        }

        pub fn $(nonNullTag)($(constructorArguments)) -> Self {
            let payload = $(name)_$(nonNullTag) { $(payloadFieldNames) };

            let ptr = unsafe { roc_std::RocBox::leak(roc_std::RocBox::new(payload)) };

            Self(ptr)
        }

        pub fn discriminant(&self) -> discriminant_$(name) {
            if self.is_$(nullTag)() {
                discriminant_$(name)::$(nullTag)
            } else {
                discriminant_$(name)::$(nonNullTag)
            }
        }

        pub fn is_$(nullTag)(&self) -> bool {
            self.0.is_null()
        }

        pub fn is_$(nonNullTag)(&self) -> bool {
            !self.0.is_null()
        }
    }

    impl core::fmt::Debug for $(name) {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            if self.is_$(nullTag)() {
                f.debug_tuple("$(name)::$(nullTag)").finish()
            } else {
                let node = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(self.0) });
                f.debug_tuple("$(name)::$(nonNullTag)")$(debugFields).finish()
            }
        }
    }

    impl Clone for $(name) {
        fn clone(&self) -> Self {
            if self.is_$(nullTag)() {
                Self::$(nullTag)()
            } else {
                use std::ops::Deref;

                let node_ref = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(self.0) });
                let payload : $(name)_$(nonNullTag) = (node_ref.deref()).clone();

                let ptr = unsafe { roc_std::RocBox::leak(roc_std::RocBox::new(payload)) };

                Self(ptr)
            }
        }
    }

    impl PartialEq for $(name) {
        fn eq(&self, other: &Self) -> bool {
            if self.discriminant() != other.discriminant() {
                return false;
            }

            if self.is_$(nullTag)() {
                return true;
            }

            let payload1 = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(self.0) });
            let payload2 = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(other.0) });

            payload1 == payload2
        }
    }

    impl Eq for $(name) {}

    impl core::hash::Hash for $(name) {
        fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
            self.discriminant().hash(state);

            if self.is_$(nonNullTag)() {
                let payload = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(self.0) });
                payload.hash(state);
            }
        }
    }
    """
    |> generateRocRefcounted types unionType name

generateSingleTagStruct = \buf, types, name, tagName, payload ->
    # Store single-tag unions as structs rather than enums,
    # because they have only one alternative. However, still
    # offer the usual tag union APIs.
    escapedName = escapeKW name
    repr =
        length =
            when payload is
                HasClosure fields -> List.len fields
                HasNoClosure fields -> List.len fields
        if length <= 1 then
            "transparent"
        else
            "C"

    when payload is
        HasNoClosure fields ->
            asStructFields =
                List.mapWithIndex fields \{ id }, index ->
                    indexStr = Num.toStr index

                    { name: "f$(indexStr)", id }
                |> HasNoClosure
            asStructType =
                Struct {
                    name,
                    fields: asStructFields,
                }

            buf
            |> generateDeriveStr types asStructType ExcludeDebug
            |> Str.concat "#[repr($(repr))]\npub struct $(escapedName) "
            |> \b ->
                if List.isEmpty fields then
                    generateZeroElementSingleTagStruct b escapedName tagName
                else
                    generateMultiElementSingleTagStruct b types escapedName tagName fields asStructFields
            |> generateRocRefcounted types (TagUnion (SingleTagStruct { name, tagName, payload })) escapedName

        HasClosure _ ->
            Str.concat buf "\\TODO: SingleTagStruct with closures"

generateMultiElementSingleTagStruct = \buf, types, name, tagName, payloadFields, asStructFields ->
    buf
    |> Str.concat "{\n"
    |> generateStructFields types Private asStructFields
    |> Str.concat "}\n\n"
    |> Str.concat
        """
        impl $(name) {

        """
    |> \b ->
        fieldTypes =
            payloadFields
            |> List.map \{ id } ->
                typeName types id
        args =
            fieldTypes
            |> List.mapWithIndex \fieldTypeName, index ->
                indexStr = Num.toStr index

                "f$(indexStr): $(fieldTypeName)"
        fields =
            payloadFields
            |> List.mapWithIndex \_, index ->
                indexStr = Num.toStr index

                "f$(indexStr)"

        fieldAccesses =
            fields
            |> List.map \field ->
                "self.$(field)"

        {
            b,
            args,
            fields,
            fieldTypes,
            fieldAccesses,
        }
    |> \{ b, args, fields, fieldTypes, fieldAccesses } ->
        argsStr = Str.joinWith args ", "
        fieldsStr = Str.joinWith fields ",\n$(indent)$(indent)$(indent)"

        {
            b: Str.concat
                b
                """
                $(indent)/// A tag named ``$(tagName)``, with the given payload.
                $(indent)pub fn $(tagName)($(argsStr)) -> Self {
                $(indent)    Self {
                $(indent)        $(fieldsStr)
                $(indent)    }
                $(indent)}


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
                $(indent)/// Since `$(name)` only has one tag (namely, `$(tagName)`),
                $(indent)/// convert it to `$(tagName)`'s payload.
                $(indent)pub fn into_$(tagName)(self) -> $(retType) {
                $(indent)    $(retExpr)
                $(indent)}


                """,
            fieldTypes,
            fieldAccesses,
        }
    |> \{ b, fieldTypes, fieldAccesses } ->
        retType =
            fieldTypes
            |> List.map \ft -> "&$(ft)"
            |> asRustTuple
        retExpr =
            fieldAccesses
            |> List.map \fa -> "&$(fa)"
            |> asRustTuple

        Str.concat
            b
            """
            $(indent)/// Since `$(name)` only has one tag (namely, `$(tagName)`),
            $(indent)/// convert it to `$(tagName)`'s payload.
            $(indent)pub fn as_$(tagName)(&self) -> $(retType) {
            $(indent)    $(retExpr)
            $(indent)}

            """
    |> Str.concat
        """
        }


        impl core::fmt::Debug for $(name) {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.debug_tuple("$(name)::$(tagName)")

        """
    |> \b ->
        payloadFields
        |> List.mapWithIndex \_, index ->
            indexStr = Num.toStr index

            "$(indent)$(indent)$(indent)$(indent).field(&self.f$(indexStr))\n"
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
        "($(joined))"

generateZeroElementSingleTagStruct = \buf, name, tagName ->
    # A single tag with no payload is a zero-sized unit type, so
    # represent it as a zero-sized struct (e.g. "struct Foo()").
    buf
    |> Str.concat "();\n\n"
    |> Str.concat
        """
        impl $(name) {
            /// A tag named $(tagName), which has no payload.
            pub const $(tagName): Self = Self();

            /// Other `into_` methods return a payload, but since $(tagName) tag
            /// has no payload, this does nothing and is only here for completeness.
            pub fn into_$(tagName)(self) {
                ()
            }

            /// Other `as_` methods return a payload, but since $(tagName) tag
            /// has no payload, this does nothing and is only here for completeness.
            pub fn as_$(tagName)(&self) {
                ()
            }
        }

        impl core::fmt::Debug for $(name) {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.write_str("$(name)::$(tagName)")
            }
        }


        """

generateDeriveStr = \buf, types, type, includeDebug ->
    condWrite = \b, cond, str ->
        if cond then
            Str.concat b str
        else
            b

    deriveDebug =
        when includeDebug is
            IncludeDebug -> Bool.true
            ExcludeDebug -> Bool.false

    buf
    |> Str.concat "#[derive(Clone, "
    |> condWrite (!(cannotSupportCopy types type)) "Copy, "
    |> condWrite (!(cannotSupportDefault types type)) "Default, "
    |> condWrite deriveDebug "Debug, "
    |> condWrite (canSupportPartialEqOrd types type) "PartialEq, PartialOrd, "
    |> condWrite (canSupportEqHashOrd types type) "Eq, Ord, Hash, "
    |> Str.concat ")]\n"

canSupportEqHashOrd : Types, Shape -> Bool
canSupportEqHashOrd = \types, type ->
    !(hasFloat types type) && (canSupportPartialEqOrd types type)

canSupportPartialEqOrd : Types, Shape -> Bool
canSupportPartialEqOrd = \types, type ->
    when type is
        Function rocFn ->
            runtimeRepresentation = Types.shape types rocFn.lambdaSet
            canSupportPartialEqOrd types runtimeRepresentation

        Unsized -> Bool.false
        Unit | EmptyTagUnion | Bool | Num _ | TagUnion (Enumeration _) -> Bool.true
        RocStr -> Bool.true
        RocList inner | RocSet inner | RocBox inner ->
            innerType = Types.shape types inner
            canSupportPartialEqOrd types innerType

        RocDict k v ->
            kType = Types.shape types k
            vType = Types.shape types v

            canSupportPartialEqOrd types kType && canSupportPartialEqOrd types vType

        TagUnion (Recursive { tags }) ->
            List.all tags \{ payload } ->
                when payload is
                    None -> Bool.true
                    Some id -> canSupportPartialEqOrd types (Types.shape types id)

        TagUnion (NullableWrapped { tags }) ->
            List.all tags \{ payload } ->
                when payload is
                    None -> Bool.true
                    Some id -> canSupportPartialEqOrd types (Types.shape types id)

        TagUnion (NonNullableUnwrapped { payload }) ->
            canSupportPartialEqOrd types (Types.shape types payload)

        TagUnion (NullableUnwrapped { nonNullPayload }) ->
            canSupportPartialEqOrd types (Types.shape types nonNullPayload)

        RecursivePointer _ -> Bool.true
        TagUnion (SingleTagStruct { payload: HasNoClosure fields }) ->
            List.all fields \{ id } -> canSupportPartialEqOrd types (Types.shape types id)

        TagUnion (SingleTagStruct { payload: HasClosure _ }) ->
            Bool.false

        TagUnion (NonRecursive { tags }) ->
            List.all tags \{ payload } ->
                when payload is
                    Some id -> canSupportPartialEqOrd types (Types.shape types id)
                    None -> Bool.true

        RocResult okId errId ->
            okShape = Types.shape types okId
            errShape = Types.shape types errId

            canSupportPartialEqOrd types okShape && canSupportPartialEqOrd types errShape

        Struct { fields: HasNoClosure fields } | TagUnionPayload { fields: HasNoClosure fields } ->
            List.all fields \{ id } -> canSupportPartialEqOrd types (Types.shape types id)

        Struct { fields: HasClosure fields } | TagUnionPayload { fields: HasClosure fields } ->
            List.all fields \{ id } -> canSupportPartialEqOrd types (Types.shape types id)

cannotSupportCopy : Types, Shape -> Bool
cannotSupportCopy = \types, type ->
    !(canDeriveCopy types type)

canDeriveCopy : Types, Shape -> Bool
canDeriveCopy = \types, type ->
    when type is
        Function rocFn ->
            runtimeRepresentation = Types.shape types rocFn.lambdaSet
            canDeriveCopy types runtimeRepresentation

        # unsized values are heap-allocated
        Unsized -> Bool.false
        Unit | EmptyTagUnion | Bool | Num _ | TagUnion (Enumeration _) -> Bool.true
        RocStr | RocList _ | RocDict _ _ | RocSet _ | RocBox _ | TagUnion (NullableUnwrapped _) | TagUnion (NullableWrapped _) | TagUnion (Recursive _) | TagUnion (NonNullableUnwrapped _) | RecursivePointer _ -> Bool.false
        TagUnion (SingleTagStruct { payload: HasNoClosure fields }) ->
            List.all fields \{ id } -> canDeriveCopy types (Types.shape types id)

        TagUnion (SingleTagStruct { payload: HasClosure fields }) ->
            List.all fields \{ id } -> canDeriveCopy types (Types.shape types id)

        TagUnion (NonRecursive { tags }) ->
            List.all tags \{ payload } ->
                when payload is
                    Some id -> canDeriveCopy types (Types.shape types id)
                    None -> Bool.true

        RocResult okId errId ->
            canDeriveCopy types (Types.shape types okId)
            && canDeriveCopy types (Types.shape types errId)

        Struct { fields: HasNoClosure fields } | TagUnionPayload { fields: HasNoClosure fields } ->
            List.all fields \{ id } -> canDeriveCopy types (Types.shape types id)

        Struct { fields: HasClosure fields } | TagUnionPayload { fields: HasClosure fields } ->
            List.all fields \{ id } -> canDeriveCopy types (Types.shape types id)

cannotSupportDefault = \types, type ->
    when type is
        Unit | Unsized | EmptyTagUnion | TagUnion _ | RocResult _ _ | RecursivePointer _ | Function _ -> Bool.true
        RocStr | Bool | Num _ -> Bool.false
        RocList id | RocSet id | RocBox id ->
            cannotSupportDefault types (Types.shape types id)

        TagUnionPayload { fields: HasClosure _ } -> Bool.true
        RocDict keyId valId ->
            cannotSupportCopy types (Types.shape types keyId)
            || cannotSupportCopy types (Types.shape types valId)

        Struct { fields: HasClosure _ } -> Bool.true
        Struct { fields: HasNoClosure fields } | TagUnionPayload { fields: HasNoClosure fields } ->
            List.any fields \{ id } -> cannotSupportDefault types (Types.shape types id)

hasFloat = \types, type ->
    hasFloatHelp types type (Set.empty {})

hasFloatHelp = \types, type, doNotRecurse ->
    # TODO: is doNotRecurse problematic? Do we need an updated doNotRecurse for calls up the tree?
    # I think there is a change it really only matters for RecursivePointer, so it may be fine.
    # Otherwise we need to deal with threading through updates to doNotRecurse
    when type is
        Num kind ->
            when kind is
                F32 | F64 -> Bool.true
                _ -> Bool.false

        Unit | Unsized | EmptyTagUnion | RocStr | Bool | TagUnion (Enumeration _) | Function _ -> Bool.false
        RocList id | RocSet id | RocBox id ->
            hasFloatHelp types (Types.shape types id) doNotRecurse

        RocDict id0 id1 | RocResult id0 id1 ->
            hasFloatHelp types (Types.shape types id0) doNotRecurse
            || hasFloatHelp types (Types.shape types id1) doNotRecurse

        Struct { fields: HasNoClosure fields } | TagUnionPayload { fields: HasNoClosure fields } ->
            List.any fields \{ id } -> hasFloatHelp types (Types.shape types id) doNotRecurse

        Struct { fields: HasClosure fields } | TagUnionPayload { fields: HasClosure fields } ->
            List.any fields \{ id } -> hasFloatHelp types (Types.shape types id) doNotRecurse

        TagUnion (SingleTagStruct { payload: HasNoClosure fields }) ->
            List.any fields \{ id } -> hasFloatHelp types (Types.shape types id) doNotRecurse

        TagUnion (SingleTagStruct { payload: HasClosure fields }) ->
            List.any fields \{ id } -> hasFloatHelp types (Types.shape types id) doNotRecurse

        TagUnion (Recursive { tags }) ->
            List.any tags \{ payload } ->
                when payload is
                    Some id -> hasFloatHelp types (Types.shape types id) doNotRecurse
                    None -> Bool.false

        TagUnion (NonRecursive { tags }) ->
            List.any tags \{ payload } ->
                when payload is
                    Some id -> hasFloatHelp types (Types.shape types id) doNotRecurse
                    None -> Bool.false

        TagUnion (NullableWrapped { tags }) ->
            List.any tags \{ payload } ->
                when payload is
                    Some id -> hasFloatHelp types (Types.shape types id) doNotRecurse
                    None -> Bool.false

        TagUnion (NonNullableUnwrapped { payload }) ->
            if Set.contains doNotRecurse payload then
                Bool.false
            else
                nextDoNotRecurse = Set.insert doNotRecurse payload

                hasFloatHelp types (Types.shape types payload) nextDoNotRecurse

        TagUnion (NullableUnwrapped { nonNullPayload }) ->
            if Set.contains doNotRecurse nonNullPayload then
                Bool.false
            else
                nextDoNotRecurse = Set.insert doNotRecurse nonNullPayload

                hasFloatHelp types (Types.shape types nonNullPayload) nextDoNotRecurse

        RecursivePointer payload ->
            if Set.contains doNotRecurse payload then
                Bool.false
            else
                nextDoNotRecurse = Set.insert doNotRecurse payload

                hasFloatHelp types (Types.shape types payload) nextDoNotRecurse

generateRocRefcounted = \buf, types, type, escapedName ->
    if containsRefcounted types type then
        impl =
            when type is
                TagUnion (NonNullableUnwrapped _) ->
                    """
                    impl roc_std::RocRefcounted for $(escapedName) {
                        fn inc(&mut self) {
                            self.0.inc();
                        }
                        fn dec(&mut self) {
                            self.0.dec();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """

                TagUnion (Recursive _) ->
                    """
                    impl roc_std::RocRefcounted for $(escapedName) {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    impl roc_std::RocRefcounted for union_$(escapedName) {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """

                TagUnion (NullableWrapped _) ->
                    """
                    impl roc_std::RocRefcounted for $(escapedName) {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    impl roc_std::RocRefcounted for union_$(escapedName) {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            unimplemented!();
                        }
                    }\n\n
                    """

                Struct { fields: HasNoClosure fields } ->
                    incFields = generateRocRefcountedNamedFields types fields Inc Struct
                    decFields = generateRocRefcountedNamedFields types fields Dec Struct
                    """
                    impl roc_std::RocRefcounted for $(escapedName) {
                        fn inc(&mut self) {
                        $(incFields)
                        }
                        fn dec(&mut self) {
                        $(decFields)
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """

                TagUnionPayload { fields: HasNoClosure fields } ->
                    incFields = generateRocRefcountedNamedFields types fields Inc Tag
                    decFields = generateRocRefcountedNamedFields types fields Dec Tag
                    """
                    impl roc_std::RocRefcounted for $(escapedName) {
                        fn inc(&mut self) {
                        $(incFields)
                        }
                        fn dec(&mut self) {
                        $(decFields)
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """

                _ ->
                    """
                    impl roc_std::RocRefcounted for $(escapedName) {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """
        Str.concat
            buf
            impl
    else
        Str.concat buf "roc_refcounted_noop_impl!($(escapedName));\n\n"

generateRocRefcountedNamedFields = \types, fields, mode, wrapper ->
    fieldName = \name ->
        escapedName = escapeKW name
        when wrapper is
            Struct -> escapedName
            Tag -> "f$(escapedName)"

    methodName =
        when mode is
            Inc -> "inc"
            Dec -> "dec"

    walker =
        \accum, { name, id } ->
            if containsRefcounted types (Types.shape types id) then
                Str.concat
                    accum
                    "$(indent) self.$(fieldName name).$(methodName)();\n"
            else
                accum

    List.walk fields "" walker

# If a value or any data in it must be refcounted.
containsRefcounted = \types, type ->
    containsRefcountedHelp types type (Set.empty {})

containsRefcountedHelp = \types, type, doNotRecurse ->
    # TODO: is doNotRecurse problematic? Do we need an updated doNotRecurse for calls up the tree?
    # I think there is a change it really only matters for RecursivePointer, so it may be fine.
    # Otherwise we need to deal with threading through updates to doNotRecurse
    when type is
        RocStr | RocList _ | RocSet _ | RocDict _ _ | RocBox _ | RecursivePointer _ ->
            Bool.true

        Unit | Unsized | EmptyTagUnion | Num _ | Bool | TagUnion (Enumeration _) ->
            Bool.false

        Function { lambdaSet: id } ->
            containsRefcountedHelp types (Types.shape types id) doNotRecurse

        RocResult id0 id1 ->
            containsRefcountedHelp types (Types.shape types id0) doNotRecurse
            || containsRefcountedHelp types (Types.shape types id1) doNotRecurse

        Struct { fields: HasNoClosure fields } | TagUnionPayload { fields: HasNoClosure fields } ->
            List.any fields \{ id } -> containsRefcountedHelp types (Types.shape types id) doNotRecurse

        Struct { fields: HasClosure fields } | TagUnionPayload { fields: HasClosure fields } ->
            List.any fields \{ id } -> containsRefcountedHelp types (Types.shape types id) doNotRecurse

        TagUnion (SingleTagStruct { payload: HasNoClosure fields }) ->
            List.any fields \{ id } -> containsRefcountedHelp types (Types.shape types id) doNotRecurse

        TagUnion (SingleTagStruct { payload: HasClosure fields }) ->
            List.any fields \{ id } -> containsRefcountedHelp types (Types.shape types id) doNotRecurse

        TagUnion (Recursive _) -> Bool.true
        TagUnion (NullableWrapped _) -> Bool.true
        TagUnion (NonNullableUnwrapped _) -> Bool.true
        TagUnion (NullableUnwrapped _) -> Bool.true
        TagUnion (NonRecursive { tags }) ->
            List.any tags \{ payload } ->
                when payload is
                    Some id -> containsRefcountedHelp types (Types.shape types id) doNotRecurse
                    None -> Bool.false

typeName = \types, id ->
    when Types.shape types id is
        Unit -> "()"
        Unsized -> "roc_std::RocList<u8>"
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
        RocDict _key _value ->
            # keyName = typeName types key
            # valueName = typeName types value
            # "roc_std::RocDict<$(keyName), $(valueName)>"
            crash "RocDict is not yet supported in rust"

        RocSet _elem ->
            # elemName = typeName types elem
            # "roc_std::RocSet<$(elemName)>"
            crash "RocSet is not yet supported in rust"

        RocList elem ->
            elemName = typeName types elem

            "roc_std::RocList<$(elemName)>"

        RocBox elem ->
            elemName = typeName types elem

            "roc_std::RocBox<$(elemName)>"

        RocResult ok err ->
            okName = typeName types ok
            errName = typeName types err

            "roc_std::RocResult<$(okName), $(errName)>"

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
        Function { functionName } -> escapeKW functionName

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
    #![allow(clippy::needless_borrow)]
    #![allow(clippy::clone_on_copy)]
    #![allow(clippy::non_canonical_partial_ord_impl)]


    use roc_std::RocRefcounted;
    use roc_std::roc_refcounted_noop_impl;


    """

indent = "    "
discriminantDocComment = "/// Returns which variant this tag union holds. Note that this never includes a payload!"

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
        "r#$(input)"
    else
        input

nextMultipleOf = \lhs, rhs ->
    when lhs % rhs is
        0 -> lhs
        r -> lhs + (rhs - r)

isUnit : Shape -> Bool
isUnit = \shape ->
    when shape is
        Unit -> Bool.true
        _ -> Bool.false

toArgStr : List TypeId, Types, (TypeId, Shape, U64 -> Str) -> Str
toArgStr = \args, types, fmt ->
    List.walkWithIndex args "" \state, argId, index ->
        shape = Types.shape types argId

        # Drop `()` args; they aren't FFI-safe, and nothing will get passed anyway.
        if isUnit shape then
            state
        else
            argStr = fmt argId shape index

            if Str.isEmpty state then
                argStr # Don't prepend a comma if this is the first one
            else
                state
                |> Str.concat ", "
                |> Str.concat argStr
