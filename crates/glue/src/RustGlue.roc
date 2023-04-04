app "rust-glue"
    packages { pf: "../platform/main.roc" }
    imports [pf.Types.{ Types }, pf.Shape.{ Shape, RocFn }, pf.File.{ File }, pf.TypeId.{ TypeId }]
    provides [makeGlue] to pf

makeGlue : List Types -> Result (List File) Str
makeGlue = \typesByArch ->
    modFileContent =
        List.walk typesByArch "" \content, types ->
            arch = (Types.target types).architecture
            archStr = archName arch

            Str.concat
                content
                """
                #[cfg(target_arch = "\(archStr)")]
                mod \(archStr);
                #[cfg(target_arch = "\(archStr)")]
                pub use \(archStr)::*;

                """

    typesByArch
    |> List.map convertTypesToFile
    |> List.append { name: "mod.rs", content: modFileContent }
    |> Ok

convertTypesToFile : Types -> File
convertTypesToFile = \types ->
    content =
        Types.walkShapes types fileHeader \buf, type, id ->
            when type is
                Struct { name, fields } ->
                    generateStruct buf types id name fields Public

                TagUnionPayload { name, fields } ->
                    generateStruct buf types id name (nameTagUnionPayloadFields fields) Private

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
                    generateRecursiveTagUnion buf types id name tags discriminantSize discriminantOffset (Some indexOfNullTag)

                TagUnion (NullableUnwrapped { name, nullTag, nonNullTag, nonNullPayload, whichTagIsNull }) ->
                    generateNullableUnwrapped buf types id name nullTag nonNullTag nonNullPayload whichTagIsNull

                TagUnion (SingleTagStruct { name, tagName, payload }) ->
                    generateSingleTagStruct buf types name tagName payload

                TagUnion (NonNullableUnwrapped { name, tagName, payload }) ->
                    generateRecursiveTagUnion buf types id name [{ name: tagName, payload: Some payload }] 0 0 None

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
        name: "\(archStr).rs",
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
                    rocFn.args
                    |> List.mapWithIndex \argId, i ->
                        type = typeName types argId
                        c = Num.toStr i
                        "arg\(c): \(type)"
                    |> Str.joinWith ", "

                ret = typeName types rocFn.ret

                "(\(arguments)) -> \(ret)"

            _ ->
                ret = typeName types id
                "() -> \(ret)"

    externSignature =
        when Types.shape types id is
            Function rocFn ->
                arguments =
                    rocFn.args
                    |> List.map \argId ->
                        type = typeName types argId
                        "_: \(type)"
                    |> Str.joinWith ", "

                ret = typeName types rocFn.ret
                "(_: *mut \(ret), \(arguments))"

            _ ->
                ret = typeName types id
                "(_: *mut \(ret))"

    externArguments =
        when Types.shape types id is
            Function rocFn ->
                rocFn.args
                |> List.mapWithIndex \_, i ->
                    c = Num.toStr i
                    "arg\(c)"
                |> Str.joinWith ", "

            _ ->
                ""

    """
    \(buf)

    pub fn \(name)\(publicSignature) {
        extern "C" {
            fn roc__\(name)_1_exposed_generic\(externSignature);
        }

        let mut ret = std::mem::MaybeUninit::uninit();

        unsafe { roc__\(name)_1_exposed_generic(ret.as_mut_ptr(), \(externArguments)) };

        unsafe { ret.assume_init() }
    }
    """

generateFunction : Str, Types, RocFn -> Str
generateFunction = \buf, types, rocFn ->
    name = rocFn.functionName
    externName = rocFn.externName

    lambdaSet = typeName types rocFn.lambdaSet

    publicArguments =
        rocFn.args
        |> List.mapWithIndex \argId, i ->
            type = typeName types argId
            c = Num.toStr i
            "arg\(c): \(type)"
        |> Str.joinWith ", "

    externDefArguments =
        rocFn.args
        |> List.mapWithIndex \argId, i ->
            type = typeName types argId
            c = Num.toStr i
            "arg\(c): *const \(type)"
        |> Str.joinWith ", "

    externCallArguments =
        rocFn.args
        |> List.mapWithIndex \_, i ->
            c = Num.toStr i
            "&arg\(c)"
        |> Str.joinWith ", "

    externComma = if Str.isEmpty publicArguments then "" else ", "

    ret = typeName types rocFn.ret

    """
    \(buf)

    #[repr(C)]
    #[derive(Debug, Clone)]
    pub struct \(name) {
        closure_data: \(lambdaSet),
    }

    impl \(name) {
        pub fn force_thunk(mut self, \(publicArguments)) -> \(ret) {
            extern "C" {
                fn \(externName)(\(externDefArguments)\(externComma) closure_data: *mut u8, output: *mut \(ret));
            }

            let mut output = std::mem::MaybeUninit::uninit();
            let ptr = &mut self.closure_data as *mut _ as *mut u8;

            unsafe { \(externName)(\(externCallArguments)\(externComma) ptr, output.as_mut_ptr(), ) };

            // ownership of the closure is transferred back to roc
            core::mem::forget(self.closure_data);

            unsafe { output.assume_init() }
        }
    }
    """

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
            Public -> "pub"
            Private -> ""

    structType = Types.shape types id

    buf
    |> generateDeriveStr types structType IncludeDebug
    |> Str.concat "#[repr(\(repr))]\n\(pub) struct \(escapedName) {\n"
    |> generateStructFields types Public structFields
    |> Str.concat "}\n\n"

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

        Str.concat accum "\(indent)\(pub) \(escapedFieldName): \(typeStr),\n"

nameTagUnionPayloadFields = \payloadFields ->
    # Tag union payloads have numbered fields, so we prefix them
    # with an "f" because Rust doesn't allow struct fields to be numbers.
    when payloadFields is
        HasNoClosure fields ->
            renamedFields = List.map fields \{ name, id } -> { name: "f\(name)", id }
            HasNoClosure renamedFields

        HasClosure fields ->
            renamedFields = List.map fields \{ name, id, accessors } -> { name: "f\(name)", id, accessors }
            HasClosure renamedFields

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

deriveCloneTagUnion : Str, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
deriveCloneTagUnion = \buf, tagUnionType, tags ->
    clones =
        List.walk tags "" \accum, { name: tagName } ->
            """
            \(accum)
                            \(tagName) => union_\(tagUnionType) {
                                \(tagName): self.payload.\(tagName).clone(),
                            },
            """

    """
    \(buf)

    impl Clone for \(tagUnionType) {
        fn clone(&self) -> Self {
            use discriminant_\(tagUnionType)::*;

            let payload = unsafe {
                match self.discriminant {\(clones)
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
            type = when payload is
                Some id -> typeName types id
                None -> "()"

            """
            \(accum)
                            \(tagName) => {
                                let field: &\(type) = &self.payload.\(tagName);
                                f.debug_tuple("\(tagUnionType)::\(tagName)").field(field).finish()
                            },
            """

    """
    \(buf)

    impl core::fmt::Debug for \(tagUnionType) {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            use discriminant_\(tagUnionType)::*;

            unsafe {
                match self.discriminant {\(checks)
                }
            }
        }
    }
    """

deriveEqTagUnion : Str, Str -> Str
deriveEqTagUnion = \buf, tagUnionType ->
    """
    \(buf)

    impl Eq for \(tagUnionType) {}
    """


derivePartialEqTagUnion : Str, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
derivePartialEqTagUnion = \buf, tagUnionType, tags ->
    checks =
        List.walk tags "" \accum, { name: tagName } ->
            """
            \(accum)
                            \(tagName) => self.payload.\(tagName) == other.payload.\(tagName),
            """

    """
    \(buf)

    impl PartialEq for \(tagUnionType) {
        fn eq(&self, other: &Self) -> bool {
            use discriminant_\(tagUnionType)::*;

            if self.discriminant != other.discriminant {
                return false;
            }

            unsafe {
                match self.discriminant {\(checks)
                }
            }
        }
    }
    """

deriveOrdTagUnion : Str, Str -> Str
deriveOrdTagUnion = \buf, tagUnionType ->
    """
    \(buf)

    impl Ord for \(tagUnionType) {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.partial_cmp(other).unwrap()
        }
    }
    """

derivePartialOrdTagUnion : Str, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
derivePartialOrdTagUnion = \buf, tagUnionType, tags ->
    checks =
        List.walk tags "" \accum, { name: tagName } ->
            """
            \(accum)
                                \(tagName) => self.payload.\(tagName).partial_cmp(&other.payload.\(tagName)),
            """

    """
    \(buf)

    impl PartialOrd for \(tagUnionType) {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            use discriminant_\(tagUnionType)::*;

            use std::cmp::Ordering::*;

            match self.discriminant.cmp(&other.discriminant) {
                Less => Option::Some(Less),
                Greater => Option::Some(Greater),
                Equal => unsafe {
                    match self.discriminant {\(checks)
                    }
                },
            }
        }
    }
    """

deriveHashTagUnion : Str, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
deriveHashTagUnion = \buf, tagUnionType, tags ->
    checks =
        List.walk tags "" \accum, { name: tagName } ->
            """
            \(accum)
                            \(tagName) => self.payload.\(tagName).hash(state),
            """

    """
    \(buf)

    impl core::hash::Hash for \(tagUnionType) {
        fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
            use discriminant_\(tagUnionType)::*;

            unsafe {
                match self.discriminant {\(checks)
                }
            }
        }
    }
    """

generateConstructorFunctions : Str, Types, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
generateConstructorFunctions = \buf, types, tagUnionType, tags ->
    buf
        |> Str.concat "\n\nimpl \(tagUnionType) {"
        |> \b -> List.walk tags b \accum, r -> generateConstructorFunction accum types tagUnionType r.name r.payload
        |> Str.concat "\n}\n\n"

generateConstructorFunction : Str, Types, Str, Str, [Some TypeId, None] -> Str
generateConstructorFunction = \buf, types, tagUnionType, name, optPayload ->
    when optPayload is
        None ->
            """
            \(buf)

                pub fn \(name)() -> Self {
                    Self {
                        discriminant: discriminant_\(tagUnionType)::\(name),
                        payload: union_\(tagUnionType) {
                            \(name): (),
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
            \(buf)

                pub fn \(name)(payload: \(payloadType)) -> Self {
                    Self {
                        discriminant: discriminant_\(tagUnionType)::\(name),
                        payload: union_\(tagUnionType) {
                            \(name): \(new),
                        }
                    }
                }
            """

generateDestructorFunctions : Str, Types, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
generateDestructorFunctions = \buf, types, tagUnionType, tags ->
    buf
        |> Str.concat "\n\nimpl \(tagUnionType) {"
        |> \b -> List.walk tags b \accum, r -> generateDestructorFunction accum types tagUnionType r.name r.payload
        |> Str.concat "\n}\n\n"

generateDestructorFunction : Str, Types, Str, Str, [Some TypeId, None] -> Str
generateDestructorFunction = \buf, types, tagUnionType, name, optPayload ->
    when optPayload is
        None ->
            """
            \(buf)

                pub fn is_\(name)(&self) -> bool {
                    matches!(self.discriminant, discriminant_\(tagUnionType)::\(name))
                }
            """

        Some payloadId ->
            payloadType = typeName types payloadId
            shape = Types.shape types payloadId

            take =
                if canDeriveCopy types shape then
                    "unsafe { self.payload.\(name) }"

                else
                    "unsafe { core::mem::ManuallyDrop::take(&mut self.payload.\(name)) }"

            """
            \(buf)

                pub fn unwrap_\(name)(mut self) -> \(payloadType) {
                    debug_assert_eq!(self.discriminant, discriminant_\(tagUnionType)::\(name));
                    \(take)
                }

                pub fn is_\(name)(&self) -> bool {
                    matches!(self.discriminant, discriminant_\(tagUnionType)::\(name))
                }
            """

generateNonRecursiveTagUnion : Str, Types, TypeId, Str, List { name : Str, payload : [Some TypeId, None] }, U32, U32 -> Str
generateNonRecursiveTagUnion = \buf, types, id, name, tags, discriminantSize, discriminantOffset ->
    escapedName = escapeKW name
    discriminantName = "discriminant_\(escapedName)"
    unionName = "union_\(escapedName)"
    discriminantOffsetStr = Num.toStr discriminantOffset
    tagNames = List.map tags \{ name: n } -> n
    selfMut = "self"

    max = \a, b -> if a >= b then a else b

    # TODO: this value can be different than the alignment of `id`
    align =
        List.walk tags 1 \accum, { payload } ->
            when payload is
                Some payloadId -> max accum (Types.alignment types payloadId)
                None -> accum
        |> Num.toStr

    buf
    |> generateDiscriminant types discriminantName tagNames discriminantSize
    |> Str.concat "#[repr(C, align(\(align)))]\npub union \(unionName) {\n"
    |> \b -> List.walk tags b (generateUnionField types)
    |> generateTagUnionSizer types id tags
    |> Str.concat
        """
        }

        impl \(escapedName) {
            \(discriminantDocComment)
            pub fn discriminant(&self) -> \(discriminantName) {
                unsafe {
                    let bytes = core::mem::transmute::<&Self, &[u8; core::mem::size_of::<Self>()]>(self);

                    core::mem::transmute::<u8, \(discriminantName)>(*bytes.as_ptr().add(\(discriminantOffsetStr)))
                }
            }

            /// Internal helper
            fn set_discriminant(&mut self, discriminant: \(discriminantName)) {
                let discriminant_ptr: *mut \(discriminantName) = (self as *mut \(escapedName)).cast();

                unsafe {
                    *(discriminant_ptr.add(\(discriminantOffsetStr))) = discriminant;
                }
            }
        }


        """
    |> Str.concat
        """
        #[repr(C)]
        pub struct \(escapedName) {
            payload: union_\(escapedName),
            discriminant: discriminant_\(escapedName),
        }
        """
    |> deriveCloneTagUnion escapedName tags
    |> deriveDebugTagUnion types escapedName tags
    |> deriveEqTagUnion escapedName
    |> derivePartialEqTagUnion escapedName tags
    |> deriveOrdTagUnion escapedName
    |> derivePartialOrdTagUnion escapedName tags
    |> deriveHashTagUnion escapedName tags
    |> generateDestructorFunctions types escapedName tags
    |> generateConstructorFunctions types escapedName tags
    |> \b ->
        type = Types.shape types id
        if cannotDeriveCopy types type then
            # A custom drop impl is only needed when we can't derive copy.
            b
            |> Str.concat
                """
                impl Drop for \(escapedName) {
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

generateRecursiveTagUnion = \buf, types, id, name, tags, discriminantSize, _discriminantOffset, _nullTagIndex ->
    escapedName = escapeKW name
    discriminantName = "discriminant_\(escapedName)"
    tagNames = List.map tags \{ name: n } -> n
    # self = "(&*self.union_pointer())"
    # selfMut = "(&mut *self.union_pointer())"
    # other = "(&*other.union_pointer())"
    unionName = "union_\(escapedName)"

    buf
    |> generateDiscriminant types discriminantName tagNames discriminantSize
    |> Str.concat
        """
        #[repr(transparent)]
        pub struct \(escapedName) {
            pointer: *mut \(unionName),
        }

        #[repr(C)]
        union \(unionName) {
        """
    |> \b -> List.walk tags b (generateUnionField types)
    |> generateTagUnionSizer types id tags
    |> Str.concat "}\n\n"
    |> Str.concat "// TODO: Recursive TagUnion impls\n\n"

generateTagUnionDropPayload = \buf, types, selfMut, tags, discriminantName, discriminantSize, indents ->
    if discriminantSize == 0 then
        when List.first tags is
            Ok { name } ->
                # There's only one tag, so there's no discriminant and no need to match;
                # just drop the pointer.
                buf
                |> writeIndents indents
                |> Str.concat "unsafe { core::mem::ManuallyDrop::drop(&mut core::ptr::read(self.pointer).\(name)); }"

            Err ListWasEmpty ->
                crash "unreachable"
    else
        buf
        |> writeTagImpls tags discriminantName indents \name, payload ->
            when payload is
                Some id if cannotDeriveCopy types (Types.shape types id) ->
                    "unsafe { core::mem::ManuallyDrop::drop(&mut \(selfMut).payload.\(name)) },"

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

writeTagImpls = \buf, tags, discriminantName, indents, f ->
    buf
    |> writeIndents indents
    |> Str.concat "match self.discriminant() {\n"
    |> \b -> List.walk tags b \accum, { name, payload } ->
            branchStr = f name payload
            accum
            |> writeIndents (indents + 1)
            |> Str.concat "\(discriminantName)::\(name) => \(branchStr)\n"
    |> writeIndents indents
    |> Str.concat "}\n"

generateTagUnionSizer : Str, Types, TypeId, _ -> Str
generateTagUnionSizer = \buf, types, id, tags ->
    if List.len tags > 1 then
        # When there's a discriminant (so, multiple tags) and there is
        # no alignment padding after the largest variant,
        # the compiler will make extra room for the discriminant.
        # We need that to be reflected in the overall size of the enum,
        # so add an extra variant with the appropriate size.
        #
        # (Do this even if theoretically shouldn't be necessary, since
        # there's no runtime cost and it more explicitly syncs the
        # union's size with what we think it should be.)
        size = getSizeRoundedToAlignment types id
        sizeStr = Num.toStr size

        Str.concat buf "\(indent)_sizer: [u8; \(sizeStr)],\n"
    else
        buf

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
                    if cannotDeriveCopy types type then
                        # types with pointers need ManuallyDrop
                        # because rust unions don't (and can't)
                        # know how to drop them automatically!
                        "core::mem::ManuallyDrop<\(typeStr)>"
                    else
                        typeStr

                Str.concat accum "\(indent)\(escapedFieldName): \(fullTypeStr),\n"

            None ->
                # use unit as the payload
                Str.concat accum "\(indent)\(escapedFieldName): (),\n"

generateNullableUnwrapped = \buf, _types, _id, _name, _nullTag, _nonNullTag, _nonNullPayload, _whichTagIsNull ->
    Str.concat buf "// TODO: TagUnion NullableUnwrapped\n\n"

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

                    { name: "f\(indexStr)", id }
                |> HasNoClosure
            asStructType =
                Struct {
                    name,
                    fields: asStructFields,
                }

            buf
            |> generateDeriveStr types asStructType ExcludeDebug
            |> Str.concat "#[repr(\(repr))]\npub struct \(escapedName) "
            |> \b ->
                if List.isEmpty fields then
                    generateZeroElementSingleTagStruct b escapedName tagName
                else
                    generateMultiElementSingleTagStruct b types escapedName tagName fields asStructFields

        HasClosure _ ->
            Str.concat buf "\\TODO: SingleTagStruct with closures"

generateMultiElementSingleTagStruct = \buf, types, name, tagName, payloadFields, asStructFields ->
    buf
    |> Str.concat "{\n"
    |> generateStructFields types Private asStructFields
    |> Str.concat "}\n\n"
    |> Str.concat
        """
        impl \(name) {

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

                "f\(indexStr): \(fieldTypeName)"
        fields =
            payloadFields
            |> List.mapWithIndex \_, index ->
                indexStr = Num.toStr index

                "f\(indexStr)"

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
            \(indent)pub fn as_\(tagName)(&self) -> \(retType) {
            \(indent)    \(retExpr)
            \(indent)}

            """
    |> Str.concat
        """
        }


        impl core::fmt::Debug for \(name) {
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

        impl core::fmt::Debug for \(name) {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.write_str("\(name)::\(tagName)")
            }
        }


        """

generateDeriveStr = \buf, types, type, includeDebug ->
    condWrite = \b, cond, str ->
        if cond then
            Str.concat b str
        else
            b

    deriveDebug = when includeDebug is
        IncludeDebug -> Bool.true
        ExcludeDebug -> Bool.false

    buf
    |> Str.concat "#[derive(Clone, "
    |> condWrite (!(cannotDeriveCopy types type)) "Copy, "
    |> condWrite (!(cannotDeriveDefault types type)) "Default, "
    |> condWrite deriveDebug "Debug, "
    |> condWrite (canDerivePartialEq types type) "PartialEq, PartialOrd, "
    |> condWrite (!(hasFloat types type) && (canDerivePartialEq types type)) "Eq, Ord, Hash, "
    |> Str.concat ")]\n"

canDerivePartialEq : Types, Shape -> Bool
canDerivePartialEq = \types, type ->
    when type is
        Function rocFn ->
            runtimeRepresentation = Types.shape types rocFn.lambdaSet
            canDerivePartialEq types runtimeRepresentation

        Unsized -> Bool.false

        Unit | EmptyTagUnion | Bool | Num _ | TagUnion (Enumeration _)  -> Bool.true
        RocStr -> Bool.true
        RocList inner | RocSet inner | RocBox inner ->
            innerType = Types.shape types inner
            canDerivePartialEq types innerType

        RocDict k v ->
            kType = Types.shape types k
            vType = Types.shape types v

            canDerivePartialEq types kType && canDerivePartialEq types vType

        TagUnion (Recursive { tags }) -> 
            List.all tags \{ payload } ->
                when payload is
                    None -> Bool.true
                    Some id -> canDerivePartialEq types (Types.shape types id)

        TagUnion (NullableWrapped { tags }) -> 
            List.all tags \{ payload } ->
                when payload is
                    None -> Bool.true
                    Some id -> canDerivePartialEq types (Types.shape types id)

        TagUnion (NonNullableUnwrapped { payload }) ->
            canDerivePartialEq types (Types.shape types payload)

        TagUnion (NullableUnwrapped { nonNullPayload }) -> 
            canDerivePartialEq types (Types.shape types nonNullPayload)

        RecursivePointer _ -> Bool.true

        TagUnion (SingleTagStruct { payload: HasNoClosure fields }) ->
            List.all fields \{ id } -> canDerivePartialEq types (Types.shape types id)

        TagUnion (SingleTagStruct { payload: HasClosure fields }) ->
            List.all fields \{ id } -> canDerivePartialEq types (Types.shape types id)

        TagUnion (NonRecursive { tags }) ->
            List.all tags \{ payload } ->
                when payload is
                    Some id -> canDerivePartialEq types (Types.shape types id)
                    None -> Bool.true

        RocResult okId errId ->
            okShape = Types.shape types okId
            errShape = Types.shape types errId

            canDerivePartialEq types okShape && canDerivePartialEq types errShape

        Struct { fields: HasNoClosure fields } | TagUnionPayload { fields: HasNoClosure fields } ->
            List.all fields \{ id } -> canDerivePartialEq types (Types.shape types id)

        Struct { fields: HasClosure fields } | TagUnionPayload { fields: HasClosure fields } ->
            List.all fields \{ id } -> canDerivePartialEq types (Types.shape types id)

cannotDeriveCopy : Types, Shape -> Bool
cannotDeriveCopy = \types, type ->
    !(canDeriveCopy types type)

canDeriveCopy : Types, Shape -> Bool
canDeriveCopy = \types, type ->
    when type is
        Function rocFn ->
            runtimeRepresentation = Types.shape types rocFn.lambdaSet
            canDeriveCopy types runtimeRepresentation

        # unsized values are heap-allocated
        Unsized -> Bool.false

        Unit | EmptyTagUnion | Bool | Num _ | TagUnion (Enumeration _)  -> Bool.true
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

cannotDeriveDefault = \types, type ->
    when type is
        Unit | Unsized | EmptyTagUnion | TagUnion _ | RocResult _ _ | RecursivePointer _ | Function _ -> Bool.true
        RocStr | Bool | Num _ |  TagUnionPayload { fields: HasClosure _ } -> Bool.false
        RocList id | RocSet id | RocBox id ->
            cannotDeriveDefault types (Types.shape types id)

        RocDict keyId valId ->
            cannotDeriveCopy types (Types.shape types keyId)
            || cannotDeriveCopy types (Types.shape types valId)

        Struct { fields: HasClosure _ } -> Bool.true

        Struct { fields: HasNoClosure fields } | TagUnionPayload { fields: HasNoClosure fields } ->
            List.any fields \{ id } -> cannotDeriveDefault types (Types.shape types id)

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
        Function { functionName } -> escapeKW functionName

getSizeRoundedToAlignment = \types, id ->
    alignment = Types.alignment types id

    Types.size types id
    |> roundUpToAlignment alignment

roundUpToAlignment = \width, alignment ->
    when alignment is
        0 -> width
        1 -> width
        _ ->
            if width % alignment > 0 then
                width + alignment - (width % alignment)
            else
                width

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
        "r#\(input)"
    else
        input
