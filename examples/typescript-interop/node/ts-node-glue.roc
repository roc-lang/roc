app "ts-node-glue"
    packages { pf: "RocType.roc" }
    imports [ pf.RocType.{ Types, TypeId } ]
    provides [makeGlue] to pf

makeGlue = \typesByTarget ->
    # TODO this is wrong! We actually do need to generate a separate .d.ts file for each
    # target, because of Nat - which is number on 32-bit targets and bigint on 64-bit targets.
    typeDefs =
        # We don't care about targets for the TypeScript type definitions;
        # just grab the first one in the list.
        when List.first typesByTarget is
            Ok { types } -> tsTypeStrs types
            Err ListWasEmpty -> crash "glue received no types. This should never happen!"

    typesByTarget
    |> List.map \{ types, target } ->
        rocTypeWalk [{ name: "addon.d.ts", content: typeDefs }] \outputs, type ->
            List.append outputs (toCFile target types type)
    |> Ok

rocResultTsName : Str
rocResultTsName = "Result"

## Define the Result type alias for later use.
preamble : Str
preamble =
    """
    export type \(rocResultTsName)<Ok, Err> =
      | { kind: "Ok", value: Ok }
      | { kind: "Err", value: Err };

    """

typeDefsStr : RocTypes -> Str
typeDefsStr = \types ->
    rocTypeWalk types preamble \buf, type ->
        when type is
            # TODO export or not based on whether it's actually a type the Roc platform exposes.
            # (If it's not, declare it - so we can use it as an alias - but don't expose it.)
            Function { functionName } ->
                typeStr = tsTypeStr types type

                Str.concat buf "export function \(functionName):\(typeStr);\n"

            Struct { name }
            | TagUnion (Enumeration { name })
            | TagUnion (NonRecursive { name })
            | TagUnion (Recursive { name })
            | TagUnion (NullableWrapped { name })
            | TagUnion (NullableUnwrapped { name })
            | TagUnion (NonNullableUnwrapped { name })
            | TagUnion (SingleTagStruct { name }) ->
                typeStr = tsTypeStr types type

                Str.concat buf "export type \(name):\(typeStr);\n"

            Unit | Unsized | EmptyTagUnion | RocBox _ | RocSet _ | RocDict _ | RocList _ | Num _ | Bool | RocResult _ _ | RocStr ->
                # All of these compile to basic TypeScript types, so we don't need to
                # specify anything extra for them.
                buf

# Tagged unions are { kind: "Foo", ...payload } in TS.
tsTaggedUnionAlternative : List { name : Str, shape : RocType }, Types -> Str
tsTaggedUnionAlternative = \shapes, types ->
    tsObjTypeStr (List.concat [{ name: "kind", shape: RocStr }] shapes) types

tsObjTypeStr : List { name : Str, shape : RocType }, Types -> Str
tsObjTypeStr = \fields ->
    if List.isEmpty fields then
        "{}"
    else
        fieldStrings = List.map fields \{ name, shape } ->
            typeStr = tsTypeStr shape types

            "\(name): \(typeStr)"

        "{ "
        |> Str.concat (Str.join fieldStrings ", ")
        |> Str.concat " }"

tsTaggedUnionStr : List { name : Str, id : TypeId }* -> Str
tsTaggedUnionStr = \fields ->
    if List.isEmpty fields then
        "{}"
    else
        fieldStrings = List.map fields \{ name, id } ->
            typeStr = tsTypeStr (rocTypeShape typeId types) types

            "\(name): \(typeStr)"

        "{ "
        |> Str.concat (Str.join fieldStrings ", ")
        |> Str.concat " }"

## Named type
tsNamedTypeStr : RocType, Types -> Str
tsNamedTypeStr = \shape, types ->
    when shape is
        Bool |
        RocStr |
        EmptyTagUnion |
        Unit |
        Unsized |
        RocResult _ _ |
        Num _ |
        RocList _ |
        RocDict _ _ |
        RocSet _ ->
            tsTypeStr shape

        RecursivePointer typeId | RocBox typeId ->
            # JS doesn't have boxes, so we will unwrap boxes.
            tsNamedTypeStr (rocTypeShape typeId types) types

        Function { functionName } -> functionName

        TagUnion type ->
            when type is
                (Enumeration name) |
                (NonRecursive name) |
                (Recursive name) |
                (NonNullableUnwrapped name) |
                (NullableUnwrapped name) |
                (NullableWrapped name) |
                (SingleTagStruct name) -> name

        Struct { name } | TagUnionPayload { name } -> name

## Anonymous type
tsTypeStr : RocType, Types -> Str
tsTypeStr = \shape, types ->
    when shape is
        Bool -> "boolean"
        RocStr -> "string"
        RocBox typeId ->
            # JS doesn't have boxes, so we unwrap it.
            tsTypeStr (rocTypeShape typeId types) types

        TagUnion tags -> crash "TODO tag unions"

        EmptyTagUnion -> "never"
        Struct { fields: HasNoClosure fields } ->
            fields
            |> List.map \{ name, id } -> { name, shape: rocTypeShape id types }
            |> tsObjTypeStr types

        Struct { fields: HasClosure fields } ->
            fields
            |> List.map \{ name, id } -> { name, shape: rocTypeShape id types }
            |> tsObjTypeStr types

        SingleTagPayload { payload: HasClosure fields } ->
            fields
            |> List.map \{ name, id } -> { name, shape: rocTypeShape id types }
            |> tsTaggedUnionAlternative types

        SingleTagPayload { payload: HasNoClosure fields } ->
            namedShapes = List.mapWithIndex fields \{ id }, index ->
                indexStr = Num.toStr (index + 1)

                { name: "f\(indexStr)", shape: rocTypeShape id types }

            tsTaggedUnionAlternative namedShapes types

        TagUnionPayload { fields: HasClosure fields } ->
            fields
            |> List.map \{ name, id } -> { name, shape: rocTypeShape id types }
            |> tsTaggedUnionAlternative types

        TagUnionPayload { fields: HasNoClosure fields } ->
            fields
            |> List.map \{ name, id } -> { name, shape: rocTypeShape id types }
            |> tsTaggedUnionAlternative types

        TagUnion (Enumeration { name, tags }) ->
            alternatives = Str.join tags ", "

            "enum \(name) { \(alternatives) }"

        TagUnion (NonRecursive { name, tags }) ->
            List.map tags \{ name, payload } ->
                when payload is
                    Some id ->
                        tsTaggedUnionAlternative
                            [{ name: "payload", shape: rocTypeShape id types }]
                            types
                    None -> tsTaggedUnionAlternative [] types

        ## A recursive tag union (general case)
        ## e.g. `Expr : [Sym Str, Add Expr Expr]`
        Recursive
            {
                name : Str,
                tags : List { name : Str, payload : [Some TypeId, None] },
                discriminantSize : U32,
                discriminantOffset : U32,
            },
        ## A recursive tag union that has an empty variant
        ## Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
        ## It has more than one other variant, so they need tag IDs (payloads are "wrapped")
        ## e.g. `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`
        ## see also: https://youtu.be/ip92VMpf_-A?t=164
        NullableWrapped
            {
                name : Str,
                indexOfNullTag : U16,
                tags : List { name : Str, payload : [Some TypeId, None] },
                discriminantSize : U32,
                discriminantOffset : U32,
            },
        ## Optimization: No need to store a tag ID (the payload is "unwrapped")
        ## e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
        NonNullableUnwrapped
            {
                name : Str,
                tagName : Str,
                payload : TypeId, # These always have a payload.
            },
        ## Optimization: No need to store a tag ID (the payload is "unwrapped")
        ## e.g. `[Foo Str Bool]`
        SingleTagStruct
            {
                name : Str,
                tagName : Str,
                payload: RocSingleTagPayload,
            },
        ## A recursive tag union with only two variants, where one is empty.
        ## Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
        ## e.g. `ConsList a : [Nil, Cons a (ConsList a)]`
        NullableUnwrapped
            {
                name : Str,
                nullTag : Str,
                nonNullTag : Str,
                nonNullPayload : TypeId,
                whichTagIsNull : [FirstTagIsNull, SecondTagIsNull],
            },

        TagUnion NonRecursive -> #TODO

        ## A recursive pointer, e.g. in StrConsList : [Nil, Cons Str StrConsList],
        ## this would be the field of Cons containing the (recursive) StrConsList type,
        ## and the TypeId is the TypeId of StrConsList itself.
        RecursivePointer id -> crash "TODO recursive pointers"

        # A zero-sized type, such as an empty record or a single-tag union with no payload
        Unit -> "{}",
        Unsized -> "any", # TODO is there a better type for representing void*?

        Function { args, ret } ->
            argsStr =
                List.map args \argId -> tsTypeStr (rocTypeShape argId types) types
                |> Str.join ", "

            retStr = tsTypeStr (rocTypeShape ret types) types

            """
            (\(argsStr)): \(retStr);
            """

        RocList elemId ->
            elemStr = tsTypeStr (rocTypeShape elemId types) types

            "Array<\(elemStr)>"

        RocSet elemId ->
            elemStr = tsTypeStr (rocTypeShape elemId types) types

            "Set<\(elemStr)>"

        RocDict keyId valId ->
            keyStr = tsTypeStr (rocTypeShape keyId types) types
            valStr = tsTypeStr (rocTypeShape valId types) types

            "Map<\(keyStr), \(valStr)>"

        RocResult okId errId ->
            okStr = tsTypeStr (rocTypeShape okId types) types
            errStr = tsTypeStr (rocTypeShape errId types) types

            "\(rocResultTsName)<\(okStr), \(errStr)>"

        Num precision ->
            when precision is
                F64
                | I32 # these all fit in F64 without loss of precision, and F64 runs faster on Node
                | U32
                | I16
                | U16
                | I8
                | U8 -> "number"

                I64 | U64 | I128 | U128 -> "bigint"

                Dec ->
                    # RocDec gets stringified because JS doesn't have a good way to represent it.
                    # This way you can either parse it into a double if you don't care about
                    # the precision loss, or convert it to https://github.com/MikeMcl/bignumber.js/
                    # or even just use it as a string if it's going to be displayed to
                    # the end user anyway.
                    "string"

expect
    types = ... # TODO generate these - make sure that's possible even when they are opaque!

    {
        functionName: "internalName",
        externName: "foo",
        args: []
        lambdaSet: 0,
        ret: 0,
    }
    |> Function
    |> tsTypeStr types
    |> Bool.isEq "export function foo(): string;"

toCFile = \target, types, type ->
    crash "TODO implement toCFile"


##############################################
# TODO move these functions into RocType.roc #
##############################################

rocTypeShape : Types, TypeId -> RocType
rocTypeShape = \{ types }, typeId ->
    when List.get types typeId is
        Ok shape -> shape
        Err OutOfBounds ->
            typeIdStr = Num.toStr typeId

            crash "TypeId \(typeIdStr) was not found. This should never happen!"

rocTypeSize : Types, TypeId -> Nat
rocTypeSize = \{ sizes }, typeId ->
    when List.get sizes typeId is
        Ok size -> size
        Err OutOfBounds ->
            typeIdStr = Num.toStr typeId

            crash "TypeId \(typeIdStr) was not found. This should never happen!"

rocTypeAlign : Types, TypeId -> Nat
rocTypeAlign = \{ aligns }, typeId ->
    when List.get aligns typeId is
        Ok align -> align
        Err OutOfBounds ->
            typeIdStr = Num.toStr typeId

            crash "TypeId \(typeIdStr) was not found. This should never happen!"

rocTypeWalk : Types, state, (state, { id : TypeId, type : RocType, size : Nat, align : Nat } -> state) -> state
rocTypeWalk = \allTypes, initialState, update ->
    stateAndIndex =
        # TODO use List.walk3 once it exists
        List.walk allTypes.types { index: 0nat, state: initialState } \{ index, state }, rocType ->
            typeId = index
            entry = {
                id: typeId,
                type: rocType,
                size: rocTypeSize typesList typeId,
                align: rocTypeAlign typesList typeId,
            }

            { index: index + 1, state: update state entry }

    stateAndIndex.state