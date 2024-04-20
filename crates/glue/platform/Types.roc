module [Types, shape, size, alignment, target, walkShapes, entryPoints]

import Shape exposing [Shape]
import TypeId exposing [TypeId]
import Target exposing [Target]
import TypeId

# TODO: switch AssocList uses to Dict once roc_std is updated.
Tuple1 : [T Str TypeId]
Tuple2 : [T TypeId (List TypeId)]

Types := {
    # These are all indexed by TypeId
    types : List Shape,
    sizes : List U32,
    aligns : List U32,

    # Needed to check for duplicates
    typesByName : List Tuple1,

    ## Dependencies - that is, which type depends on which other type.
    ## This is important for declaration order in C; we need to output a
    ## type declaration earlier in the file than where it gets referenced by another type.
    deps : List Tuple2,

    ## Names and types of the entry points of the program (e.g. mainForHost)
    entrypoints : List Tuple1,
    target : Target,
}

target : Types -> Target
target = \@Types types -> types.target

entryPoints : Types -> List Tuple1
entryPoints = \@Types { entrypoints } -> entrypoints

walkShapes : Types, state, (state, Shape, TypeId -> state) -> state
walkShapes = \@Types { types: shapes }, originalState, update ->
    List.walkWithIndex shapes originalState \state, elem, index ->
        id = TypeId.fromU64 index

        update state elem id

shape : Types, TypeId -> Shape
shape = \@Types types, id ->
    when List.get types.types (TypeId.toU64 id) is
        Ok answer -> answer
        Err OutOfBounds ->
            idStr = Num.toStr (TypeId.toU64 id)

            crash "TypeId #$(idStr) was not found in Types. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>"

alignment : Types, TypeId -> U32
alignment = \@Types types, id ->
    when List.get types.aligns (TypeId.toU64 id) is
        Ok answer -> answer
        Err OutOfBounds ->
            idStr = Num.toStr (TypeId.toU64 id)

            crash "TypeId #$(idStr) was not found in Types. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>"

size : Types, TypeId -> U32
size = \@Types types, id ->
    when List.get types.sizes (TypeId.toU64 id) is
        Ok answer -> answer
        Err OutOfBounds ->
            idStr = Num.toStr (TypeId.toU64 id)

            crash "TypeId #$(idStr) was not found in Types. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>"
