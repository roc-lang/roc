module [Types, shape, size, alignment, target, walk_shapes, entry_points]

import Shape exposing [Shape]
import TypeId exposing [TypeId, type_id_from_u64, type_id_to_u64]
import Target exposing [Target]

# TODO: switch AssocList uses to Dict once roc_std is updated.
Tuple1 : [T Str TypeId]
Tuple2 : [T TypeId (List TypeId)]

Types := {
    # These are all indexed by TypeId
    types : List Shape,
    sizes : List U32,
    aligns : List U32,

    # Needed to check for duplicates
    types_by_name : List Tuple1,

    ## Dependencies - that is, which type depends on which other type.
    ## This is important for declaration order in C; we need to output a
    ## type declaration earlier in the file than where it gets referenced by another type.
    deps : List Tuple2,

    ## Names and types of the entry points of the program (e.g. main_for_host)
    entrypoints : List Tuple1,
    target : Target,
}
    implements [Inspect, Encoding]

target : Types -> Target
target = \@Types(types) -> types.target

entry_points : Types -> List Tuple1
entry_points = \@Types({ entrypoints }) -> entrypoints

walk_shapes : Types, state, (state, Shape, TypeId -> state) -> state
walk_shapes = \@Types({ types: shapes }), original_state, update ->
    List.walk_with_index(shapes, original_state, \state, elem, index ->
        id = type_id_from_u64(index)

        update(state, elem, id))

shape : Types, TypeId -> Shape
shape = \@Types(types), id ->
    when List.get(types.types, type_id_to_u64(id)) is
        Ok(answer) -> answer
        Err(OutOfBounds) ->
            id_str = Num.to_str(type_id_to_u64(id))

            crash("TypeId #${id_str} was not found in Types. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>")

alignment : Types, TypeId -> U32
alignment = \@Types(types), id ->
    when List.get(types.aligns, type_id_to_u64(id)) is
        Ok(answer) -> answer
        Err(OutOfBounds) ->
            id_str = Num.to_str(type_id_to_u64(id))

            crash("TypeId #${id_str} was not found in Types. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>")

size : Types, TypeId -> U32
size = \@Types(types), id ->
    when List.get(types.sizes, type_id_to_u64(id)) is
        Ok(answer) -> answer
        Err(OutOfBounds) ->
            id_str = Num.to_str(type_id_to_u64(id))

            crash("TypeId #${id_str} was not found in Types. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>")
