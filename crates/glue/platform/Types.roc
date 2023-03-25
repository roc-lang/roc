interface Types
    exposes [Types, shape, target]
    imports [Shape.{ Shape }, TypeId.{ TypeId }, InternalTypeId, Target.{ Target }, InternalTypeId]

Types := {
    # These are all indexed by TypeId
    shapes : List Shape,
    sizes : List U32,
    aligns : List U32,

    # Needed internally to check for duplicates
    typesByName : List Tuple1,

    ## Dependencies - that is, which type depends on which other type.
    ## This is important for declaration order in C; we need to output a
    ## type declaration earlier in the file than where it gets referenced by another type.
    deps : List Tuple2,
    target : Target,
}

target : Types -> Target
target = \@Types types -> types.target

shape : Types, TypeId -> Shape
shape = \@Types types, id ->
    when List.get types.shapes (InternalTypeId.toNat id) is
        Ok answer -> answer
        Err OutOfBounds ->
            idStr = Num.toStr (InternalTypeId.toNat id)

            crash "TypeId #\(idStr) was not found in Types. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>"

# TODO use real tuples here when doing so doesn't crash the compiler
Tuple1 : [T Str TypeId]
Tuple2 : [T TypeId (List TypeId)]
