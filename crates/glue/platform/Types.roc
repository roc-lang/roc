interface Types
    exposes [Types, shape]
    imports [Shape.{ Shape }, TypeId.{ TypeId }, InternalTypeId, Target.{ Target }]

# TODO use real tuples here when doing so doesn't crash the compiler
Tuple1 : [T Str TypeId]
Tuple2 : [T TypeId (List TypeId)]

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

shape : Types, TypeId -> Shape
shape = \@Types types, id ->
    when List.get types.shapes (InternalTypeId.toNat id) is
        Ok answer -> answer
        Err _ -> crash "unreachable"
