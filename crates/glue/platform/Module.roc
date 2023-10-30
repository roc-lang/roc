interface Module
    exposes [Module, name, type, size, alignment, target, walkTypes, entryPoints, path]
    imports [Type.{ Type }, TypeId.{ TypeId }, Target.{ Target }, InternalTypeId]

Module := {
    name : Str,
    # Although some OSes allow paths with non-Unicode characters, `roc glue` does not support this.
    path : Str,

    # These are all indexed by TypeId
    types : List Type,
    sizes : List U32,
    aligns : List U32,

    # Needed to check for duplicates
    typesByName : Dict Str TypeId,

    ## Dependencies - that is, which type depends on which other type.
    ## This is important for declaration order in C; we need to output a
    ## type declaration earlier in the file than where it gets referenced by another type.
    deps : Dict TypeId (List TypeId),

    ## Names and types of the entry points of the program (e.g. mainForHost)
    entrypoints : Dict Str TypeId,
    target : Target,
}

name : Module -> Str
name = \@Module mod -> mod.name

path : Module -> Str
path = \@Module mod -> mod.path

target : Module -> Target
target = \@Module mod -> mod.target

entryPoints : Module -> Dict Str TypeId
entryPoints = \@Module { entrypoints } -> entrypoints

walkTypes : Module, state, (state, Type, TypeId -> state) -> state
walkTypes = \@Module { types }, originalState, update ->
    List.walkWithIndex types originalState \state, elem, index ->
        id = InternalTypeId.fromNat index

        update state elem id

type : Module, TypeId -> Type
type = \@Module mod, id ->
    when List.get mod.types (InternalTypeId.toNat id) is
        Ok answer -> answer
        Err OutOfBounds ->
            idStr = Num.toStr (InternalTypeId.toNat id)

            crash "TypeId #\(idStr) was not found in Module. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>"

alignment : Module, TypeId -> U32
alignment = \@Module mod, id ->
    when List.get mod.aligns (InternalTypeId.toNat id) is
        Ok answer -> answer
        Err OutOfBounds ->
            idStr = Num.toStr (InternalTypeId.toNat id)

            crash "TypeId #\(idStr) was not found in Module. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>"

size : Module, TypeId -> U32
size = \@Module mod, id ->
    when List.get mod.sizes (InternalTypeId.toNat id) is
        Ok answer -> answer
        Err OutOfBounds ->
            idStr = Num.toStr (InternalTypeId.toNat id)

            crash "TypeId #\(idStr) was not found in Module. This should never happen, and means there was a bug in `roc glue`. If you have time, please open an issue at <https://github.com/roc-lang/roc/issues>"
