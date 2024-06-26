hosted PlatformTask
    exposes [putLine, putInt, getInt]
    imports []

putLine : Str -> Task {} *

putInt : I64 -> Task {} *

getInt : Task { value : I64, isError : Bool } *
