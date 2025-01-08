hosted PlatformTasks
    exposes [put_line, put_int, get_int]
    imports []

put_line : Str -> Task {} *

put_int : I64 -> Task {} *

get_int : Task { value : I64, is_error : Bool } *
