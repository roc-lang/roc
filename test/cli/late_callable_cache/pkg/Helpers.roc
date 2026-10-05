import Container

Helpers := [].{
    apply : (U64 -> Str), U64 -> Str
    apply = |callback, value| callback(value)

    apply_record : { inner : { callback : U64 -> Str } }, U64 -> Str
    apply_record = |bundle, value| {
        first = (bundle.inner.callback)(value)
        second = (bundle.inner.callback)(value)
        Str.concat(first, second)
    }

    apply_container : Container, U64 -> Str
    apply_container = |container, value| {
        first = (container.callback)(value)
        second = (container.callback)(value)
        Str.concat(first, second)
    }
}
