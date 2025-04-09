## Things to take note of for the rewrite

- Eta expansion -- we should make an issue
- For function lifting we probably want to pass the new "struct" of captures as the last argument, that way all the other as args are passed in the same order.
- Boxed closures need to work differently, heap allocating the capture is fine, that way the host doesn't need to know the size at runtime
