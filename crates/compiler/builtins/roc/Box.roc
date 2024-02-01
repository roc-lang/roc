## Most users won't need Box, it is used for:
## - Holding unknown Roc types when developing [platforms](https://github.com/roc-lang/roc/wiki/Roc-concepts-explained#platform).
## - To improve performance in rare cases.
##
interface Box
    exposes [box, unbox]

## Allocates a value on the heap. Boxing is an expensive process as it copies
## the value from the stack to the heap. This may provide a performance
## optimization for advanced use cases with large values. A platform may require
## that some values are boxed.
## ```
## expect Box.unbox (Box.box "Stack Faster") == "Stack Faster"
## ```
box : a -> Box a

## Returns a boxed value.
## ```
## expect Box.unbox (Box.box "Stack Faster") == "Stack Faster"
## ```
unbox : Box a -> a

# # we'd need reset/reuse for box for this to be efficient
# # that is currently not implemented
# map : Box a, (a -> b) -> Box b
# map = \boxed, transform =
#     boxed
#         |> Box.unbox
#         |> transform
#         |> Box.box
