interface Box
    exposes [box, unbox]
    imports []

## Allocate a value on the heap. Boxing is an expensive processes as it copies
## the value from the stack to the heap.
##
## expect Box.unbox(Box.box "Stack Faster") == "Stack Faster"
box : a -> Box a

## Return a value to the stack. Unboxing is an expensive processes as it copies
## the value from the heap to the stack.
##
## expect Box.unbox(Box.box "Stack Faster") == "Stack Faster"
unbox : Box a -> a

# # we'd need reset/reuse for box for this to be efficient
# # that is currently not implemented
# map : Box a, (a -> b) -> Box b
# map = \boxed, transform =
#     boxed
#         |> Box.unbox
#         |> transform
#         |> Box.box
