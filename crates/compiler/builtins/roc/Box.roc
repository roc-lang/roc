interface Box
    exposes [box, unbox]
    imports []

box : a -> Box a
unbox : Box a -> a

# # we'd need reset/reuse for box for this to be efficient
# # that is currently not implemented
# map : Box a, (a -> b) -> Box b
# map = \boxed, transform =
#     boxed
#         |> Box.unbox
#         |> transform
#         |> Box.box
