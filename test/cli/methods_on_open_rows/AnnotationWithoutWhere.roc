# A row variable an annotation names carries only the requirements its
# where clause states.

AnnotationWithoutWhere :: [].{}

anno_eq : { id : U64, ..r }, { id : U64, ..r } -> Bool
anno_eq = |l, r| l == r

anno_foo : { id : U64, ..r }, { id : U64, ..r } -> Bool
anno_foo = |l, r| l.foo(r)

anno_red : [Red, ..] -> Bool
anno_red = |c| c == Red
