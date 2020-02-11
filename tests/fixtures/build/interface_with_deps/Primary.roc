interface Primary
    exposes [ blah, str ]
    imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar } ]

blah = {}

str = Dep1.str

# alwaysThree = \_ -> Dep1.three # TODO FIXME for some reason this infers as a circular type
alwaysThree = \_ -> 3

identity = \a -> a

# z = identity (alwaysThree {}) # TODO FIXME for some reason this infers as a circular type
# z = identity 3                # TODO FIXME for some reason this also infers as a circular type

z = 3
