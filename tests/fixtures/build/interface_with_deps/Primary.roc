interface Primary
    exposes [ blah, str ]
    imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar }, Result ]

blah = {}

str = Dep1.str

# alwaysThree = \_ -> Dep1.three # TODO FIXME for some reason this infers as a circular type
alwaysThree = \_ -> "foo"

identity = \a -> a

# z = identity (alwaysThree {}) # TODO FIXME for some reason this infers as a circular type
# z = identity 3                # TODO FIXME for some reason this also infers as a circular type

z : Dep1.Unit
z = Unit

w : Dep1.Identity {}
w = Identity {}

succeed : a -> Dep1.Identity a
succeed = \x -> Identity x

map = Result.withDefault

yay : Result.Result e {}
yay =
    v = Ok "foo"

    f = \_ -> {}

    Result.map v f
