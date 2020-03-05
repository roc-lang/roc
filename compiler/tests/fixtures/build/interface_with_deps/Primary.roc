interface Primary
    exposes [ blah2, blah3, str, alwaysThree, identity, z, w, succeed, withDefault, yay ]
    imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar }, Res ]

blah2 = Dep2.two
blah3 = bar # TODO FIXME does work as Dep3.Blah.bar, some scoping issue

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

withDefault = Res.withDefault

yay : Res.Res {} err
yay =
    ok = Ok "foo"

    f = \_ -> {}

    Res.map ok f
