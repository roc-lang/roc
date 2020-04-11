app Primary
    provides [ blah2, blah3, str, alwaysThree, identity, z, w, succeed, withDefault, yay ]
    imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar }, Res ]

blah2 = Dep2.two
blah3 = bar

str = Dep1.str

# alwaysThree = \_ -> Dep1.three # TODO FIXME for some reason this infers as a circular type
alwaysThree = \_ -> "foo"

identity = \a -> a

z = identity (alwaysThree {})

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
