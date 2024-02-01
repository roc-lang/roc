interface Primary
    exposes [blah2, blah3, str, alwaysThree, identity, z, w, succeed, withDefault, yay]

import Dep1
import Dep2
import Dep3Blah exposing [bar]
import Res

blah2 = Dep2.two
blah3 = bar

str = Dep1.str

alwaysThree = \_ -> Dep1.three

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
