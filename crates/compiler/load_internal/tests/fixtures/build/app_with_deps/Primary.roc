module [blah2, blah3, str, always_three, identity, z, w, succeed, with_default, yay]

import Dep1
import Dep2
import Dep3Blah exposing [bar]
import Res

blah2 = Dep2.two
blah3 = bar

str = Dep1.str

always_three = \_ -> Dep1.three

identity = \a -> a

z = identity(always_three({}))

w : Dep1.Identity {}
w = Identity({})

succeed : a -> Dep1.Identity a
succeed = \x -> Identity(x)

with_default = Res.with_default

yay : Res.Res {} err
yay =
    ok = Ok("foo")

    f = \_ -> {}

    Res.map(ok, f)
