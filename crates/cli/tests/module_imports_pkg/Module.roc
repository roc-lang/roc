module [valueFromPkg]

import pkg.Foo

valueFromPkg = Foo.foo

expect valueFromPkg == "Foo"
