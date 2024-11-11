module [valueFromPkg]

import cli.Foo

valueFromPkg = Foo.foo

expect valueFromPkg == "Foo"
