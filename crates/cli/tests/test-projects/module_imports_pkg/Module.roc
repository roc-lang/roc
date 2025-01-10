module [value_from_pkg]

import pkg.Foo

value_from_pkg = Foo.foo

expect value_from_pkg == "Foo"
