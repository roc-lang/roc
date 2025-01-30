module [value_from_pkg]

import cli.Foo

value_from_pkg = Foo.foo

expect value_from_pkg == "Foo"
