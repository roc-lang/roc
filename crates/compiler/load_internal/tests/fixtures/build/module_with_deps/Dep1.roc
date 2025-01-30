module [three, str, Unit, Identity, one, two]

import Dep3 exposing [foo]

one = 1

two = foo

three = 3.0

str = "string!"

Unit : [Unit]

Identity a : [Identity a]
