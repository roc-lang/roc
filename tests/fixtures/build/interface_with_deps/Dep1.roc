interface Dep1
    exposes [ three, str, Unit, Identity ]
    imports [ Dep3.Blah.{ foo } ]

one = 1

two = 2

three = 3.0

str = "string!"

Unit : [ Unit ]

Identity a : [ Identity a ]
