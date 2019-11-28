interface Dep1
    exposes [ three, str ]
    imports [ Dep3.Blah.{ foo } ]

one = 1

two = 2

three = 3

str = "string!"
