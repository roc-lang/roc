interface Primary
    exposes [ blah, str ]
    imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar } ]

blah = 1

two = 2

str = foo

alwaysThree = \_ -> Dep1.three

identity = \a -> a

three = identity (alwaysThree {}) 
