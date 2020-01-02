interface WithoutBuiltins
    exposes [ blah ]
    imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar } ]

alwaysThreePointZero = \_ -> Dep1.three

answer = 42

fromDep2 = two

identity = \a -> a

threePointZero = identity (alwaysThreePointZero {}) 
