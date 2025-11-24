# META
~~~ini
description=Test == and != operators with custom user-defined types that have is_eq methods
type=snippet
~~~
# SOURCE
~~~roc
# Define a simple Color type with custom equality
Color := [Red, Green, Blue].{
    is_eq : Color, Color -> Bool
    is_eq = |a, b| match a {
        Red => match b {
            Red => True
            Green => False
            Blue => False
        }
        Green => match b {
            Red => False
            Green => True
            Blue => False
        }
        Blue => match b {
            Red => False
            Green => False
            Blue => True
        }
    }
}

c1 : Color
c1 = Color.Red

c2 : Color
c2 = Color.Red

c3 : Color
c3 = Color.Blue

# Test equality - same values should be equal
expect c1 == c2

# Test inequality - different values should not be equal
expect c1 != c3
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
