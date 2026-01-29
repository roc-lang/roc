# Types

## Roc's Type System

- no hkp
- rank-1

## Type Annotations

### Where Clauses

## Structural Types

## Nominal Types

### Constructing Nominal Types

### Destructuring Nominal Types

Pattern can be destructured to access inner values.

```roc
Color := [Red, Green, Blue].{
    my_color : Color
    my_color = Red
    
    is_red : Color -> Bool
    is_red = |color| match color {
        Red -> True
        _ -> False
    }
}

expect my_color.is_red() # PASS
```

For records:

```roc
get_x : Point -> F64
get_x = |{ x }| x
```

### Opaque Nominal Types

### Nested Nominal Types

Nominal types can be defined inside other nominal types using the associated items block:

```roc
Geometry := [].{
    Point := { x: F64, y: F64 }.{
        origin : Point
        origin = { x: 0, y: 0 }
    }

    Rectangle := { top_left: Point, bottom_right: Point }.{
        area : Rectangle -> F64
        area = |{ top_left, bottom_right }|
            width = bottom_right.x - top_left.x
            height = bottom_right.y - top_left.y
            width * height
    }
}
```

Nested types are accessed using dot notation:

```roc
rect = Geometry.Rectangle.{ top_left: Geometry.Point.origin, bottom_right: { x: 10, y: 10 } }
```

This is useful for grouping related types under a common namespace.

## Type Aliases
