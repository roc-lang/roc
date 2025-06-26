# META
~~~ini
description=Nested record creation
type=expr
~~~
# SOURCE
~~~roc
{
    person: { name: "Alice", age: 30 },
    address: { 
        street: "123 Main St", 
        city: "Springfield", 
        coordinates: { lat: 42.1234, lng: -71.5678 }
    },
    contact: {
        email: "alice@example.com",
        phone: { home: "555-1234", work: "555-5678" }
    }
}
~~~