# META
~~~ini
description=Record construction with complex field types including lists and tag unions
type=expr
~~~
# SOURCE
~~~roc
{
    name: "Alice",
    scores: [95, 87, 92, 78],
    status: Active({ since: "2023-01-15" }),
    preferences: { theme: Dark, notifications: Email("alice@example.com") },
    metadata: Ok {
        tags: ["developer", "senior", "fullstack"],
        permissions: [Read, Write, Admin],
    },
    callback: |x| x + 1,
    nested: {
        items: [Some("first"), None, Some("third")],
        result: Success({ data: [1, 2, 3], timestamp: "2024-01-01" })
    }
}
~~~
