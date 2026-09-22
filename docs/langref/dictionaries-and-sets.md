# Dictionaries and Sets

A [`Dict`](../Dict) (short for "dictionary") is a collection of key-value pairs, where each key
appears at most once. A [`Set`](../Set) is a collection of values, where each value appears at most once.

## Dictionaries

`Dict(k, v)` is the type of a dictionary with keys of type `k` and values of type `v`. For example, a
dictionary that maps people's names to their ages could have the type `Dict(Str, U64)`.

There's no special syntax for dictionaries. Instead, you create them using functions such as
[`Dict.empty`](../Dict#empty), [`Dict.single`](../Dict#single), and [`Dict.from_list`](../Dict#from_list):

```roc
ages = Dict.from_list([("Sam", 30), ("Alex", 25)])
```

Then you can use [`insert`](../Dict#insert), [`get`](../Dict#get), [`remove`](../Dict#remove),
and so on:

```roc
with_jo = ages.insert("Jo", 41)

sam_age = with_jo.get("Sam") ?? 0 # 30
```

Like all values in Roc, dictionaries are immutable, so `insert` and `remove` return a new dictionary rather
than modifying the existing one. (If nothing else is referencing the original dictionary, Roc will
[update it in place](expressions#opportunistic-mutation) behind the scenes, so this is often
just as fast as mutating it.)

Inserting a key that's already in the dictionary replaces its value.

### Looking Up Values

Since a key might not be in the dictionary, [`Dict.get`](../Dict#get) returns a [`Try`](../Try):
`Ok` with the value if the key was found, and `Err(KeyNotFound)` if it wasn't. The
[`??` operator](operators#-default-value-on-err) is a convenient way to provide a default value for a
missing key, as in the example above. To check whether a key is present without getting its value,
use [`Dict.contains`](../Dict#contains).

To change the value for a key based on its current value (or lack thereof), use
[`Dict.update`](../Dict#update). Here's how you might count how many times each word appears in a list:

```roc
count_words : List(Str) -> Dict(Str, U64)
count_words = |words| {
    var $counts = Dict.empty()

    for word in words {
        $counts = $counts.update(word, |existing| match existing {
            Ok(count) => Ok(count + 1)
            Err(Missing) => Ok(1)
        })
    }

    $counts
}
```

The function passed to `update` receives `Ok(value)` if the key is present, or `Err(Missing)` if it isn't. It returns
`Ok(new_value)` to store a value for the key, or `Err(Missing)` to remove the key from the dictionary.

### Iteration Order

Dictionaries remember the order in which keys were inserted, and iterate over them in that order:

```roc
for (name, age) in ages {
    echo!("${name} is ${age.to_str()}\n")
}
```

As this example shows, iterating over a dictionary yields `(key, value)` tuples.

Replacing an existing key's value doesn't change that key's position in the order. However,
removing a key moves the most recently inserted key into the removed key's position, so after
a removal, the order is no longer purely based on when keys were inserted. (This makes removal
fast, because nothing else needs to be moved.)

## Sets

`Set(a)` is the type of a set of values of type `a`. Like dictionaries, sets are created with functions,
such as [`Set.empty`](../Set#empty) and [`Set.from_list`](../Set#from_list):

```roc
primes = Set.from_list([2, 3, 5, 7, 7, 7])

primes.len() # 4, because each value appears at most once
primes.contains(5) # True
```

Sets support operations like [`union`](../Set#union), [`intersection`](../Set#intersection),
and [`difference`](../Set#difference). They follow the same iteration order rules as dictionaries.

Under the hood, a `Set(a)` is a `Dict(a, {})`: a dictionary whose keys are the set's values, and whose values
are all the [empty record](records#empty-record), which takes up no memory.

## Keys and Hashing

Dictionaries and sets are [hash tables](https://en.wikipedia.org/wiki/Hash_table), so their keys must support
two operations:

- Equality, using the `is_eq` method (which is what `==` uses)
- Hashing, using the `to_hash` method

Most types have both of these automatically. Builtin types like strings and numbers have them, and
so do [records](records), [tuples](tuples), [tag unions](tag-unions), and lists, as long as all the types
they contain do too. So you can use a record like `{ x: 0, y: 0 }` as a key without doing anything special.

[Nominal types](types#nominal-types) can use the compiler's [derived implementations](static-dispatch#compiler-derived-methods)
by declaring `is_eq : _` and `to_hash : _`, or they can define their own.

Functions can't be compared for equality, so they can't be dictionary keys or set elements.

Hash tables which use a predictable hashing function can be vulnerable to [hash flooding](https://en.wikipedia.org/wiki/Collision_attack#Hash_flooding)
attacks, where an attacker sends many keys which are known to have the same hash in order to slow down
the program. To prevent this, dictionaries which are built at runtime use a hash seed which is
randomly chosen each time the program runs.
