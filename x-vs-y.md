# Roc and TypeScript

These examples include code snippets for brevity, but you can use the "Full Source"
link next to each one to see the complete source code - including things like
any necessary imports, dependencies, and so on.

## Hello, World!

(This example shows the full contents of each file, so there is no "Full Source" link.)

### hello.ts
```typescript
console.log("Hello, World!")
```

### hello.roc
```roc
echo! "Hello, World!"
```

Roc applications normally begin by specifying their platform,
but when you're just starting out you can leave it unspecified
while you play around and get a feel for the basics. This gets
you get a default platform which only exposes the `echo!` function.

## Variables, Constants, Comments, If-Else

```typescript
// This is a line comment.
const targetValue = 5
let count = init

/* This is a block comment. */
if (something > targetValue) {
  count += 1
}
```

```roc
# This is a line comment.
targetValue = 5

# (Roc doesn't have block comments, just line comments.)
count = if something > targetValue then
    init + 1
else
    init
```

In Roc, all named values are constants; Roc doesn't have mutable variables like `var` or `let` in TypeScript.
`x = 5` in Roc is equivalent to `const x = 5` in TypeScript.

Roc's `if` keyword can be used as an expression, like TypeScript's ternary operator. That's why can have code like:

```roc
x = if foo then bar else baz
```

...whereas in TypeScript, that would be written as:

```typescript
x = foo ? bar : baz
```

## Functions, Shadowing, String Interpolation

```typescript
const num = 5
const amountToIncrease = 1
const list = nums.map(num => {
    num = num + amountToIncrease
    console.log(`New num inside list: ${num}`)
    return num
})

console.log(`Original num: ${num}`)
```

```roc
num = 5
amountToIncrease = 1
list = List.map nums \num ->
    num = num + amountToIncrease
    echo "New num inside list: $(num)"
    num

echo! "Original num: $(num)"
```

This line demonstrates a subtle difference in what `=` does in TypeScript compared to Roc.

In TypeScript, this gives the `num` value in the *outer* scope a new value:

```typescript
num = num + amountToIncrease
```

In Roc, assigning values (with `=` or shadowing or anything else) *never* affects the outer scope.

Instead, `x = y` in Roc directly translates to TypeScript's `const x = y` - with the one difference
That you can declare a `const` with the same name as one that's already in scope, in which case
the new one shadows the old one. For example, this Roc code...

```roc
name = "outer"

echo! name

if 1 + 1 == 2 then
    name = "inner"
    echo! name

echo! name
```

...will print "outer" and then "inner" and then "outer" again. That's because the second `name =` is
*shadowing* the outer one, only for the duration of that inner scope. Once we return to the outer scope,
the shadow has ended and `name` once again refers to the original `name =` definition on the first line.

Shadowing works the same way inside the same scope:

```roc
name = "first"
echo! name
name = "second"
echo! name
```

This will print "first" and then "second" and then for the remainder of the scope, `name` will be `"second"`.
Shadowing in this way is useful when you have a stale version of something that you want to make sure not to
refer to anymore because there is an updated version.


## Reading JSON from a file, with error handling

```typescript
fs.read(path, "utf8").then(json => {
  try {
    const { name, email } = JSON.parse(json)
    console.log(`Name: ${name} Email: ${email}`)
  } catch(err) {
    console.log(`Invalid JSON: ${err}`)
  }
}).catch(err => {
  console.log(`Error reading file: ${err}`)
})
```

```roc
when File.read path Json.utf8 is
    Ok { name, email } ->
        echo "Name: $(name) Email: $(email)"

    Err (DecodingFailed err) ->
        echo "Invalid JSON: $(err)"

    Err (FileReadErr err) ->
        echo "Error reading file: $(Inspect.toStr err)"
```

Roc's I/O is always performed asynchronously behind the scenes,
although you don't need to say `await` or chain Promises with `.then`.

The Roc implementation has an additional benefit that you'll only
see if you run this passing JSON that is syntactically valid, but
doesn't have the expected shape (namely, fields of `"email"` and
`"name"` that are both strings).

In the TS program, this will not trip an error condition, but will
instead print an incorrect name and email. The Roc program will
print that the JSON is invalid, because it will have detected that
problem right when it tried to decode the JSON.

## Concurrent Asynchronous I/O, Destructuring

```typescript
[read, http] = await Promise.all([
    fetch(url).then(res => fromStr(res.text())),
    fs.readFile(input, "utf-8"),
    fs.writeFile(output, contents),
])
```

```roc
{ read, http } = { Result.parallel <- ()
    http: \() -> Http.getUtf8 url |> Result.map fromStr,
    read: \() -> File.readUtf8 input,
    _: \() -> File.writeUtf8 output contents,
}?
```

## Modules

```roc

```

## Imports

## Packages
