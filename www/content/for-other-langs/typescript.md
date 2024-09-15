# Roc for TypeScript Programmers

This is not a full [Roc tutorial](/tutorial) but rather a reference point
for how certain things work in Roc compared to TypeScript.

All Roc examples are using the `basic-cli` platform for effects. See
the [full tutorial](/tutorial) for more details on `basic-cli`.

## Functions

In Roc, functions are called with whitespace (spaces, tabs, or newlines) separating their arguments.
No parentheses are needed in function calls.

### Calling functions

Caling a function with one argument:

```typescript
Math.abs(something)
```

```roc
Num.abs something
```

Caling a function with two arguments:

```typescript
Math.max(x, y)
```

```roc
Num.max x y
```

Notice that Roc function calls don't have commas between their arguments!

### Nested function calls

In TypeScript, nested function calls look like this:

```typescript
Math.max(Num.abs(x), Num.abs(y))
```

In Roc, the nested calls need parentheses around them:

```roc
Num.max (Num.abs x) (Num.abs y)
```

Without the parentheses, Roc's compiler would see 4 arguments being passed to `Num.max`:

```roc
Num.max Num.abs x Num.abs y
```

The parentheses make it clear where nested function calls are intended instead.

### Defining functions

TypeScript has multiple ways to define a function:

```typescript
const increment = (num => num + 1)
const multiplyBy = (num, by) => { num * by }
```
```typescript
function increment(num) { return num + 1 }
function multiplyBy(num, by) { return num * by }
```

Roc has one syntax for defining functions:

```roc
increment = \num -> num + 1
multiplyBy = \num, by -> num * by
```

## Constants and Variables

```typescript
const message = "Hello, World!"
```

```roc
message = "Hello, World!"
```

Note that in Roc, there is no `let` or `var` equiavlent. All declarations with `=` are `const`,
which is why there's no `const` keyword.

### Redeclaration and Reassignment

Here is an example of *redeclaration* - declaring the same constant twice in the same scope - in
both TypeScript and Roc. These two pieces of code are saying the same thing.

```typescript
const message = "Hello,"
const message = "World!"
```

```roc
message = "Hello,"
message = "World!"
```

(These might not look equivalent, but remember that in Roc *whenever* you see `foo =` it's
actually the equivalent of `const foo =` in TypeScript.)

In TypeScript, declaring a `const` with the same name again in the same scope results in an
error saying that *redeclaration* is not allowed.

In Roc, redeclaration is not supported today, but there is a plan to support it in the future.
When it's supported, anything between these two `message =` declarations will evaluate `message`
to `"Hello,"` and anything after the second declaration will evaluate `message` as `"World!"`.

Note that redeclaration is not the same as *reassignment*. Reassignment in TypeScript is where
you have another `message =` without `const` in front of it, like so:

```typescript
const message = "Hello,"
["Foo", "Bar", "Baz"].map(string => message = string)
```

TypeScript does not allow this because it's attempting to *reassign* `message` (using
`message = string`), and constants cannot be reassigned.

Here is some similar Roc code that actually tries to do something slightly different:

```typescript
message = "Hello,"
["Foo", "Bar", "Baz"] |> List.map \string -> message = string
```

In Roc, because `message =` *always* means the TypeScript equivalent of `const message =`,
this Roc code is actually equivalent to te following TypeScript code: (note the second `const`)

```typescript
const message = "Hello,"
["Foo", "Bar", "Baz"].map(string => const message = string)
```

Here there's no reassignment error because the code is not even trying to reassign anything
(since Roc has only constants, it does not even have syntax for attempting to reassign things),
although the code is defining a `const` which is never referenced and is therefore pointless.

### Summary
* TypeScript supports `const`, `let`, and `var`.
* Roc supports only the equivalent of `const`, although there's no `const` keyword in Roc. Instead, whenver you say `foo =` in Roc, it's as if you were saying `const foo =` in TypeScript.
* TypeScript does not allow redeclaration or reassignment of `const` values.
* Roc does not currently support redeclaration (e.g. `const foo =` in TypeScript followed by another `const foo =` in the same scope), but support for redeclaration is planned in the future. Reassignment (changing the value of a `const` after it has been declared) does not exist in Roc.

## Async Effects

In TypeScript, promises can be chained using either `.then` or `await`. Roc's `Task`
(which is used in a similar way to `Promise` in TypeScript, but with some differences)
can be either chained using `Task.andThen` or using `!` syntax
(which is similar to TypeScript's `await` syntax):

### `Promise.then`/`Task.andThen`

```typescript
promise =
  fetch("https://example.com")
    .then(resp => `HTML: ${resp.text()}`)
```

```roc
task =
    Http.getUtf8 "https://roc-lang.org"
    |> Task.andThen \text -> "HTML: $(text)"
```

### `await`/`!`

```typescript
resp = await fetch("https://example.com")
text = `HTML: ${resp.text()}`
```

```roc
utf8 = Http.getUtf8! "https://example.com"
text = "HTML: $(utf8)"
```

### Synchronous Effects

In Roc, all effects are asynchronous. You won't find synchronous effectful functions
like [`localStorage.setItem()`](https://developer.mozilla.org/en-US/docs/Web/API/Storage/setItem)
in the browser or [`fs.ReadSync()`](https://nodejs.org/api/fs.html#fsreadsyncfd-buffer-offset-length-position)
in Node.js.

Note that this is true of *all* effects, not just I/O effects. For example,
[`Math.random()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
does not perform I/O, but it does perform a side effect. Roc doesn't have a
direct equivalent of `Math.random`, but if it did, it would use a `Task` instead
of performing a synchronous side effect.

This design means that:
* Roc does not have a split ecosystem of synchronous and asynchronous effects. They are all consistently async.
* If a Roc function does not involve effects (e.g. return a `Task`), then you know it's a [pure function](https://en.wikipedia.org/wiki/Pure_function).

## Error Handling

```typescript
promise = fetch("https://roc-lang.org/authors")
  .then(response => response.text())
  .then(data => {
    console.log('Response:', data);
  })
  .catch(error => {
    console.error('Error:', error);
  });
```
