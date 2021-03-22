# How To Make Platforms in Roc

A Roc platform consists of two pieces:

1. A pure Roc API, defined in `Pkg-Config.roc`.
2. A *host* implementation backing that API, implemented in a lower-level language than Roc (e.g. C, Zig, Rust, etc.).

Building a platform involves making both of these pieces - so unlike Roc application development, Roc platform devlopement necessarily involves both writing Roc code and also writing code in a different language.

## Design Philosophy

Roc's "platforms and applications" design assumes that one platform will be
used by many application authors. This won't always be the case, but it's the
scenario platforms are designed for.

Becasue of this, whenever there is an opportunity to make life for application
authors better at the expense of more work on the part of platform authors, all
else being equal, that's a good trade to make. The benefit will be felt by a
large number of application authors, while the cost will be felt by a
comparatively small number of platform authors.

One implication of this is the following foundational principle of Roc's design:

### Application authors only need to know Roc

Application authors should only need to interact with the pure Roc API in
a platform, never the host. The host is an implementation detail, and
application authors should not need to know or care what language the
lower-level host is written in.

Additionally, host code should never be built on application authors' machines;
rather, all host code should be built ahead of time, and distributed to
application authors as prebuilt binaries.

A frustrating experience people have in many languages is that they install a
package and get a surprising error about `libsomething` not being found.
"What is `libsomething` and how do I get it? I searched around online and it
seems `libsomething` is a C library. I thought this was Roc, not C - should I
take a detour to learn C now? All I wanted was the Roc thing!"

Roc is designed to prevent this frustrating experience from happening to
application authors. Accordingly, hosts must be prebuilt for different targets;
if you want your platform to be usable on Linux, macOS, and Windows, you'll
need to build host [object file](https://en.wikipedia.org/wiki/Object_file)
binaries for each of those targets. It's a bit of work for the platform author,
but think of all the headaches it will save for the application authors using
your platform!

## The Application compiles to a library

Although we use the term *application* to refer to the portion of a Roc program
which runs on the platform, behind the scenes the application actually compiles
to something more like a library.

For example, let's say I have a Roc platform which consists of:

* A Roc API which requires that the application expose a value called `entrypoint`
* A host written in C

When the Roc compiler compiles the application, here's what happens:

* The application's Roc code gets compiled into a binary object file which exposes a function called `entrypoint`.
* The C host will have [externed](https://en.wikipedia.org/wiki/External_variable) a [symbol](https://en.wikipedia.org/wiki/Symbol_(programming)) called `entrypoint`, and will be calling it at some point in the course of running its code. The Roc compiler now links the compiled object file from the previous step with the precompiled object file for the C host, producing an executable binary with all symbols resolved. When this executable gets run, it will invoke whatever C `main` function the host had written in its C code.

Note that the C host is what actually specifies the `main` function which
the executable will run. The Roc application is essentially compiled down
to a library which the host is free to call...or not!

> The host does not necessarily need to compile to an executable.
> It could very well compile to a library!
> 
> For example, a platform designed to build database extensions might very well
> intend for the output of the compilation to be another binary object file which
> will be picked up by the database. This might require some knowledge of C
> (or at least of the database) on the application author's part, which is fine;
> the key is that the application author should only need to know Roc
> *in order to build their Roc application.* Once the application has been built,
> they may of course need to know other things to fully achieve their goals.

We can describe this relationship concisely as *the application compiles to a library.*
The host is then free to call functions exposed by that library, but ultimately,
the host is in charge!

## Hello World

Let's look at a basic platform that prints whatever `Str` it's given to the console.

Here's a Hello World application which uses this platform:

```elm
app "hello-world"
    packages { base: "platform" }
    imports []
    provides [ main ] to base

main = "Hello, World!"
```

Here's the platform:

```elm
platform examples/hello-world
    requires { main : Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects fx.Effect {}

mainForHost : Str
mainForHost = main
```

Note that `mainForHost` is essentially a copy of `main`; it is passing directly
to the host whatever the application author specified. Sometimes the platform
author will want to do some additional processing on it first, but here we've
decided not to.

Inside `requires` we have a list of values that the application must expose,
and their types. In our case, we just require one: `main`, with the type `Str`.
When the application does `provides [ main ] to base`, it's providing the
values required by this `requires` keyword on the platform side. If we changed
it to `requires { main : Str, init : I64 }`, then the application would change
to something like `provides [ main, foo ] to base` assuming it had `foo : I64`
defined at the top level.

The platform also has a `provides` - in this case, `provides [ mainForHost ]`.
This specifies that `mainForHost` will be provided to the host in the form
of an extern. The externed value will always be a function, even if the Roc code
is not a Roc function. This is because it's possible for the application author
to do something like this:

```elm
main : Str
main = callSomeFunction 1 2 3
```

By compiling `main` (and therefore `mainForHost`) to a function in all cases,
even though its type is `Str`, the host gets to control when `main` actually
gets evaluated. That means if its evaluation takes awhile (e.g. because
`callSomeFunction` is slow), the host can decide when to allow that slowness
rather than being forced into having it happen right when the program starts up.

For that reason, the Roc compiler will always compile this `Str` to a function,
and a C host could access that function from the compiled object binary using
this declaration:

```c
extern void roc__mainForHost_1_exposed(*RocCallResult return_value);
```

There are a few things to note about this:

1. The prefix `roc__` was added to `mainForHost` in order to namespace it as a Roc function within the C global namespace. The Roc compiler automatically adds this `roc__` prefix to everything it externs.
1. The prefix `roc__` was added to `mainForHost` in order to namespace it as a Roc function within the C global namespace. The Roc compiler automatically adds this `roc__` prefix to everything it externs.
2. The suffix `_1` was added. This number exists because behind the scenes, Roc functions can have mulitple specializations - and each specialization gets its own number here. In the case of host authors, though, this number will always be 1. You can pretty much ignore this.
3. The suffix `_exposed` was added. This isn't the only suffix that can appear here; we'll see others later.
4. The function returns [`void`](https://en.wikipedia.org/wiki/Void_type), but takes a [pointer](https://en.wikipedia.org/wiki/Pointer_(computer_programming)) to a `RocCallResult`.

That `RocCallResult` is a C struct which in this case looks like so:

```c
typedef struct RocCallResult {
    uintptr_t flag;
    RocStr content;
} RocCallResult;
```

This struct contains two pieces of information: a flag, and the actual return
value of the function. (In this case, a `RocStr`, which is the host representation
of a Roc `Str`.)

The flag specifies whether the function call succeeded or crashed. Roc code
can in a limited number of circumstances, such as integer overflow or trying
to add so many elements to a list that it exceeds the maximum possible list
length. When one of these exceptional circumstances happens, the Roc function
will return a nonzero `flag` here, and the host can recover however it sees fit.
(If `flag` is nonzero, the rest of the struct is garbage; don't try to use it!)

If `flag` is zero, much like an [exit status](https://en.wikipedia.org/wiki/Exit_status)
code, all is well. So the first thing to do after calling any Roc function from
a host is to check if `flag` is zero, and only proceed to grabbing the actual
return value out of the struct if `flag` is zero.

> Since the `roc__mainForHost_1_exposed` function takes a *pointer* to this
> `RocResult` struct, the host needs to first create one of those (typically
> on the stack; there's no need to [`malloc`](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation)
> it since it will only live as long as the call to the Roc function anyway)
> before passing a pointer to it.

Once we've confirmed that `flag` is zero, we can look at the other piece of
data in `RocCallResult` to get the `Str` the application gave us, and then
print it to the console. *Hello, World!*

Afterwards, we need to clean up that `Str`. Since Roc applications are
essentially compiled to libraries with no persistent runtime, there's no real
way for that application to deallocate the `Str` it allocated. As the author
of the host, that's our responsibility!

> Note that you can always decide intentionally to leak memory and not
> deallocate values you get back from the Roc application. This can be a
> desirable strategy when running a batch script, for example. Of course,
> it's not good to leak memory by accident!

Cleaning up a Roc collection like `Str` is more involved than calling `free`
on it! Roc collections use reference counting, so in the general case, there
may still be active references to that `Str` somewhere - and if there are,
freeing it will result in a [use after free](https://en.wikipedia.org/wiki/Dangling_pointer)!

How to properly deallocate a `RocStr` is outside the scope of this document;
see the `str.zig` file in this repo for more on how this works. You'll want
something similar for a `RocList`, `RocSet`, or `RocDict` too!

## The Rules of Passing

In this example, we had a `mainForHost : Str`, but a different platform might
have had `mainForHost : I32, U8 -> Str` instead. In that case, we'd change the
extern in our C host to something like this:

```c
extern void roc__mainForHost_1_exposed(
    int int_arg,
    char byte_arg,
    *RocCallResult return_value
);
```

We still have the `RocCallResult` struct at the end, but before it are the two
new integer arguments.

Sometimes you'll also pass types which contain pointers, such as a `RocStr`.
Be careful when doing this! The Roc application will assume any pointers you
pass it are safe to dereference, so make sure you only pass pointers that point
to valid memory locations.

Also, be aware that the Roc compiler never generates any
[locking](https://en.wikipedia.org/wiki/Lock_(computer_science)) code, and that
it may not only read from but also write to any pointer you give it. As such,
if you give it a pointer which is being read from other threads, or written to
in other threads, you may end up with
[data races](https://en.wikipedia.org/wiki/Race_condition#Data_race)!

Similar care must be taken with floats.

Roc's standard library is designed never to generate values of `NaN`, `Infinity`,
or `-Infinity`, and as such, standard library functions assume they will never
encounter any of these. If a Roc application encounters one of these, it will
crash at best, or exhibit
[undefined behavior](https://en.wikipedia.org/wiki/Undefined_behavior)
at worst. It is very, very important that you only ever pass finite floats
(that is, floats that are not `NaN`, `Infinity`, or `-Infinity`) to Roc
applications!

In summary, here are the **Rules of Passing** that must be followed
when passing values from the host to the application:

1. Pass only **valid pointers** which won't be read or mutated concurrently from other threads
2. Pass only **finite floats**

Again, these cannot be verified by the Roc compiler, so it is the responsibility
of every host author to take great care to follow these rules manually.

Be careful!

## Passing closures to the host

Passing closures to the host is less straightforward than passing most values.
However, it's important to understand how to do this, because all Roc effects
use closures under the hood!

A Roc closure is a [struct](https://en.wikipedia.org/wiki/Struct_(C_programming_language))
consisting of two parts:

1. A pointer to a function
2. A pointer to any [captured](https://en.wikipedia.org/wiki/Closure_(computer_programming)) data

Inconveniently, the size of the captured data may vary depending on the
application code. For example, if a platform specifies that an application
should provide an `I64 -> I64` function, the host can't possibly know
ahead of time what the size of the captured data will be. One application
might close over nothing at all, another might capture 8 bytes of data,
and another might capture a hundred megabytes. There's no way to know, and
yet the host must be precompiled to a binary object file without this knowledge!

To solve this problem, the Roc compiler will automatically expose additional
symbols to help answer this question. For example, let's suppose I wrote this
in my platform:

```elm
closureToStr : (I64 -> I64 as MyClosure) -> Str
```

This will compile to the following externed symbols:

1. `roc__closureToStr_1_exposed`, a function which takes a closure and returns a `Str`
2. `closureToStr`, a function which takes a closure and returns a `Str`

Note the `as MyClosure` there. This is very important! Whenever passing a closure
to a host - or a type alias for a closure - you must use the `as`


mainForHost :
    {
        init : ({} -> { model: I64 as Model, cmd : (Cmd.Cmd [ Line Str ]) as Fx }) as Init,
        update : ([ Line Str ], I64 -> { model: I64, cmd : Cmd.Cmd [ Line Str ] } ) as Update
    }
mainForHost = main

## Effects are closures

Sometimes
