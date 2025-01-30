# Ambient Lambda Set Specialization

Ayaz Hafiz

## Summary

This document describes how polymorphic lambda sets are specialized and resolved in the compiler's type solver.

TL;DR: lambda sets are resolved by unifying their ambient arrow types in a “bottom-up” fashion.

## Background

In this section I’ll explain how lambda sets and specialization lambda sets work today, mostly from the ground-up. I’ll gloss over a few details and assume an understanding of type unification. The background will leave us with a direct presentation of the current specialization lambda set unification algorithm, and its limitation.

Lambda sets are a technique Roc uses for static dispatch of closures. For example,

```jsx
id1 = \x -> x
id2 = \x -> x
f = if True then id1 else id2
```

has the elaboration (solved-type annotations)

```jsx
id1 = \x -> x
^^^ id1 : a -[[id1]] -> a
id2 = \x -> x
^^^ id2 : a -[[id2]] -> a
f = if True then id1 else id2
^ f : a -[[id1, id2]] -> a
```

The syntax `-[[id1]]->` can be read as “a function that dispatches to `id1`". Then the arrow `-[[id1, id2]]->` then is “a function that dispatches to `id1`, or `id2`". The tag union `[id1, id2]` can contain payloads to represent the captures of `id1` and `id2`; however, the implications of that are out of scope for this discussion, see [Folkert’s great explanation](https://github.com/roc-lang/roc/pull/2307#discussion_r777042512) for more information. During compile-time, Roc would attach a run-time examinable tag to the value in each branch of the `f` expression body, representing whether to dispatch to `id1` or `id2`. Whenever `f` is dispatched, that tag is examined to determine exactly which function should be dispatched to. This is “**defunctionalization**”.

In the presence of [abilities](https://docs.google.com/document/d/1kUh53p1Du3fWP_jZp-sdqwb5C9DuS43YJwXHg1NzETY/edit), lambda sets get more complicated. Now, I can write something like

```jsx
Hash has hash : a -> U64 | a has Hash

zeroHash = \_ -> 0

f = if True then hash else zeroHash
```

The elaboration of `f` as `f : a -[[hash, zeroHash]]-> U64` is incorrect, in the sense that it is incomplete - `hash` is not an actual definition, and we don’t know exactly what specialization of `hash` to dispatch to until the type variable `a` is resolved. This elaboration does not communicate to the code generator that the value of `hash` is actually polymorphic over `a`.

To support polymorphic values in lambda sets, we use something we call “**specialization lambda sets**”. In this technique, the lambda under the only arrow in `hash` is parameterized on (1) the type variable the `hash` specialization depends on, and (2) the “region” in the type signature of the specialization that the actual type should be recovered from.

That was a lot of words, so let me give you an example. To better illustrate how the mechanism works, let’s suppose `Hash` is actually defined as `Hash has hashThunk : a -> ({} -> U64) | a has Hash`. Now let’s consider the following program elaboration:

```jsx
Hash has
  hashThunk : a -> ({} -> U64) | a has Hash
# ^^^^^^^^^ a -[[] + a:hashThunk:1]-> ({} -[[] + a:hashThunk:2]-> U64)

zeroHash = \_ -> \{} -> 0
#^^^^^^^   a -[[zeroHash]]-> \{} -[[lam1]]-> U64

f = if True then hash else zeroHash
#^ a -[[zeroHash] + a:hashThunk:1]-> ({} -[[lam1] + a:hashThunk:2]-> U64)
```

The grammar of a lambda set is now

```jsx
lambda_set: [[(concrete_lambda)*] (+ specialization_lambda)*]

concrete_lambda: lambda_name ( capture_type)*
specialization_lambda: <type>:ability_member_name:region
region: <number>
```

Since `hashThunk` is a specification for an ability member and not a concrete implementation, it contains only specialization lambdas, in this case parameterized over `a`, which is the type parameter that implementors of `Hash` must specialize. Since `hashThunk` has two function types, we need to distinguish how they should be resolved. For this reason we record a “region” noting where the specialization lambda occurs in an ability member signature. When `a` is resolved to a concrete type `C`, we would resolve `C:hashThunk:2` by looking up the lambda set of `C`'s specialization of `hashThunk`, at region 2.

`zeroHash` is a concrete implementation, and uses only concrete lambdas, so its two lambda sets are fully resolved with concrete types. I’ve named the anonymous lambda `\{} -> 0` `lam1` for readability.

At `f`, we unify the function types in both branches. Unification of lambda sets is basically a union of both sides’ concrete lambdas and specialization lambdas (this is not quite true, but that doesn’t matter here). This way, we preserve the fact that how `f` should be dispatched is parameterized over `a`.

Now, let’s say we apply `f` to a concrete type, like

```jsx
Foo := {}
hashThunk = \@Foo {} -> \{} -> 1
#^^^^^^^^   Foo -[[Foo#hashThunk]]-> \{} -[[lam2]]-> U64

f (@Foo {})
```

The unification trace for the call `f (@Foo {})` proceeds as follows. I use `'tN`, where `N` is a number, to represent fresh unbound type variables. Since `f` is a generalized type, `a'` is the fresh type “based on `a`" created for a particular usage of `f`.

```text
  typeof f
~ Foo -'t1-> 't2
=>
  a'  -[[zeroHash] + a':hashThunk:1]-> ({} -[[lam1] + a':hashThunk:2]-> U64)
~ Foo -'t1->                           't2
=>
  Foo -[[zeroHash] + Foo:hashThunk:1]-> ({} -[[lam1] + Foo:hashThunk:2]-> U64)
```

Now that the specialization lambdas’ type variables point to concrete types, we can resolve the concrete lambdas of `Foo:hashThunk:1` and `Foo:hashThunk:2`. Cool! Let’s do that. We know that

```text
hashThunk = \@Foo {} -> \{} -> 1
#^^^^^^^^   Foo -[[Foo#hashThunk]]-> \{} -[[lam2]]-> U64
```

So `Foo:hashThunk:1` is `[[Foo#hashThunk]]` and `Foo:hashThunk:2` is `[[lam2]]`. Applying that to the type of `f` we get the trace

```text
  Foo -[[zeroHash] + Foo:hashThunk:1]-> ({} -[[lam1] + Foo:hashThunk:2]-> U64)
<specialization time>
  Foo:hashThunk:1 -> [[Foo#hashThunk]]
  Foo:hashThunk:2 -> [[lam2]]
=>
  Foo -[[zeroHash, Foo#hashThunk]]-> ({} -[[lam1, lam2]] -> U64)
```

Great, so now we know our options to dispatch `f` in the call `f (@Foo {})`, and the code-generator will insert tags appropriately for the specialization definition of `f` where `a = Foo` knowing the concrete lambda symbols.

## The Problem

This technique for lambda set resolution is all well and good when the specialized lambda sets are monomorphic, that is, they contain only concrete lambdas. So far in our development of the end-to-end compilation model that’s been the case, and when it wasn’t, there’s been enough ambient information to coerce the specializations to be monomorphic.

Unfortunately we cannot assume that the specializations will be monomorphic in general, and we must now think about how to deal with that. I didn’t think there was any good, efficient solution, but now we have no option other than to come up with something, so this document is a description of my attempt. But before we get there, let’s whet our appetite for what the problem even is. I’ve been waving my hands too long.

Let’s consider the following program:

```python
F has f : a -> (b -> {}) | a has F, b has G
#     ^ a -[[] + a:f:1]-> (b -[[] + a:f:2]-> {}) | a has F, b has G

G has g : b -> {} | b has G
#     ^ b -[[] + b:g:1]-> {}

Fo := {}
f = \@Fo {} -> g
#^  Fo -[[Fo#f]]-> (b -[[] + b:g:1]-> {}) | b has G
#   instantiation with a=Fo of
#   a -[[] + a:f:1]-> (b -[[] + a:f:2]-> {}) | a has F, b has G

Go := {}
g = \@Go {} -> {}
#^  Go -[[Go#g]]-> {}
#   instantiation with b=Go of
#   b -[[] + b:g:1]-> {}
```

Apologies for the complicated types, I know this can be a bit confusing. It helps to look at the specialized types of `f` and `g` relative to the ability member signatures.

The key thing to notice here is that `Fo#f` must continue to vary over `b | b has G`, since it can only specialize the type parameter `a` (in this case, it specialized it to `Fo`). Its return value is the unspecialized ability member `g`, which has type `b -> {}`, as we wanted. But its lambda set **also** varies over `b`, being `b -[[] + b:g:1]-> {}`.

Suppose we have the call

```python
(f (@Fo {})) (@Go {})
```

With the present specialization technique, unification proceeds as follows:

```text
== solve (f (@Fo {})) ==
  typeof f
~ Fo -'t1-> 't2

   a' -[[] + a':f:1]-> (b' -[[] + a':f:2]-> {})
~  Fo -'t1->           't2
=> Fo -[[] + Fo:f:1]-> (b' -[[] + Fo:f:2]-> {})
   <specialization time>
      Fo:f:1 -> [[Fo#f]]
      Fo:f:2 -> [[] + b'':g:1]    | This is key bit 1!
=> Fo -[[Fo#f]]-> (b' -[[] + b'':g:1] -> {})

== solve (f (@Fo {})) (@Go {}) ==
  return_typeof f
~ Go -'t3-> 't4
                               -
   b' -[[] + b'':g:1] -> {}    | This is key bit 2!
~  Go -'t3->             't4   |
=> Go -[[] + b'':g:1] -> {}    |
                               -

== final type of f ==
f : Fo -[[Fo#f]]-> (Go -[[] + b'':g:1]-> {})
```

Let's go over what happened. The important pieces are the unification traces I’ve annotated as “key bits”.

In resolving `Fo:f:2`, we pulled down the let-generalized lambda set `[[] + b:g:2]` at that region in `Fo`, which means we have to generate a fresh type variable for `b` for that particular instantiation of the lambda set. That makes sense, that’s how let-generalization works. So, we get the lambda set `[[] + b'':g:1]` for our particular instance.

But in key bit 2, we see that we know what we want `b''` to be! We want it to be this `b'`, which gets instantiated to `Go`. But `b'` and `b''` are independent type variables, and so unifying `b' ~ Go` doesn’t solve `b'' = Go`. Instead, `b''` is now totally unbound, and in the end, we get a type for `f` that has an unspecialized lambda set, even though you or I, staring at this program, know exactly what `[[] + b'':g:1]` should really be - `[[Go#g]]`.

So where did we go wrong? Well, our problem is that we never saw that `b'` and `b''` should really be the same type variable. If only we knew that in this specialization `b'` and `b''` are the same instantiation, we’d be all good.

## A Solution

I’ll now explain the best way I’ve thought of for us to solve this problem. If you see a better way, please let me know! I’m not sure I love this solution, but I do like it a lot more than some other naive approaches.

Okay, so first we’ll enumerate some terminology, and the exact algorithm. Then we’ll illustrate the algorithm with some examples; my hope is this will help explain why it must proceed in the way it does. We’ll see that the algorithm depends on a few key invariants; I’ll discuss them and their consequences along the way. Finally, we’ll discuss a couple details regarding the algorithm not directly related to its operation, but important to recognize. I hope then, you will tell me where I have gone wrong, or where you see a better opportunity to do things.

### The algorithm

#### Some definitions

- **The region invariant.** Previously we discussed the “region” of a lambda set in a specialization function definition. The way regions are assigned in the compiler follows a very specific ordering and holds a invariant we’ll call the “region invariant”. First, let’s define a procedure for creating function types and assigning regions:

    ```text
    Type = \region ->
      (Type_atom, region)
    | Type_function region

    Type_function = \region ->
      let left_type, new_region = Type (region + 1)
      let right_type, new_region = Type (new_region)
      let func_type = left_type -[Lambda region]-> right_type
      (func_type, new_region)
    ```

    This procedure would create functions that look like the trees(abbreviating `L=Lambda`, `a=atom` below)

    ```text
     -[L 1]->
    a         a

    ===

              -[L 1]->
     -[L 2]->          -[L 3]->
    a        a        a        a

    ===
                               -[L 1]->
              -[L 2]->                            -[L 5]->
     -[L 3]->          -[L 4]->          -[L 6]->          -[L 7]->
    a        a        a        a        a        a        a        a
    ```

    The invariant is this: for a region `r`, the only functions enclosing `r` have a region number that is less than `r`. Moreover, every region `r' < r`, either the function at `r'` encloses `r`, or is disjoint from `r`.

- **Ambient functions.** For a given lambda set at region `r`, any function that encloses `r` is called an **ambient function** of `r`. The function directly at region `r` is called the **directly ambient function**.

    For example, the functions identified by `L 4`, `L 2`, and `L 1` in the last example tree above are all ambient functions of the function identified by `L 4`.

    The region invariant means that the only functions that are ambient of a region `r` are those identified by regions `< r`.

- `uls_of_var`. A look aside table of the unspecialized lambda sets (uls) depending on a variable. For example, in `a -[[] + a:f:1]-> (b -[[] + a:f:2]-> {})`, there would be a mapping of `a => { [[] + a:f:1]; [[] + a:f:2] }`. When `a` gets instantiated with a concrete type, we know that these lambda sets are ready to be resolved.

#### Explicit Description

The algorithm concerns what happens during the lambda-set-specialization-time. You may want to read it now, but it’s also helpful to first look at the intuition below, then the examples, then revisit the explicit algorithm description.

Suppose a type variable `a` with `uls_of_var` mapping `uls_a = {l1, ... ln}` has been instantiated to a concrete type `C`. Then,

1. Let each `l` in `uls_a` be of form `[concrete_lambdas + ... + C:f:r + ...]`. It has to be in this form because of how `uls_of_var` is constructed.
    1. Note that there may be multiple unspecialized lambdas of form `C:f:r, C:f1:r1, ..., C:fn:rn` in `l`. In this case, let `t1, ... tm` be the other unspecialized lambdas not of form `C:_:_`, that is, none of which are now specialized to the type `C`. Then, deconstruct `l` such that `l' = [concrete_lambdas + t1 + ... + tm + C:f:r` and `l1 = [[] + C:f1:r1], ..., ln = [[] + C:fn:rn]`. Replace `l` with `l', l1, ..., ln` in `uls_a`, flattened.
2. Now, each `l` in `uls_a` has a unique unspecialized lambda of form `C:f:r`. Sort `uls_a` primarily by `f` (arbitrary order), and secondarily by `r` in descending order. This sorted list is called `uls_a'`.
    1. That is, we are sorting `uls_a` so that it is partitioned by ability member name of the unspecialized lambda sets, and each partition is in descending order of region.
    2. An example of the sort would be `[[] + C:foo:2], [[] + C:bar:3], [[] + C:bar:1]`.
3. For each `l` in `uls_a'` with unique unspecialized lambda `C:f:r`:
    1. Let `t_f1` be the directly ambient function of the lambda set containing `C:f:r`. Remove `C:f:r` from `t_f1`'s lambda set.
        1. For example, `(b' -[[] + Fo:f:2]-> {})` if `C:f:r=Fo:f:2`. Removing `Fo:f:2`, we get `(b' -[[]]-> {})`.
    2. Let `t_f2` be the directly ambient function of the specialization lambda set resolved by `C:f:r`.
        1. For example, `(b -[[] + b:g:1]-> {})` if `C:f:r=Fo:f:2`, running on example from above.
    3. Unify `t_f1 ~ t_f2`.

#### Intuition

The intuition is that we walk up the function type being specialized, starting from the leaves. Along the way we pick up bound type variables from both the function type being specialized, and the specialization type. The region invariant makes sure we thread bound variables through an increasingly larger scope.

### Some Examples

#### The motivating example

Recall the program from our problem statement

```python
F has f : a -> (b -> {}) | a has F, b has G
#     ^ a -[[] + a:f:1]-> (b -[[] + a:f:2]-> {}) | a has F, b has G

G has g : b -> {} | b has G
#     ^ b -[[] + b:g:1]-> {}

Fo := {}
f = \@Fo {} -> g
#^  Fo -[[Fo#f]]-> (b -[[] + b:g:1]-> {}) | b has G
#   instantiation with a=Fo of
#   a -[[] + a:f:1]-> (b -[[] + a:f:2]-> {}) | a has F, b has G

Go := {}
g = \@Go {} -> {}
#^  Go -[[Go#g]]-> {}
#   instantiation with b=Go of
#   b -[[] + b:g:1]-> {}
```

With our algorithm, the call

```python
(f (@Fo {})) (@Go {})
```

has unification proceed as follows:

```text
== solve (f (@Fo {})) ==
  typeof f
~ Fo -'t1-> 't2

   a' -[[] + a':f:1]-> (b' -[[] + a':f:2]-> {})
~  Fo -'t1->           't2
=> Fo -[[] + Fo:f:1]-> (b' -[[] + Fo:f:2]-> {})
   <specialization time>
      step 1:
        uls_Fo =  { [[] + Fo:f:1], [[] + Fo:f:2] }
      step 2 (sort):
        uls_Fo' = { [[] + Fo:f:2], [[] + Fo:f:1] }
      step 3:
        1. iteration: [[] + Fo:f:2]
             b'  -[[]]-> {}             (t_f1 after removing Fo:f:2)
           ~ b'' -[[] + b'':g:1]-> {}
           = b'' -[[] + b'':g:1]-> {}
        => typeof f now Fo -[[] + Fo:f:1]-> (b'' -[[] + b'':g:1]-> {})

        2. iteration: [[] + Fo:f:1]
             Fo -[[]]-> (b'' -[[] + b'':g:1]-> {})        (t_f1 after removing Fo:f:1)
           ~ Fo -[[Fo#f]]-> (b''' -[[] + b''':g:1]-> {})
           = Fo -[[Fo#f]]-> (b''' -[[] + b''':g:1]-> {})

   => typeof f = Fo -[[Fo#f]]-> (b''' -[[] + b''':g:1]-> {})

== solve (f (@Fo {})) (@Go {}) ==
  return_typeof f
~ Go -'t3-> 't4

   b''' -[[] + b''':g:1]-> {}
~  Go   -'t3->             't4
=> Go -[[] + Go:g:1] -> {}
   <specialization time>
      step 1:
        uls_Go =  { [[] + Go:g:1] }
      step 2 (sort):
        uls_Go' = { [[] + Go:g:1] }
      step 3:
        1. iteration: [[] + Go:g:1]
             Go -[[]]-> {}     (t_f1 after removing Go:g:1)
           ~ Go -[[Go#g]]-> {}
           = Go -[[Go#g]]-> {}

   => typeof f = Fo -[[Fo#f]]-> (Go -[[Go#g]]-> {})

== final type of f ==
f : Fo -[[Fo#f]]-> (Go -[[Go#g]]-> {})
```

There we go. We’ve recovered the specialization type of the second lambda set to `Go#g`, as we wanted.

#### The motivating example, in the presence of let-generalization

Suppose instead we let-generalized the motivating example, so it was a program like

```coffee
h = f (@Fo {})
h (@Go {})
```

`h` still gets resolved correctly in this case. It’s basically the same unification trace as above, except that after we find out that

```text
typeof f = Fo -[[Fo#f]]-> (b''' -[[] + b''':g:1]-> {})
```

we see that `h` has type

```text
b''' -[[] + b''':g:1]-> {}
```

We generalize this to

```text
h : c -[[] + c:g:1]-> {}
```

Then, the call `h (@Go {})` has the trace

```text
=== solve h (@Go {}) ===
  typeof h
~ Go -'t1-> 't2

   c' -[[] + c':g:1]-> {}
~  Go -'t1->           't2
=> Go -[[] + Go:g:1]-> {}
   <specialization time>
      step 1:
        uls_Go =  { [[] + Go:g:1] }
      step 2 (sort):
        uls_Go' = { [[] + Go:g:1] }
      step 3:
        1. iteration: [[] + Go:g:1]
             Go -[[]]-> {}     (t_f1 after removing Go:g:1)
           ~ Go -[[Go#g]]-> {}
           = Go -[[Go#g]]-> {}
   => Go -[[Go#g]]-> {}
```

#### Bindings on the right side of an arrow

This continues to work if instead of a type variable being bound on the left side of an arrow, it is bound on the right side. Let’s see what that looks like. Consider

```python
F has f : a -> ({} -> b) | a has F, b has G
G has g : {} -> b | b has G

Fo := {}
f = \@Fo {} -> g
#^  Fo -[[Fo#f]]-> ({} -[[] + b:g:1]-> b) | b has G
#   instantiation with a=Fo of
#   a -[[] + a:f:1]-> ({} -[[] + a:f:2]-> b) | a has F, b has G

Go := {}
g = \{} -> @Go {}
#^  {} -[[Go#g]]-> Go
#   instantiation with b=Go of
#   {} -[[] + b:g:1]-> b
```

This is symmetrical to the first example we ran through. I can include a trace if you all would like, though it could be helpful to go through yourself and see that it would work.

#### Deep specializations and captures

Alright, bear with me, this is a long and contrived one, but it demonstrates how this works in the presence of polymorphic captures (it’s “nothing special”), and more importantly, why the bottom-up unification is important.

Here’s the source program:

```python
F has f : a, b -> ({} -> ({} -> {})) | a has F, b has G
#     ^ a, b -[[] + a:f:1]-> ({} -[[] + a:f:2]-> ({} -[[] + a:f:3]-> {})) | a has F, b has G
G has g : b -> ({} -> {}) | b has G
#     ^ b -[[] + b:g:1]-> ({} -[[] + b:g:2]-> {}) | b has G

Fo := {}
f = \@Fo {}, b -> \{} -> g b
#^  Fo, b -[[Fo#f]]-> ({} -[[lamF b]]-> ({} -[[] + b:g:2]]-> {})) | b has G
#   instantiation with a=Fo of
#   a, b -[[] + a:f:1]-> ({} -[[] + a:f:2]-> ({} -[[] + a:f:3]-> {})) | a has F, b has G

Go := {}
g = \@Go {} -> \{} -> {}
#^  {} -[[Go#g]]-> ({} -[[lamG]]-> {})
#   instantiation with b=Go of
#   b -[[] + b:g:1]-> ({} -[[] + b:g:2]-> {}) | b has G
```

Here is the call we’re going to trace:

```python
(f (@Fo {}) (@Go {})) {}
```

Let’s get to it.

```text
=== solve (f (@Fo {}) (@Go {})) ===
  typeof f
~ Fo, Go -'t1-> 't2

   a,  b  -[[] + a:f:1]-> ({} -[[] + a:f:2]-> ({} -[[] + a:f:3]-> {}))
~  Fo, Go -'t1->          't2
=> Fo, Go -[[] + Fo:f:1]-> ({} -[[] + Fo:f:2]-> ({} -[[] + Fo:f:3]-> {}))
   <specialization time>
      step 1:
        uls_Fo = { [[] + Fo:f:1], [[] + Fo:f:2], [[] + Fo:f:3] }
      step 2:
        uls_Fo = { [[] + Fo:f:3], [[] + Fo:f:2], [[] + Fo:f:1] }   (sorted)
      step_3:
        1. iteration: [[] + Fo:f:3]
             {} -[[]]-> {}             (t_f1 after removing Fo:f:3)
           ~ {} -[[] + b':g:2]]-> {}
           = {} -[[] + b':g:2]-> {}
        => Fo, Go -[[] + Fo:f:1]-> ({} -[[] + Fo:f:2]-> ({} -[[] + b':g:2]-> {}))

        2. iteration: [[] + Fo:f:2]
             {} -[[]]->         ({} -[[] + b':g:2]-> {})   (t_f1 after removing Fo:f:2)
           ~ {} -[[lamF b'']]-> ({} -[[] + b'':g:2]]-> {})
           = {} -[[lamF b'']]-> ({} -[[] + b'':g:2]]-> {})
        => Fo, Go -[[] + Fo:f:1]-> ({} -[[lamF b'']]-> ({} -[[] + b'':g:2]]-> {}))

        3. iteration: [[] + Fo:f:1]
             Fo, Go   -[[]]->     ({} -[[lamF b'']]->  ({} -[[] + b'':g:2]]-> {}))   (t_f1 after removing Fo:f:2)
           ~ Fo, b''' -[[Fo#f]]-> ({} -[[lamF b''']]-> ({} -[[] + b''':g:2]]-> {}))
           = Fo, Go -[[Fo#f]]-> ({} -[[lamF Go]]-> ({} -[[] + Go:g:2]-> {}))
             <specialization time>
                step 1:
                  uls_Go = { [[] + Go:g:2] }
                step 2:
                  uls_Go = { [[] + Go:g:2] } (sorted)
                step_3:
                  1. iteration: [[] + Go:g:2]
                       {} -[[]]-> {} (t_f1 after removing Go:g:2)
                     ~ {} -[[lamG]]-> {}
                     = {} -[[lamG]]-> {}
    => Fo, Go -[[Fo#f]]-> ({} -[[lamF Go]]-> ({} -[[lamG]]-> {}))

== final type of f ==
f : Fo, Go -[[Fo#f]]-> ({} -[[lamF Go]]-> ({} -[[lamG]]-> {}))
```

Look at that! Resolved the capture, and all the lambdas.

Notice that in the first `<specialization time>` trace, had we not sorted the `Fo:f:_` specialization lambdas in descending order of region, we would have resolved `Fo:f:3` last, and not bound the specialized `[[] + b':g:2]` to any `b'` variable. Intuitively, that’s because the variable we need to bind it to occurs in the most ambient function type of all those specialization lambdas: the one at `[[] + Fo:f:1]`

### An important requirement

There is one invariant I have left implicit in this construction, that may not hold in general. (Maybe I left others that you noticed that don’t hold - let me know!). That invariant is that any type variable in a signature is bound in either the left or right hand side of an arrow.

I know what you’re thinking, “of course, how else can you get a type variable?” Well, they have played us for fools. Evil lies in the midst. No sanctity passes unscathed through ad-hoc polymorphism.

```python
Evil has
    getEvil : {} -> a | a has Evil
    eatEvil : a -> ({} -> {}) | a has Evil

f = eatEvil (getEvil {})
```

The type of `f` here is `{} -> [[] + a:eatEvil:2]-> {} | a has Evil`. “Blasphemy!” you cry. Well, you’re totally right, this program is total nonsense. Somehow it’s well-typed, but the code generator can’t just synthesize an `a | a has Evil` out of nowhere.

Well, okay, the solution is actually pretty simple - make this a type error. It’s actually a more general problem with abilities, for example we can type the following program:

```python
Evil has
    getEvil : {} -> a | a has Evil
    eatEvil : a -> {} | a has Evil

f = eatEvil (getEvil {})
```

Now the type variable `a | a has Evil` isn’t even visible on the surface: `f` has type `f : {}`. But it lies in the middle, snuggly between `getEvil` and `eatEvil` where it can’t be seen.

In fact, to us, detecting these cases is straightforward - such nonsense programs are identified when they have type variables that don’t escape to either the front or the back of an exposed type. That’s the only way to do monomorphization - otherwise, we could have values that are pathologically polymorphic, which means they are either unused, or this kind of non-codegen-able case.

How do we make this a type error? A couple options have been considered, but we haven’t settled on anything.

1. One approach, suggested by Richard, is to sort abilities into strongly-connected components and see if there is any zig-zag chain of member signatures in a SCC where an ability-bound type variable doesn’t escape through the front or back. We can observe two things: (1) such SCCs can only exist within a single module because Roc doesn’t have (source-level) circular dependencies and (2) we only need to examine pairs of functions have at least one type variable only appearing on one side of an arrow. That means the worst case performance of this analysis is quadratic in the number of ability members in a module. The downside of this approach is that it would reject some uses of abilities that can be resolved and code-generated by the compiler.
2. Another approach is to check whether generalized variables in a let-bound definition’s body escaped out the front or back of the let-generalized definition’s type (and **not** in a lambda set, for the reasons described above). This admits some programs that would be illegal with the other analysis but can’t be performed until typechecking. As for performance, note that new unbound type variables in a body can only be introduced by using a let-generalized symbol that is polymorphic. Those variables would need to be checked, so the performance of this approach on a per-module basis is linear in the number of let-generalized symbols used in the module (assuming the number of generalized variables returned is a constant factor).

### A Property that’s lost, and how we can hold on to it

One question I asked myself was, does this still ensure lambda sets can vary over multiple able type parameters? At first, I believed the answer was yes — however, this may not hold and be sound. For example, consider

```python
J has j : j -> (k -> {}) | j has J, k has K
K has k : k -> {} | k has K

C := {}
j = \@C _ -> k

D := {}
j = \@D _ -> k

E := {}
k = \@E _ -> {}

f = \flag, a, b, c ->
  it = when flag is
    A -> j a
    B -> j b
  it c
```

The first branch has type (`a` has generalized type `a'`)

```text
c'' -[[] + a':j:2]-> {}
```

The second branch has type (`b` has generalized type `b'`)

```text
c''' -[[] + b':j:2]-> {}
```

So now, how do we unify this? Well, following the construction above, we must unify `a'` and `b'` - but this demands that they are actually the same type variable. Is there another option?

Well, one idea is that during normal type unification, we simply take the union of unspecialized lambda sets with **disjoint** variables. In the case above, we would get `c' -[[] + a':j:2 + b':j:2]` (supposing `c` has type `c'`). During lambda set compaction, when we unify ambient types, choose one non-concrete type to unify with. Since we’re maintaining the invariant that each generalized type variable appears at least once on one side of an arrow, eventually you will have picked up all type variables in unspecialized lambda sets.

```text
=== monomorphize (f A (@C {}) (@D {}) (@E {})) ===
(inside f, solving `it`:)

it ~ E -[[] + C:j:2 + D:j:2]-> {}
   <specialization time: C>
      step 1:
        uls_C = { [[] + C:j:2 + D:j:2] }
      step 2:
        uls_C = { [[] + C:j:2 + D:j:2] }   (sorted)
      step_3:
        1. iteration: [[] + C:j:2 + D:j:2]
             E  -[[] + D:j:2]-> {}  (t_f1 after removing C:j:2)
           ~ k' -[[] + k':k:2]-> {}
           = E -[[] + E:k:2 + D:j:2]-> {} (no non-concrete type to unify with)
        => E -[[] + E:k:2 + D:j:2]-> {}
   <specialization time: D>
      step 1:
        uls_D = { [[] + E:k:2 + D:j:2] }
      step 2:
        uls_D = { [[] + E:k:2 + D:j:2] }   (sorted)
      step_3:
        1. iteration: [[] + E:k:2 + D:j:2]
             E   -[[] + E:k:2]-> {}  (t_f1 after removing D:j:2)
           ~ k'' -[[] + k'':k:2]-> {}
           = E -[[] + E:k:2 + E:k:2]-> {} (no non-concrete type to unify with)
        => E -[[] + E:k:2 + E:k:2]-> {}
   <specialization time: E>
      step 1:
        uls_E = { [[] + E:k:2], [[] + E:k:2] }
      step 2:
        uls_E = { [[] + E:k:2], [[] + E:k:2] }   (sorted)
      step_3:
        1. iteration: [[] + E:k:2]
             E -[[]]-> {}  (t_f1 after removing E:k:2)
           ~ E -[[lamE]]-> {}
           = E -[[lamE]]-> {}
        => E -[[lamE]]-> {}
    => E -[[lamE]]-> {}

== final type of it ==
it : E -[[lamE]]-> {}
```

The disjointedness is important - we want to unify unspecialized lambdas whose type variables are equivalent. For example,

```coffee
f = \flag, a, c ->
  it = when flag is
    A -> j a
    B -> j a
  it c
```

Should produce `it` having generalized type

```text
c' -[[] + a':j:2]-> {}
```

and not

```text
c' -[[] + a':j:2 + a':j:2]-> {}
```

For now, we will not try to preserve this property, and instead unify all type variables with the same member/region in a lambda set. We can improve the status of this over time.

## Conclusion

Will this work? I think so, but I don’t know. In the sense that, I am sure it will work for some of the problems we are dealing with today, but there may be even more interactions that aren’t clear to us until further down the road.

Obviously, this is not a rigorous study of this problem. We are making several assumptions, and I have not proved any of the properties I claim. However, the intuition makes sense to me, predicated on the “type variables escape either the front or back of a type” invariant, and this is the only approach that really makes sense to me while only being a little bit complicated. Let me know what you think.

## Appendix

### Optimization: only the lowest-region ambient function type is needed

You may have observed that step 1 and step 2 of the algorithm are somewhat overkill, really, it seems you only need the lowest-number region’s directly ambient function type to unify the specialization with. That’s because by the region invariant, the lowest-region’s ambient function would contain every other region’s ambient function.

This optimization is correct with a change to the region numbering scheme:

```python
Type = \region ->
  (Type_atom, region)
| Type_function region

Type_function = \region ->
  let left_type = Type (region * 2)
  let right_type = Type (region * 2 + 1)
  let func_type = left_type -[Lambda region]-> right_type
  func_type
```

Which produces a tree like

```text
                           -[L 1]->
          -[L 2]->                            -[L 3]->
 -[L 4]->          -[L 5]->          -[L 6]->          -[L 7]->
a        a        a        a        a        a        a        a
```

Now, given a set of `uls` sorted in increasing order of region, you can remove all `uls` that have region `r` such that a floored 2-divisor of `r` is another region `r'` of a unspecialized lambda in `uls`. For example, given `[a:f:2, a:f:5, a:f3, a:f:7]`, you only need to keep `[a:f:2, a:f:3]`.

Then, when running the algorithm, you must remove unspecialized lambdas of form `C:f:_` from **all** nested lambda sets in the directly ambient function, not just in the directly ambient function. This will still be cheaper than unifying deeper lambda sets, but may be an inconvenience.

### Testing Strategies

- Quickcheck - the shape of functions we care about is quite clearly defined. Basically just create a bunch of let-bound functions, polymorphic over able variables, use them in an expression that evaluates monomorphically, and check that everything in the monomorphic expression is resolved.
