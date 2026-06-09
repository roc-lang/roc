A polymorphic numeric **literal** used as a static-dispatch receiver (e.g. `0.to(14)`), whose concrete type is only pinned by downstream context, passes type-checking but then **panics during postcheck monotype lowering** instead of either defaulting the literal or reporting a clean error.

## Reproduction

`app.roc`:

```roc
app [main!] { pf: platform "./platform/main.roc" }

gather : Iter(item) -> List(item)
gather = |iterator| {
    var $list = []
    for item in iterator {
        $list = $list.append(item)
    }
    $list
}

main! : () => List(I32)
main! = || gather(0.to(14))
```

`platform/main.roc`:

```roc
platform ""
    requires {} { main! : () => List(I32) }
    exposes []
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "targets/",
        exe: { arm64mac: ["libhost.a", app], x64mac: ["libhost.a", app] }
    }

main_for_host! : () => List(I32)
main_for_host! = || main!()
```

Then:

```
roc check app.roc      # passes: "No errors found"
roc build --opt=dev --no-link app.roc      # panics (same with --opt=speed)
```

## Expected

Either default the un-pinned numeric literal to a concrete type (so this compiles), or — if it genuinely cannot be resolved — emit a clean type error. It should not reach an `unreachable`/invariant in a post-check stage.

## Actual

```
thread panic: postcheck invariant violated: checked dispatch target return type conflicted with an existing Monotype constraint
src/postcheck/common.zig:56:24            in invariant
src/postcheck/monotype/lower.zig:2755:78  in constrainNominalToMono   (the `mono_primitive != primitive` guard)
src/postcheck/monotype/lower.zig:2508     in constrainTypeChildrenToMono
src/postcheck/monotype/lower.zig:2466     in constrainTypeToMono
src/postcheck/monotype/lower.zig:2717     in constrainTypeSpanToMono
src/postcheck/monotype/lower.zig:2791     in constrainNominalToMono
src/postcheck/monotype/lower.zig:2508     in constrainTypeChildrenToMono
src/postcheck/monotype/lower.zig:2475     in constrainTypeToMono
src/postcheck/monotype/lower.zig:4802     in targetReturnTypeFromPlan
src/postcheck/monotype/lower.zig:4704     in instantiateTargetFromPlan
src/postcheck/monotype/lower.zig:6427     in methodTargetMonoTypeFromPlan
src/postcheck/monotype/lower.zig:6078     in lowerDispatchExprAtType
...
```

## Analysis

The receiver `0` starts as an unresolved numeric type variable. Static dispatch (`.to`) resolves against it, and the constraint that would pin it to `I32` only arrives from downstream context (the `List(I32)` return type flowing through `gather`). When the still-un-pinned numeric var reaches `targetReturnTypeFromPlan` → `constrainNominalToMono`, two different concrete primitives get stamped into the same monotype slot and the `mono_primitive != primitive` guard fires.

This is the same root cause as the un-defaulted-flex-var-in-dispatch family (cf. #9485). Note it has two faces depending on the surrounding code: in some shapes the checker catches it first and reports a clean `MISSING METHOD` ("dispatch ... on an unresolved type variable"); in the shape above, check passes and lowering panics.

## Workaround

Pin the receiver's type so dispatch resolves at check time:

```roc
main! = || {
    lo : I32
    lo = 0
    hi : I32
    hi = 14
    gather(lo.to(hi))
}
```
