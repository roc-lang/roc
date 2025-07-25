### Plan: Validate and Correct Capture Analysis with Snapshot Tests

The root of the `CaptureNotFound` error appears to be in the capture analysis phase within `src/check/canonicalize.zig`, which incorrectly identifies free variables for closures. Instead of continuing to patch the interpreter, we will first build a robust suite of snapshot tests to validate and guide the correction of the canonicalizer's capture analysis.

Here is the step-by-step plan:

**1. Establish a Dedicated Test Suite for Captures**

*   **Action:** I will create a new directory: `src/snapshots/captures/`.
*   **Purpose:** This directory will contain a focused set of snapshot tests (`.md` files) designed specifically to exercise and validate the closure capture logic. This isolates the problem and provides a clear measure of success.

Here is an example snapshot (before the snapshot tool has generated the other sections).

```md
# META
~~~ini
description=Hello world
type=expr
~~~
# SOURCE
~~~roc
(|y, z| (|x, w| (|a| a + w + x + y + z)(5))(2, 4))(1, 3)
~~~
# EXPECTED
NIL
```

**2. Develop a Comprehensive Set of Snapshot Scenarios**

*   **Action:** I will create individual snapshot files within the new directory for each of the following scenarios. This will ensure we cover a wide range of capture behaviors.

    *   **`simple_capture.md`**: A basic case where a lambda captures one variable from its immediate parent scope.
        ```roc
        {
            x = 5
            y = (|_| x)(1)
            y
        }
        ```
    *   **`no_capture.md`**: A pure lambda that takes an argument and does not capture from its environment.
        ```roc
        (|x| x + 1)(10)
        ```
    *   **`nested_capture.md`**: The core failing scenario. An inner lambda captures a variable defined in an outer lambda's scope.
        ```roc
        {
            f = (|a| |b| a + b)
            g = f(10)
            g(5) # Expect: 15
        }
        ```
    *   **`deeply_nested_capture.md`**: The test case from the original request, involving multiple levels of nesting and local assignments. This will be the ultimate validation.
        ```roc
        (((|a| {
            a_loc = a * 2
            |b| {
                b_loc = a_loc + b
                |c| b_loc + c
            }
        })(100))(20))(3)
        ```
    *   **`argument_shadows_capture.md`**: A lambda argument shadows a variable from an outer scope. The lambda should use the argument, not the captured variable.
        ```roc
        {
            x = 5
            (|x| x)(10) # Should not capture outer `x` -- this should give a shadowing warning
        }
        ```
    *   **`let_shadows_capture.md`**: A `let` binding inside a lambda's body shadows a would-be captured variable.
        ```roc
        {
            x = 5
            y = (|_| { 
                x = 10
                x 
            })({}) # Inner `x` should be used; outer `x` is not captured (it should be a shadowing warning)
            y
        }
        ```
    *   **`capture_from_block.md`**: A lambda within a block expression captures a variable also defined within that block.
        ```roc
        {
            a = 10
            b = (|_| a * 2)(5)
            b
        }
        ```

**3. Generate and Validate Snapshots**

*   **Action:** For each `.md` file, I will run `zig build snapshot -- --update-expected`.
*   **Purpose:** This will generate the initial Canonical Intermediate Representation (CIR) for each test case. I will then meticulously inspect the `(captures ...)` list within each `e_closure` node in the generated snapshots. This allows us to verify precisely which variables the canonicalizer *thinks* are being captured. The discrepancies between the expected and actual captures will pinpoint the exact flaws in the current logic.

**4. Implement and Verify the Fix in `canonicalize.zig`**

*   **Action:** With a clear set of failing tests, I will modify the free-variable analysis in `src/check/canonicalize.zig`, focusing on the `canonicalizeExpr` function for `.lambda` and `.block` expressions.
*   **Algorithm Refinement:** The core of the fix will be to correctly manage the set of "bound" variables as the canonicalizer descends into nested scopes. When exiting a scope (like a lambda's body or a block), the free variables from that scope must be correctly differenced with the variables that were bound within it. This ensures that only truly free variables are propagated upwards and identified as captures.
*   **Iterative Testing:** After each modification, I will re-run the snapshot tests. I will repeat this process until the generated CIR for all snapshot scenarios is correct, confirming that the capture analysis is robust and accurate.

By following this plan, we can fix the capture analysis bug methodically and with high confidence, ensuring the foundation is solid before we return to fixing the interpreter.