# Plan for Removing Special-Cased Number Type Representations

## Executive Summary

This plan outlines the incremental removal of all special-cased number type representations from the Roc compiler, including `num_poly`, `int_poly`, `frac_poly`, `num_compact`, `num_unbound`, `int_unbound`, `frac_unbound`, `int_precision`, and `frac_precision`. These were intended as a compiler performance optimization but have proven to be a premature optimization that overcomplicates the compiler.

The removal will allow number types in `Builtin.roc` to be represented as normal nominal types (e.g., `U8`, `I64`, `F32`, `Dec`) throughout the entire compiler pipeline, just like any other user-defined nominal type.

## Core Insight

Currently, the type system has special-case handling for numbers via the `FlatType.num` variant containing a `Num` enum with 9 different cases:
- **Poly forms**: `num_poly(var)`, `int_poly(var)`, `frac_poly(var)` - polymorphic wrappers
- **Unbound forms**: `num_unbound`, `int_unbound`, `frac_unbound` - literals with requirements
- **Precision forms**: `int_precision`, `frac_precision` - partially resolved types
- **Compact form**: `num_compact` - fully resolved efficient representation

After removal, all numbers will be represented uniformly as **nominal types** from `Builtin.roc`:
- Concrete types: `U8`, `I8`, `U16`, `I16`, ..., `U128`, `I128`, `F32`, `F64`, `Dec`
- Number literals: flex vars with a static dispatch constraint:
  ```
  num where [num.from_num_literal : NumLiteral -> Try(num, [InvalidNumLiteral(Str)])]
  ```

This means `FlatType.num` will be completely removed, and number types will use `FlatType.nominal_type` exclusively.

## Important Scope Limitations

**Out of scope for this plan:**
- Literal requirements checking (bits needed, sign needed, etc.)
- Special error messages for literals that don't fit in their annotated type

**Type inference and interpreter support:**
The `from_num_literal` constraint enables type inference - it allows the type system to infer what type a number literal should have. The interpreter handles polymorphic number types by defaulting to `Dec`.

**Interpreter strategy for unresolved polymorphic types:**
When the interpreter encounters an unresolved polymorphic type (a flex var, possibly with static dispatch constraints) during layout resolution:
1. Attempt to unify the type with `Builtin.Num.Dec`
2. If unification fails: crash with an error explaining the incompatibility with `Dec`
3. If unification succeeds: use `Dec` layout and continue

This allows:
- ✅ **Type inference works**: `x = 42` will have type `num where [num.from_num_literal : ...]`
- ✅ **Interpreter works with polymorphic numbers**: Defaults to `Dec` automatically
- ✅ **Type annotations still work**: `x : U8` then `x = 42` gives concrete type `U8`

**Consequence:**
- Tests can use unannotated number literals (they'll default to `Dec`)
- Only tests that specifically need non-`Dec` types need annotations
- This matches the eventual runtime behavior

## Affected Files

Based on comprehensive codebase analysis, here are all files that reference special-cased number types:

### Core Type System (High Priority)
- `src/types/types.zig` - Type definitions (defines `Num` enum)
- `src/types/store.zig` - Type storage and manipulation
- `src/types/instantiate.zig` - Type instantiation
- `src/types/generalize.zig` - Type generalization
- `src/types/TypeWriter.zig` - Type display

### Type Checking (High Priority)
- `src/check/Check.zig` - Main type checker
- `src/check/unify.zig` - Unification logic (has entire `unifyNum` function)
- `src/check/copy_import.zig` - Import copying
- `src/check/snapshot.zig` - Type snapshots for errors

### Builtin Types (High Priority)
- `src/build/roc/Builtin.roc` - Builtin type definitions

### Code Generation (Medium Priority)
- `src/layout/store.zig` - Layout computation
- `src/eval/interpreter.zig` - Interpreter

### Tests (High Priority)
- `src/check/test/unify_test.zig` - Unification tests
- `src/check/test/num_type_inference_test.zig` - Number type inference tests (may need removal/revision)
- `src/check/test/num_type_requirements_test.zig` - Number literal requirements tests (may need removal/revision)
- `src/types/test/test_rigid_instantiation.zig` - Instantiation tests
- `src/layout/store_test.zig` - Layout tests
- `src/canonicalize/test/int_test.zig` - Integer canonicalization tests

### Error Reporting (Medium Priority)
- `crates/reporting/src/error/canonicalize.rs` - Rust error reporting
- `crates/compiler/problem/src/can.rs` - Rust problem types

### Canonicalization (Low Priority)
- `src/canonicalize/Can.zig` - Canonicalization
- `src/canonicalize/CIR.zig` - CIR data structures
- `crates/compiler/can/src/def.rs` - Rust canonicalization

### Other
- `src/eval/mod.zig` - Evaluation module definitions
- `src/check/mod.zig` - Check module definitions

## Incremental Removal Strategy

The strategy is to work **outside-in**: first set up the new infrastructure (NumLiteral + from_num_literal), update the interpreter to default polymorphic types to `Dec`, then replace number literal creation, then remove the special-case infrastructure.

**Key insight**: By having the interpreter default to `Dec` for unresolved polymorphic number types, we avoid needing to annotate all test literals. Tests will continue to work throughout the transition.

---

### Phase 1: Preparation (Setup & Understanding)
**Goal**: Ensure tests pass and establish baseline understanding

**Steps**:

1. **Verify current state**
   ```bash
   zig build snapshot && zig build test
   ```
   Ensure all tests pass before beginning changes.

2. **Document current builtin structure**
   - Verify that `Builtin.roc` defines: `U8`, `I8`, `U16`, `I16`, ..., `F32`, `F64`, `Dec`
   - Understand how builtin types are currently imported and represented
   - Document the module structure for number types

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Tests pass; baseline established.

---

### Phase 2: Add `from_num_literal` Methods to Number Types
**Goal**: Add `from_num_literal` methods to all builtin number types

**File**: `src/build/roc/Builtin.roc`

**Current state**: `NumLiteral` already exists:
```roc
NumLiteral :: [Self(Bool)].{
    is_negative : NumLiteral -> Bool
    # ...
}
```

**Changes needed**:

1. **Add `from_num_literal` method to each number type**:
   ```roc
   U8 :: [].{
       # ... existing methods ...
       from_num_literal : NumLiteral -> Try(U8, [InvalidNumLiteral(Str)])
   }

   I8 :: [].{
       # ... existing methods ...
       from_num_literal : NumLiteral -> Try(I8, [InvalidNumLiteral(Str)])
   }

   # ... repeat for all 13 number types: U8, I8, U16, I16, U32, I32, U64, I64, U128, I128, F32, F64, Dec
   ```

2. **Update auto-imported types** (in the compiler):
   - Verify `NumLiteral` is added to the list of auto-imported types from Builtin
   - This likely happens in canonicalization or the type checker initialization

3. **Verify the constraint format**:
   - Ensure the constraint syntax is correct for the current constraint system
   - The constraint should be: `num.from_num_literal : NumLiteral -> Try(num, [InvalidNumLiteral(Str)])`

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Tests still pass. All number types have `from_num_literal` method signatures.

---

### Phase 3: Update Interpreter to Default Polymorphic Numbers to Dec
**Goal**: Make the interpreter handle unresolved polymorphic number types by defaulting to `Dec`

**Files**:
- `src/layout/store.zig` - Layout computation (where type resolution happens)
- `src/eval/interpreter.zig` - Interpreter

**Changes needed**:

When the layout system encounters an unresolved flex var during layout resolution:

1. **Detect polymorphic number types**: Check if the flex var has a `from_num_literal` static dispatch constraint
2. **Attempt Dec unification**: Try to unify the flex var with `Builtin.Num.Dec`
3. **Handle the result**:
   - If unification succeeds: use `Dec` layout and continue
   - If unification fails: provide a clear error message explaining the type is incompatible with `Dec`

**Implementation approach**:

```zig
// In layout resolution code (src/layout/store.zig)
fn layoutFromVar(self: *Self, var_id: Var) !Layout {
    const content = self.type_store.getContent(var_id);

    switch (content) {
        .flex_var => {
            // Check if this is a polymorphic number (has from_num_literal constraint)
            if (self.hasFromNumLiteralConstraint(var_id)) {
                // Try to unify with Dec
                const dec_var = try self.getBuiltinType("Builtin.Num.Dec");
                if (self.tryUnify(var_id, dec_var)) {
                    // Unification succeeded - use Dec layout
                    return Layout.dec();
                } else {
                    // Unification failed - error
                    return error.PolymorphicNumberIncompatibleWithDec;
                }
            }
            // Regular flex var - error as before
            return error.UnresolvedType;
        },
        // ... rest of cases
    }
}
```

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**:
- Tests still pass (still using old number type creation)
- Interpreter is ready to handle polymorphic numbers
- When we switch to the new approach, tests won't need annotations

---

### Phase 4: Update Number Literal Type Creation
**Goal**: Stop creating special-cased unbound types; create flex vars with static dispatch constraints instead

**File**: `src/check/Check.zig`

**Prerequisites**: Phase 3 must be complete (interpreter can handle polymorphic numbers)

**Current behavior** (around lines 2060-2127):
- Integer literals create `num_unbound` or `int_unbound` wrapped in `num_poly`
- Decimal literals create `frac_unbound` wrapped in `num_poly`

**New behavior**:
- All number literals (integer or decimal) create a flex var with a static dispatch constraint
- Unannotated literals will be inferred and default to `Dec` in the interpreter

**Changes needed**:

1. **For all number literals** (both integer and decimal):
   ```zig
   // When encountering a number literal in a pattern or expression
   const literal_var = try self.fresh(env, literal_region);

   // Add static dispatch constraint: num.from_num_literal : NumLiteral -> Try(num, [InvalidNumLiteral(Str)])
   const constraint = StaticDispatchConstraint{
       .fn_name = try self.cir.getIdentStore().intern("from_num_literal"),
       .fn_var = try self.createFromNumLiteralConstraintType(),
       .origin = .desugared_binop, // or new origin type for literals
   };

   try self.addStaticDispatchConstraint(literal_var, constraint);

   // Return the constrained flex var
   return literal_var;
   ```

2. **Implement `createFromNumLiteralConstraintType()`**:
   ```zig
   fn createFromNumLiteralConstraintType(self: *Self) !Var {
       // Create type: NumLiteral -> Try(num, [InvalidNumLiteral(Str)])
       // where 'num' is a flex var that will unify with the actual number type

       const num_literal_var = try self.getBuiltinType("NumLiteral");
       const result_var = try self.fresh(...);
       const error_tag_var = try self.createInvalidNumLiteralTag();
       const try_var = try self.createTryType(result_var, error_tag_var);

       return try self.createFunctionType(&.{num_literal_var}, try_var);
   }
   ```

3. **Remove all special cases for number literal types**:
   - Delete code that creates `num_unbound`, `int_unbound`, `frac_unbound`
   - Delete code that wraps in `num_poly`, `int_poly`, `frac_poly`

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**:
- Tests still pass because the interpreter defaults unannotated literals to `Dec`
- Literal type creation now uses the new constraint-based approach
- Number literals work with type inference

---

### Phase 5: Replace Concrete Type Creation
**Goal**: Stop creating `num_compact`, `int_precision`, `frac_precision` - create nominal types instead

**File**: `src/check/Check.zig`

**Current code** (lines 1590-1603):
```zig
.u8 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_u8 } }, env, anno_region),
.i8 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_i8 } }, env, anno_region),
// ... etc for all number types
```

**Changes needed**:

1. **Create helper to get builtin number types**:
   ```zig
   fn getBuiltinNumberType(self: *Self, type_name: []const u8) !Var {
       // Look up the type in the Builtin.Num module
       // Return a nominal type variable
       const builtin_module = self.common_idents.builtin_module orelse return error.BuiltinNotFound;

       // Navigate to Builtin.Num.<TypeName>
       const num_ident = try self.cir.getIdentStore().intern("Num");
       const type_ident = try self.cir.getIdentStore().intern(type_name);

       // Look up and return the type
       // ... implementation details ...
   }
   ```

2. **Replace each concrete type case**:
   ```zig
   .u8 => return try self.getBuiltinNumberType("U8"),
   .i8 => return try self.getBuiltinNumberType("I8"),
   .u16 => return try self.getBuiltinNumberType("U16"),
   .i16 => return try self.getBuiltinNumberType("I16"),
   .u32 => return try self.getBuiltinNumberType("U32"),
   .i32 => return try self.getBuiltinNumberType("I32"),
   .u64 => return try self.getBuiltinNumberType("U64"),
   .i64 => return try self.getBuiltinNumberType("I64"),
   .u128 => return try self.getBuiltinNumberType("U128"),
   .i128 => return try self.getBuiltinNumberType("I128"),
   .f32 => return try self.getBuiltinNumberType("F32"),
   .f64 => return try self.getBuiltinNumberType("F64"),
   .dec => return try self.getBuiltinNumberType("Dec"),
   ```

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Concrete type annotations work with nominal types.

---

### Phase 6: Eliminate Num(a), Int(a), Frac(a) Entirely
**Goal**: Remove all traces of `Num(a)`, `Int(a)`, `Frac(a)` - these concepts no longer exist

This is a comprehensive cleanup phase that removes these types from creation, usage, display, and tests.

**Files affected**:
- `src/check/Check.zig` - Type creation
- `src/types/TypeWriter.zig` - Type display
- `src/check/snapshot.zig` - Snapshot serialization
- All test files that use these types
- Any other files that reference these types

**Changes needed**:

#### A. Remove Type Creation (Check.zig)

**Current code** (lines 1638-1700):
- `.num` case creates `num_poly(a)`
- `.int` case creates `num_poly(int_unbound)` wrapper
- `.frac` case creates `num_poly(frac_unbound)` wrapper

**Delete these cases entirely**:
```zig
// DELETE these entire cases from handleBuiltinAnnotation():
.num => { ... }
.int => { ... }
.frac => { ... }
```

If these are used in Builtin.roc or anywhere else as actual type annotations, the compiler will give clear errors about undefined types, which is what we want.

#### B. Remove Type Display (TypeWriter.zig)

Search for any code that displays `Num(...)`, `Int(...)`, or `Frac(...)` and remove it. The type writer should never print these types anymore.

**Search for**:
- Code that writes "Num(" or "Int(" or "Frac(" when displaying types
- Any special-case handling for these polymorphic number types

#### C. Remove from Snapshots (snapshot.zig)

Search for any code that serializes `Num(a)`, `Int(a)`, `Frac(a)` types in snapshots and remove it.

#### D. Update or Delete Tests

**Strategy**: For each test that uses `Num(a)`, `Int(a)`, or `Frac(a)`:

1. **If the test is about general polymorphism** (and just happens to use numbers):
   - Translate to use concrete number types: `U8`, `I64`, etc.
   - Example: `foo : Num(a) -> Num(a)` → `foo : U8 -> U8`

2. **If the test is specifically about number type polymorphism**:
   - Ask: "Does this test still make sense without `Num(a)`?"
   - If it's testing inference that will come back later: **delete the test**
   - If it's testing constraints on number types: **translate to use concrete types**

3. **If the test is about literal inference with `Num(a)`**:
   - **Delete the test** - this functionality doesn't exist yet (future work)

**Example translations**:

Before:
```roc
identity : Num(a) -> Num(a)
identity = |x| x

# Test
identity(42)  # Should infer to some Num type
```

After (translate):
```roc
identity : U8 -> U8
identity = |x| x

# Test
input : U8
input = 42
result = identity(input)
```

Or **delete** if the test was specifically about inference.

**Specific test files to check**:
- `src/check/test/num_type_inference_test.zig` - **Likely delete most/all of this**
- `src/check/test/num_type_requirements_test.zig` - **Likely delete most/all of this**
- `src/check/test/unify_test.zig` - Translate or delete tests using `Num(a)`, etc.
- Any other test files found with `rg "Num\(|Int\(|Frac\("`

**Search commands**:
```bash
# Find all references to Num(, Int(, Frac( in Roc and Zig code
rg "Num\(|Int\(|Frac\(" --type roc --type zig

# Find tests that might be testing number polymorphism
rg "Num\(a\)|Int\(a\)|Frac\(a\)"
```

#### E. Remove from Builtin.roc (if present)

Check if `Builtin.roc` defines or exports `Num`, `Int`, or `Frac` as types that can take arguments. If so, remove those definitions or make them concrete types only.

**Expected Builtin.roc after this phase**:
- ✅ Has: `U8`, `I8`, ..., `F32`, `F64`, `Dec` (concrete types)
- ✅ Has: `NumLiteral` (for literal constraint)
- ❌ Does NOT have: `Num`, `Int`, `Frac` as polymorphic types

#### F. Verify No References Remain

**Search entire codebase**:
```bash
# Should return no results after this phase:
rg "Num\(a\)|Int\(a\)|Frac\(a\)" --type zig --type roc
rg "Num\(|Int\(|Frac\(" --type zig --type roc  # Check for any parameterized usage
```

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**:
- No code references `Num(a)`, `Int(a)`, or `Frac(a)` anywhere
- Type display never shows these types
- Tests are either translated to use concrete types or deleted
- All tests pass

---

### Phase 7: Update Unification for Nominal Number Types
**Goal**: Simplify unification - just use regular nominal type unification

**File**: `src/check/unify.zig`

**Changes needed**:

1. **Remove entire `unifyNum()` function** (lines 1577-2413):
   - This function is no longer needed
   - All number type unification goes through regular nominal type unification

2. **Remove `.num` case from main unify function**:
   ```zig
   // DELETE this case:
   .num => |a_num| {
       // ... entire num unification logic ...
   }
   ```

3. **Ensure nominal type unification handles number types**:
   - Builtin number types should unify like any other nominal type
   - `U8 × U8` → success
   - `U8 × I8` → mismatch error
   - No special handling needed

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Unification works correctly with nominal number types.

---

### Phase 8: Update Type Display
**Goal**: Display nominal types instead of special-cased number representations

**File**: `src/types/TypeWriter.zig`

**Current code** (lines 755-799): Has `writeNum()` function that handles all 9 Num cases.

**Changes needed**:

1. **Remove `writeNum()` function entirely**.

2. **Remove `.num` case from `writeFlatType()`**:
   ```zig
   // DELETE this entire case:
   .num => |num| try self.writeNum(num, root_var),
   ```

3. **Rely on existing `writeNominalType()`** for all number types:
   - `U8` displays as `U8`
   - `I32` displays as `I32`
   - Flex vars with `from_num_literal` constraint display as `<number>` or similar

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Type errors display `U8`, `I32`, etc.

---

### Phase 9: Update Instantiation
**Goal**: Remove number-specific instantiation logic

**File**: `src/types/instantiate.zig`

**Changes needed**:

1. **Remove `instantiateNum()` function**.

2. **Remove `.num` case from `instantiateFlatType()`**:
   ```zig
   // DELETE this case:
   .num => |num| FlatType{ .num = try self.instantiateNum(num) },
   ```

3. **Rely on nominal type instantiation**:
   - Number types are just nominal types - no special handling needed

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Instantiation works correctly.

---

### Phase 10: Update Generalization
**Goal**: Remove number-specific generalization logic

**File**: `src/types/generalize.zig`

**Changes needed**:

1. **Remove num cases from `adjustRank()`**:
   ```zig
   // DELETE this entire case:
   .num => |num| {
       switch (num) {
           .num_poly => |poly_var| ...,
           .int_poly => |poly_var| ...,
           .frac_poly => |poly_var| ...,
           .num_unbound, .int_unbound, .frac_unbound => ...,
           .int_precision, .frac_precision, .num_compact => ...,
       }
   }
   ```

2. **Rely on nominal type generalization**.

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Generalization works correctly.

---

### Phase 11: Update Layout Generation
**Goal**: Generate layouts from nominal number types

**File**: `src/layout/store.zig`

**Current code** (lines 914-957): Unwraps poly/unbound to find concrete types.

**Changes needed**:

1. **Remove `.num` case from `layoutFromFlatType()`**.

2. **Add/update nominal type layout logic**:
   ```zig
   .nominal_type => |nominal| {
       const type_name = self.getTypeName(nominal);

       // Check if it's a builtin number type
       if (std.mem.eql(u8, type_name, "U8")) return Layout.int(.u8);
       if (std.mem.eql(u8, type_name, "I8")) return Layout.int(.i8);
       if (std.mem.eql(u8, type_name, "U16")) return Layout.int(.u16);
       // ... all 13 number types ...
       if (std.mem.eql(u8, type_name, "Dec")) return Layout.dec();

       // Regular nominal type layout
       return try self.layoutForNominalType(nominal);
   }
   ```

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Layout generation works correctly.

---

### Phase 12: Update Copy/Import Logic
**Goal**: Remove number-specific copy logic

**File**: `src/check/copy_import.zig`

**Changes needed**:

1. Search for `num_poly`, `int_poly`, `frac_poly`, `num_compact`, etc.
2. Remove all special-case handling
3. Rely on nominal type copying

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Cross-module imports work correctly.

---

### Phase 13: Update Snapshot Logic
**Goal**: Remove number-specific snapshot logic

**File**: `src/check/snapshot.zig`

**Changes needed**:

1. Remove num-specific snapshot cases (lines 223-250)
2. Rely on nominal type snapshots

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Error snapshots work correctly.

---

### Phase 14: Update Store Utilities
**Goal**: Remove number-specific utility functions

**File**: `src/types/store.zig`

**Changes needed**:

1. **Remove num cases from `needsInstantiation()`** (lines 412-419):
   ```zig
   // DELETE this case:
   .num => |num| switch (num) { ... },
   ```

2. Remove any other number-specific helper functions.

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Type store operations work correctly.

---

### Phase 15: Update Interpreter/Eval
**Goal**: Remove number-specific evaluation logic

**Files**: `src/eval/interpreter.zig`, `src/eval/mod.zig`

**Changes needed**:

1. Search for number type special cases
2. Replace with nominal type handling
3. Ensure evaluation works with nominal number types

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Compile-time evaluation works correctly.

---

### Phase 16: Remove FlatType.num Variant
**Goal**: Delete the num variant from FlatType enum and all related types

**File**: `src/types/types.zig`

**Changes needed**:

1. **Remove from `FlatType` enum** (line 403):
   ```zig
   pub const FlatType = union(enum) {
       str,
       box: Var,
       list: Var,
       // DELETE this line:
       num: Num,
       // ... rest of variants
   }
   ```

2. **Delete entire `Num` enum and related types** (lines 420-842):
   ```zig
   // DELETE all of this:
   pub const Num = union(enum) { ... };
   pub const NumRequirements = struct { ... };
   pub const IntRequirements = struct { ... };
   pub const FracRequirements = struct { ... };
   pub const Int = struct { ... };
   pub const Frac = struct { ... };
   // All helper functions, constants, tests, etc.
   ```

3. **Update documentation**:
   - Remove references to special-cased number types
   - Document that numbers are nominal types

**Test after this phase**:
```bash
zig build snapshot && zig build test
```

**Expected outcome**: Compiler builds successfully without `Num` type. All tests pass.

---

### Phase 17: Update Rust Error Reporting
**Goal**: Update Rust code for new type representation

**Files**:
- `crates/reporting/src/error/canonicalize.rs`
- `crates/compiler/problem/src/can.rs`

**Changes needed**:

1. Search for number type references in error messages
2. Update terminology to use nominal types
3. Remove any special-case error handling for num types
4. Add error for deprecated `Num(a)`, `Int(a)`, `Frac(a)` usage

**Test after this phase**:
```bash
cargo test
zig build snapshot && zig build test
```

**Expected outcome**: Error messages reference nominal types correctly.

---

### Phase 18: Final Cleanup
**Goal**: Remove all remaining references

**Changes needed**:

1. **Search for remnants**:
   ```bash
   rg "num_poly|int_poly|frac_poly|num_compact|num_unbound|int_unbound|frac_unbound|int_precision|frac_precision" --type zig --type rust
   ```

2. **Remove dead code**:
   - Helper functions only used for special-cased num types
   - Comments referencing the old optimization
   - Debug code specific to num types

3. **Update documentation**:
   - README files
   - Code comments
   - Architecture documentation
   - Document the new NumLiteral + from_num_literal approach

**Test after this phase**:
```bash
zig build snapshot && zig build test
cargo test
```

**Expected outcome**: Clean codebase with no special-case number type references.

---

### Phase 19: Performance Verification
**Goal**: Ensure no performance regressions

**Steps**:

1. Run benchmark suite (if available)
2. Compare compile times on real-world projects
3. Check memory usage during type checking
4. Profile hot paths if needed

**Expected outcome**:
- Similar or better performance (simpler code often runs faster)
- If regressions found, profile and optimize

---

## Testing Strategy

After **each phase**, run:
```bash
zig build snapshot && zig build test
```

This ensures:
1. Tests continue to pass (or fail expectedly during transition)
2. Test snapshots are updated incrementally
3. Problems are caught early before they compound

**Important**: Phase 3 (annotating test literals) is critical - without it, tests will fail after Phase 4.

## Rollback Strategy

Each phase should be a separate commit with a clear message. If a phase causes unexpected issues:
1. Attempt to fix within the phase
2. If too complex, revert the commit
3. Reassess approach
4. Try alternative implementation

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Breaking existing code | Incremental approach allows stopping at any point |
| Missing test annotations | Compiler errors will identify all unannotated literals |
| Performance regression | Simpler representation should be faster; profile if needed |
| Complex interactions | Test after each phase; small focused changes |
| Missing edge cases | Comprehensive test suite catches behavioral changes |

## Success Criteria

1. ✅ All tests pass: `zig build test` succeeds
2. ✅ Snapshots updated: `zig build snapshot` succeeds
3. ✅ No special-case num types: `rg "num_poly|num_compact"` returns no results
4. ✅ Nominal types used: All numbers use `FlatType.nominal_type`
5. ✅ NumLiteral infrastructure works: All number types have `from_num_literal` method
6. ✅ Performance maintained: No significant regressions
7. ✅ Simpler codebase: Less special-case logic

## Future Work (Out of Scope)

After this plan is complete, future work can add:
1. Better error messages for invalid number literals
2. Literal requirements checking (bits needed, sign needed)
3. Optimizations for the common case of concrete number types
4. More sophisticated defaulting strategy (e.g., defaulting to smaller types when possible)

## Next Steps

1. Start with **Phase 1** (Preparation)
2. Proceed through phases sequentially
3. Commit after each successful phase
4. Update this document if approach changes
5. Report blockers or unexpected issues

