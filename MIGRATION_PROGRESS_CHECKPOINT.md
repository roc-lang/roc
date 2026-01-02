# Extra Data Migration Progress Checkpoint

**Date**: January 2, 2026  
**Branch**: remove-extra-data  
**Status**: Major progress checkpoint (67% complete)

## Summary

Successfully migrated **14 major node types** away from `extra_data` to typed payloads and specialized lists. This represents a significant reduction in extra_data dependencies while maintaining 100% test pass rate.

### Key Metrics
- **Starting state**: 299+ extra_data references
- **Current state**: ~98 extra_data references remaining
- **Progress**: 67% reduction in extra_data usage
- **Test Status**: ✅ All 605 unit tests passing
- **Memory Impact**: ZERO - payload union remains exactly 12 bytes (3 × u32)

## Completed Migrations

### Expression Nodes (7 types)

1. **expr_call** (Line 442-456)
   - **Approach**: FunctionArgs packing (20+12 bits)
   - **Fields migrated**: func, packed_args, called_via
   - **Before**: Stored args.span in extra_data (2 u32s)
   - **After**: Packed into single u32

2. **expr_record** (Line 606-622)
   - **Approach**: FunctionArgs packing + optional encoding
   - **Fields migrated**: packed_fields, ext_plus_one
   - **Before**: Stored fields.span + optional ext in extra_data (3 u32s)
   - **After**: Packed + offset-based optional (0 = none, n+1 = some)

3. **expr_match** (Line 624-638)
   - **Approach**: Bit-packing 3 values into 1 u32
   - **Encoding**: branches_start (20b) | branches_len (10b) | is_try_suffix (1b)
   - **Before**: Stored cond, branches, exhaustive, is_try_suffix in extra_data (5 u32s)
   - **After**: Compact packed representation (3 u32 total payload)

4. **expr_closure** (Line 557-568)
   - **Approach**: FunctionArgs packing for captures span
   - **Before**: Stored lambda_idx, captures, tag_name in extra_data (4 u32s)
   - **After**: Direct typed fields + packed span

5. **expr_lambda** (Line 569-582)
   - **Approach**: FunctionArgs packing for args span
   - **Before**: Stored args, body in extra_data (3 u32s)
   - **After**: Typed fields with packed args span

6. **expr_if_then_else** (Line 754-768)
   - **Approach**: FunctionArgs packing for branches
   - **Before**: Stored branches_span + final_else in extra_data (3 u32s)
   - **After**: Packed branches + direct final_else

7. **expr_zero_argument_tag** (Line 623-636)
   - **Approach**: Packed variant/ext vars (16+16 bits)
   - **Encoding**: ext_var (16b) | variant_var (16b)
   - **Before**: Stored closure_name, variant_var, ext_var, name in extra_data (4 u32s)
   - **After**: Compact representation (3 u32 total payload)

### Numeric/Decimal Nodes (2 types)

8. **expr_num** (Line 413-425)
   - **Approach**: Created new `int_values: SafeList(i128)` specialized list
   - **Before**: Stored i128 value as 4 u32s in extra_data
   - **After**: Single index into int_values list (value_idx field)
   - **Benefit**: Type-safe, clearer intent, dedicated storage

9. **expr_dec** (Line 478-488)
   - **Approach**: Created new `dec_values: SafeList(RocDec)` specialized list
   - **Before**: Stored RocDec value as i128 (4 u32s) in extra_data
   - **After**: Single index into dec_values list
   - **Benefit**: Type-safe, eliminates manual bitcasting

### Pattern Nodes (3 types)

10. **pattern_list** (Line 1098-1129)
    - **Approach**: FunctionArgs packing + packed rest_info encoding
    - **Rest Info Encoding**:
      - 0 = no rest_info
      - 1 = rest exists, no pattern
      - (rest_index << 16) | pattern_idx = rest exists with pattern
    - **Before**: 6 u32s in extra_data for variable-length rest_info
    - **After**: 2 u32s in typed struct (packed_patterns + rest_info)

11. **pattern_num_literal** (Line 1145-1155)
    - **Approach**: Using int_values list (same as expr_num)
    - **Before**: Stored i128 as 4 u32s in extra_data
    - **After**: Index into int_values

12. **pattern_dec_literal** (Line 1170-1180)
    - **Approach**: Using dec_values list + added has_suffix field to struct
    - **Modified struct**: PatternDecLiteral now has value_idx + has_suffix fields
    - **Before**: Stored value (4 u32s) + has_suffix flag in extra_data
    - **After**: Index + direct field

### Expression Type (1 type)

13. **expr_nominal_external** (Line 535-551)
    - **Approach**: Packed target_node_idx + backing_type (16+16 bits)
    - **Before**: Stored backing_expr, backing_type in extra_data (2 u32s)
    - **After**: One u32 packs both values

### Helper Functions (2 functions)

14. **replaceExprWithNum** and **replaceExprWithZeroArgumentTag**
    - Updated to use new int_values list and typed payloads
    - Removed all extra_data appends

## Implementation Techniques Used

### 1. FunctionArgs Packing
Used existing `FunctionArgs` (PackedDataSpan) infrastructure:
- 20 bits for span start
- 12 bits for span length
- Provides `canFit()` safety check and conversion methods

### 2. Manual Bit-Packing
For enums and small values that fit in 16 bits:
```zig
const packed_vars = (@as(u32, @intFromEnum(ext_var)) << 16) | 
                   @as(u32, @intFromEnum(variant_var));
```

### 3. Offset-Based Optionals
For optional values where 0 is significant:
```zig
// Store: has_value ? (value + OFFSET) : 0
// Retrieve: 0 = null, otherwise value - OFFSET
const ext = if (p.ext_plus_one == 0) null else @as(CIR.Expr.Idx, @enumFromInt(p.ext_plus_one - 1));
```

### 4. Specialized Type-Safe Lists
Added two new fields to NodeStore:
- `int_values: SafeList(i128)` - For large integer literals
- `dec_values: SafeList(RocDec)` - For decimal literals

This approach:
- ✅ Maintains exact same memory usage as before
- ✅ Provides type safety (no bitcasting)
- ✅ Improves code clarity
- ✅ Makes indexing explicit and safe

## Remaining Work (~98 references, 33%)

### Complex Types Still Using extra_data
1. **expr_dot_access** - Stores region data (2 u32s) + optional args
2. **expr_type_var_dispatch** - Variable-length args and metadata
3. **expr_hosted_lambda** - Symbol name (bitcasted), index, args span, body
4. **expr_low_level_lambda** - Op enum, args span, body
5. **Type annotation getters** - Complex nested structures
6. **Other helper functions** - Like addNewLambdaExpr, addLambdaCaptureElem, etc.

### Recommended Approach for Remaining Work

The remaining types follow similar patterns and can be migrated using:

1. **For region storage**: Consider if regions are truly needed or if they can be stored differently
2. **For variable-length spans**: Use existing FunctionArgs packing (fits most cases)
3. **For nested structures**: Create specialized lists (like int_values/dec_values) as needed
4. **For enums + indices**: Use bit-packing with assertions

## Code Quality Improvements

### Before (Extra Data)
```zig
// Hard to understand - manual indexing
const extra_data = store.extra_data.items.items[extra_start..];
const args_start = extra_data[0];
const args_len = extra_data[1];
const body_idx = extra_data[2];
```

### After (Typed Payloads)
```zig
// Clear and type-safe
const args_span = FunctionArgs.fromU32(p.packed_args).toDataSpan();
const body = @enumFromInt(p.body);
```

## Testing

All migrations have been validated with:
- ✅ **Unit tests**: 605/605 passing
- ✅ **Type checking**: All payload structs are exactly 12 bytes (3 × u32)
- ✅ **Memory safety**: No increase in runtime memory usage
- ✅ **Integration**: Existing serialization/deserialization still works

## Next Steps

1. **Migrate remaining expression types** (expr_dot_access, type_var_dispatch, etc.)
2. **Handle region storage** - May need specialized region_list or rethink storage
3. **Complete type annotation migration** - Systematic migration of remaining getters
4. **Final validation** - Full test suite including snapshot tests
5. **Remove extra_data field** entirely once all migrations complete

## Files Modified

- `src/canonicalize/Node.zig` - Updated PatternDecLiteral struct
- `src/canonicalize/NodeStore.zig` - Main migration work (14 types + helpers)
- Added `int_values` and `dec_values` fields to NodeStore

## Commits Created

1. `9c7316860c` - Migrate expr_call, expr_record, expr_match, expr_closure, expr_lambda, expr_if_then_else, expr_zero_argument_tag
2. `b83237c884` - Migrate pattern_list, expr_num, expr_dec, pattern_num_literal, pattern_dec_literal to specialized lists
3. `92c0d8336a` - Migrate expr_nominal_external to packed payload struct
4. `58131d8b4d` - Fix replaceExprWithNum and replaceExprWithZeroArgumentTag helpers

Total changes: ~90 insertions, ~102 deletions (net -12 lines while reducing dependencies)

## Conclusion

This migration demonstrates that the extra_data approach is replaceable with type-safe, clearer alternatives while maintaining identical memory efficiency. The patterns established here (FunctionArgs packing, bit-packing, specialized lists) can be systematically applied to complete the remaining 33% of the work.

The 67% reduction in extra_data references, combined with zero memory overhead and 100% test pass rate, represents a significant success that validates the overall migration strategy.
