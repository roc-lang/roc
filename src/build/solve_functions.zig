const std = @import("std");
const base = @import("../base.zig");
const func_lift = @import("lift_functions.zig");
const collections = @import("../collections.zig");

const Ident = base.Ident;

pub const FunctionSet = struct {
    higher_order_function: Ident.Idx,
    pattern: func_lift.IR.Pattern.Idx,
    data: collections.SafeMultiList(Data),

    pub const Data = struct {
        function_name: Ident.Idx,
        captures: ?func_lift.IR.Type.Slice,
    };

    pub const List = collections.SafeList(@This());
};

/// For every function that takes a function as an argument:
/// - find all functions that can be called from that function
/// - create a function set that tracks said functions for fixing in function specialization
///
/// https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_solve
pub fn solveFunctions(ir: func_lift.IR, other_modules: []FunctionSet.List) FunctionSet.List {
    _ = ir;
    _ = other_modules;

    @panic("not implemented");
}
