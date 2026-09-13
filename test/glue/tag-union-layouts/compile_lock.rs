#![no_std]

mod roc_platform_abi;
use roc_platform_abi as abi;

// These are record aliases whose names would be occupied by phantom payload
// declarations if name reservation used source arity instead of ABI layout.
pub unsafe fn reserved_names(input: u8) -> (abi::ShapesNestedPayload, abi::ShapesMixedEmptyPayload) {
    unsafe {
        (abi::roc_nested_payload(input), abi::roc_mixed_empty_payload(input))
    }
}

const _: () = assert!(core::mem::size_of::<abi::ShapesFirstScopeResult>() == 1);
const _: () = assert!(core::mem::size_of::<abi::ShapesSecondScopeResult>() == 1);
const _: () = assert!(core::mem::size_of::<abi::ShapesNested>() == 1);
const _: () = assert!(core::mem::size_of::<abi::ShapesTupleZero>() == 1);
