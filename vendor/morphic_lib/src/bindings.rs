// TODO: These bindings are incomplete
// TODO: Add test for compatibility with `include/morphic.h`

#![allow(clippy::boxed_local)]

use crate::api::*;
use std::{ffi::CString, os::raw::c_char, ptr, slice};

macro_rules! check_err {
    ($expr:expr) => {
        match $expr {
            ::std::result::Result::Ok(val) => val,
            ::std::result::Result::Err(err) => {
                return ::std::boxed::Box::into_raw(::std::boxed::Box::new(err));
            }
        }
    };
}

#[repr(C)]
pub struct RawModName {
    data: *mut u8,
    len: usize,
}

impl RawModName {
    unsafe fn slice<'a>(&self) -> ModName<'a> {
        ModName(slice::from_raw_parts(self.data, self.len))
    }
}

#[repr(C)]
pub struct RawEntryPointName {
    data: *mut u8,
    len: usize,
}

impl RawEntryPointName {
    unsafe fn slice<'a>(&self) -> EntryPointName<'a> {
        EntryPointName(slice::from_raw_parts(self.data, self.len))
    }
}

#[repr(C)]
pub struct RawFuncName {
    data: *mut u8,
    len: usize,
}

impl RawFuncName {
    unsafe fn slice<'a>(&self) -> FuncName<'a> {
        FuncName(slice::from_raw_parts(self.data, self.len))
    }
}

#[repr(C)]
pub struct RawTypeName {
    data: *mut u8,
    len: usize,
}

impl RawTypeName {
    unsafe fn slice<'a>(&self) -> TypeName<'a> {
        TypeName(slice::from_raw_parts(self.data, self.len))
    }
}

#[repr(C)]
pub struct RawCalleeSpecVar {
    data: *mut u8,
    len: usize,
}

impl RawCalleeSpecVar {
    unsafe fn slice<'a>(&self) -> CalleeSpecVar<'a> {
        CalleeSpecVar(slice::from_raw_parts(self.data, self.len))
    }
}

#[repr(C)]
pub struct RawUpdateModeVar {
    data: *mut u8,
    len: usize,
}

impl RawUpdateModeVar {
    unsafe fn slice<'a>(&self) -> UpdateModeVar<'a> {
        UpdateModeVar(slice::from_raw_parts(self.data, self.len))
    }
}

#[repr(C)]
pub struct RawString {
    data: *mut c_char,
}

impl RawString {
    fn new<T: Into<Vec<u8>>>(t: T) -> Self {
        let c_str = CString::new(t).unwrap();
        Self {
            data: c_str.into_raw(),
        }
    }
}

fn raw<T>(t: T) -> *mut T {
    Box::into_raw(Box::new(t))
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_String_Drop(_: Option<Box<RawString>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_Error_Clone(err: &Error) -> Box<Error> {
    Box::new(err.clone())
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_Error_Drop(_: Option<Box<Error>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_Error_Display(self_: *const Error, out: *mut RawString) {
    *out = RawString::new(format!("{}", *self_));
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_TypeDef_Drop(_: Option<Box<TypeDef>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_TypeDefBuilder_Drop(_: Option<Box<TypeDefBuilder>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_TypeDefBuilder_New(out: *mut *mut TypeDefBuilder) {
    *out = raw(TypeDefBuilder::new());
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_TypeDefBuilder_Build(
    self_: Box<TypeDefBuilder>,
    root: TypeId,
    out: *mut *mut TypeDef,
) -> *mut Error {
    *out = raw(check_err!(self_.build(root)));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_TypeDefBuilder_AddNamedType(
    self_: *mut TypeDefBuilder,
    mod_: RawModName,
    type_: RawTypeName,
    out: *mut TypeId,
) {
    *out = (*self_).add_named_type(mod_.slice(), type_.slice());
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_TypeDefBuilder_AddTupleType(
    self_: *mut TypeDefBuilder,
    field_types: *const TypeId,
    field_types_len: usize,
    out: *mut TypeId,
) -> *mut Error {
    *out = check_err!((*self_).add_tuple_type(slice::from_raw_parts(field_types, field_types_len)));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_TypeDefBuilder_AddUnionType(
    self_: *mut TypeDefBuilder,
    variant_types: *const TypeId,
    variant_types_len: usize,
    out: *mut TypeId,
) -> *mut Error {
    *out = check_err!(
        (*self_).add_union_type(slice::from_raw_parts(variant_types, variant_types_len))
    );
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_TypeDefBuilder_AddHeapCellType(
    self_: *mut TypeDefBuilder,
    out: *mut TypeId,
) {
    *out = (*self_).add_heap_cell_type();
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_TypeDefBuilder_AddBagType(
    self_: *mut TypeDefBuilder,
    item_type: TypeId,
    out: *mut TypeId,
) -> *mut Error {
    *out = check_err!((*self_).add_bag_type(item_type));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDef_Drop(_: Option<Box<FuncDef>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_Drop(_: Option<Box<FuncDefBuilder>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_New(out: *mut *mut FuncDefBuilder) {
    *out = raw(FuncDefBuilder::new());
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_Build(
    self_: Box<FuncDefBuilder>,
    arg_type: TypeId,
    ret_type: TypeId,
    root: BlockExpr,
    out: *mut *mut FuncDef,
) -> *mut Error {
    *out = raw(check_err!(self_.build(arg_type, ret_type, root)));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_DeclareContinuation(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    arg_type: TypeId,
    ret_type: TypeId,
    out0: *mut ContinuationId,
    out1: *mut ValueId,
) -> *mut Error {
    let (continuation, value) =
        check_err!((*self_).declare_continuation(block, arg_type, ret_type));
    *out0 = continuation;
    *out1 = value;
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_DefineContinuation(
    self_: *mut FuncDefBuilder,
    continuation: ContinuationId,
    body: BlockExpr,
) -> *mut Error {
    check_err!((*self_).define_continuation(continuation, body));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddJump(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    continuation: ContinuationId,
    arg: ValueId,
    unreachable_result_type: TypeId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_jump(block, continuation, arg, unreachable_result_type));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddBlock(
    self_: *mut FuncDefBuilder,
    out: *mut BlockId,
) {
    *out = (*self_).add_block();
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddUnknownWith(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    args: *const ValueId,
    args_len: usize,
    result_type: TypeId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_unknown_with(
        block,
        slice::from_raw_parts(args, args_len),
        result_type
    ));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddCall(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    callee_spec_var: RawCalleeSpecVar,
    callee_mod: RawModName,
    callee: RawFuncName,
    arg: ValueId,
) -> *mut Error {
    check_err!((*self_).add_call(
        block,
        callee_spec_var.slice(),
        callee_mod.slice(),
        callee.slice(),
        arg
    ));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddChoice(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    cases: *const BlockExpr,
    cases_len: usize,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_choice(block, slice::from_raw_parts(cases, cases_len)));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddTerminate(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    result_type: TypeId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_terminate(block, result_type));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddNewHeapCell(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_new_heap_cell(block));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddTouch(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    heap_cell: ValueId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_touch(block, heap_cell));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddUpdate(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    update_mode_var: RawUpdateModeVar,
    heap_cell: ValueId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_update(block, update_mode_var.slice(), heap_cell));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddUpdateWriteOnly(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    update_mode_var: RawUpdateModeVar,
    heap_cell: ValueId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_update_write_only(block, update_mode_var.slice(), heap_cell));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddEmptyBag(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    item_type: TypeId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_empty_bag(block, item_type));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddBagInsert(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    bag: ValueId,
    to_insert: ValueId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_bag_insert(block, bag, to_insert));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddBagGet(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    bag: ValueId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_bag_get(block, bag));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddBagRemove(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    bag: ValueId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_bag_remove(block, bag));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddMakeTuple(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    field_vals: *const ValueId,
    field_vals_len: usize,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!(
        (*self_).add_make_tuple(block, slice::from_raw_parts(field_vals, field_vals_len))
    );
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddGetTupleField(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    tuple: ValueId,
    field_idx: u32,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_get_tuple_field(block, tuple, field_idx));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddMakeUnion(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    variant_types: *const TypeId,
    variant_types_len: usize,
    variant_idx: u32,
    to_wrap: ValueId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_make_union(
        block,
        slice::from_raw_parts(variant_types, variant_types_len),
        variant_idx,
        to_wrap
    ));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddMakeNamed(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    named_mod: RawModName,
    named: RawTypeName,
    to_wrap: ValueId,
    out: *mut ValueId,
) -> *mut Error {
    *out = check_err!((*self_).add_make_named(block, named_mod.slice(), named.slice(), to_wrap));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddUnwrapNamed(
    self_: *mut FuncDefBuilder,
    block: BlockId,
    named_mod: RawModName,
    named: RawTypeName,
    to_unwrap: ValueId,
    out: *mut ValueId,
) -> *mut Error {
    *out =
        check_err!((*self_).add_unwrap_named(block, named_mod.slice(), named.slice(), to_unwrap));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddNamedType(
    self_: *mut FuncDefBuilder,
    mod_: RawModName,
    type_: RawTypeName,
    out: *mut TypeId,
) {
    *out = (*self_).add_named_type(mod_.slice(), type_.slice());
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddTupleType(
    self_: *mut FuncDefBuilder,
    field_types: *const TypeId,
    field_types_len: usize,
    out: *mut TypeId,
) -> *mut Error {
    *out = check_err!((*self_).add_tuple_type(slice::from_raw_parts(field_types, field_types_len)));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddUnionType(
    self_: *mut FuncDefBuilder,
    variant_types: *const TypeId,
    variant_types_len: usize,
    out: *mut TypeId,
) -> *mut Error {
    *out = check_err!(
        (*self_).add_union_type(slice::from_raw_parts(variant_types, variant_types_len))
    );
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddHeapCellType(
    self_: *mut FuncDefBuilder,
    out: *mut TypeId,
) {
    *out = (*self_).add_heap_cell_type();
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncDefBuilder_AddBagType(
    self_: *mut FuncDefBuilder,
    item_type: TypeId,
    out: *mut TypeId,
) -> *mut Error {
    *out = check_err!((*self_).add_bag_type(item_type));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ModDef_Drop(_: Option<Box<ModDef>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ModDefBuilder_Drop(_: Option<Box<ModDefBuilder>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ModDefBuilder_New(out: *mut *mut ModDefBuilder) {
    *out = raw(ModDefBuilder::new());
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ModDefBuilder_Build(
    self_: Box<ModDefBuilder>,
    out: *mut *mut ModDef,
) -> *mut Error {
    *out = raw(check_err!(self_.build()));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ModDefBuilder_AddNamedType(
    self_: *mut ModDefBuilder,
    name: RawTypeName,
    type_def: Box<TypeDef>,
) -> *mut Error {
    check_err!((*self_).add_named_type(name.slice(), *type_def));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ModDefBuilder_AddFunc(
    self_: *mut ModDefBuilder,
    name: RawFuncName,
    func_def: Box<FuncDef>,
) -> *mut Error {
    check_err!((*self_).add_func(name.slice(), *func_def));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_Program_Drop(_: Option<Box<Program>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ProgramBuilder_Drop(_: Option<Box<ProgramBuilder>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ProgramBuilder_New(out: *mut *mut ProgramBuilder) {
    *out = raw(ProgramBuilder::new());
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ProgramBuilder_Build(
    self_: Box<ProgramBuilder>,
    out: *mut *mut Program,
) -> *mut Error {
    *out = raw(check_err!(self_.build()));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ProgramBuilder_AddMod(
    self_: *mut ProgramBuilder,
    name: RawModName,
    mod_def: Box<ModDef>,
) -> *mut Error {
    check_err!((*self_).add_mod(name.slice(), *mod_def));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ProgramBuilder_AddEntryPoint(
    self_: *mut ProgramBuilder,
    name: RawEntryPointName,
    func_mod: RawModName,
    func: RawFuncName,
) -> *mut Error {
    check_err!((*self_).add_entry_point(name.slice(), func_mod.slice(), func.slice()));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncSpecSolutions_CalleeSpec(
    self_: *const FuncSpecSolutions,
    var: RawCalleeSpecVar,
    out: *mut FuncSpec,
) -> *mut Error {
    *out = check_err!((*self_).callee_spec(var.slice()));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncSpecSolutions_UpdateMode(
    self_: *const FuncSpecSolutions,
    var: RawUpdateModeVar,
    out: *mut UpdateMode,
) -> *mut Error {
    *out = check_err!((*self_).update_mode(var.slice()));
    ptr::null_mut()
}

pub struct FuncSpecIter<'a> {
    iter: Box<dyn Iterator<Item = &'a FuncSpec>>,
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncSpecIter_Next(
    self_: *mut FuncSpecIter,
    item: *mut *const FuncSpec,
) -> bool {
    match (*self_).iter.next() {
        Some(val) => {
            *item = val;
            true
        }
        None => false,
    }
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncSolutions_Specs(
    _self: *const FuncSolutions,
    _out: *mut *mut FuncSpecIter,
) {
    todo!();
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_FuncSolutions_Spec(
    self_: *mut FuncSolutions,
    spec: *const FuncSpec,
    out: *mut *const FuncSpecSolutions,
) -> *mut Error {
    *out = check_err!((*self_).spec(&*spec));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_ModSolutions_FuncSolutions(
    self_: *mut ModSolutions,
    func: RawFuncName,
    out: *mut *const FuncSolutions,
) -> *mut Error {
    *out = check_err!((*self_).func_solutions(func.slice()));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_Solutions_Drop(_: Option<Box<Solutions>>) {}

#[no_mangle]
pub unsafe extern "C" fn Morphic_Solutions_ModSolutions(
    self_: *mut Solutions,
    mod_: RawModName,
    out: *mut *const ModSolutions,
) -> *mut Error {
    *out = check_err!((*self_).mod_solutions(mod_.slice()));
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn Morphic_Solve(
    program: Box<Program>,
    out: *mut *mut Solutions,
) -> *mut Error {
    *out = raw(check_err!(solve(*program)));
    ptr::null_mut()
}
