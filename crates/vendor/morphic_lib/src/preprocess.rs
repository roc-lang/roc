//! Before we perform any interprocedural analysis, we run a preprocessing pass to convert the "API
//! IR" (the IR constructed by the input API) to the "analysis IR" (the IR used for all subsequent
//! analysis).
//!
//! We cannot perform this preprocessing step eagerly while the API IR is being constructed, because
//! some parts of the preprocessing (for example, typechecking) rely on nonlocal information (such
//! as functions' type signatures) which is not available until the entire input program has been
//! constructed.
//!
//! This pass currently implements the following functionality:
//! - Scope checking for values
//! - Typechecking

// https://github.com/morphic-lang/morphic_lib/issues/19
#![allow(clippy::result_large_err)]

use smallvec::{smallvec, SmallVec};

use crate::api;
use crate::ir;
use crate::ir::JumpTarget;
use crate::name_cache::{ConstId, EntryPointId, FuncId, NameCache, NamedTypeId};
use crate::type_cache::{TypeCache, TypeData, TypeId};
use crate::util::id_vec::IdVec;
use crate::util::op_graph::OpGraph;
use crate::util::replace_none::replace_none;

#[derive(Clone, thiserror::Error, Debug)]
pub(crate) enum ErrorKind {
    #[error("could not find named type in module {0:?} with name {1:?}")]
    NamedTypeNotFound(api::ModNameBuf, api::TypeNameBuf),
    #[error("could not find func in module {0:?} with name {1:?}")]
    FuncNotFound(api::ModNameBuf, api::FuncNameBuf),
    #[error("could not find const in module {0:?} with name {1:?}")]
    ConstNotFound(api::ModNameBuf, api::ConstNameBuf),
    #[error("value {0:?} is not in scope")]
    ValueNotInScope(api::ValueId),
    #[error("continuation {0:?} is not in scope")]
    ContinuationNotInScope(api::ContinuationId),
    #[error("expected type '{expected}', found type '{actual}'")]
    TypeMismatch { expected: String, actual: String },
    #[error("expected bag type, found type '{0}'")]
    ExpectedBagType(String),
    #[error("expected tuple type, found type '{0}'")]
    ExpectedTupleType(String),
    #[error("expected union type, found type '{0}'")]
    ExpectedUnionType(String),
    #[error("expected named type, found type '{0}'")]
    ExpectedNamedType(String),
    #[error("tuple field index {0} out of range")]
    TupleFieldOutOfRange(u32),
    #[error("union variant index {0} out of range")]
    UnionVariantOutOfRange(u32),
    #[error("entry point does not have type '() -> ()'")]
    EntryPointDisallowedSig,
}

/// For use in error locations
#[derive(Clone, Debug)]
enum DefName {
    Type(api::TypeNameBuf),
    Func(api::FuncNameBuf),
    Const(api::ConstNameBuf),
    EntryPoint(api::EntryPointNameBuf),
}

#[derive(Clone, Debug)]
enum BindingLocation {
    Type(api::TypeId),
    Value(api::ValueId),
    Continuation(api::ContinuationId),
}

#[derive(Clone, Debug)]
pub(crate) struct Error {
    kind: ErrorKind,
    mod_: Option<api::ModNameBuf>,
    def: Option<DefName>,
    binding: Option<BindingLocation>,
}

impl Error {
    fn annotate_mod_def<E: Into<Self>>(err: E, mod_: api::ModNameBuf, def: DefName) -> Self {
        let mut err = err.into();
        if err.mod_.is_none() {
            err.mod_ = Some(mod_);
        }
        if err.def.is_none() {
            err.def = Some(def);
        }
        err
    }

    fn annotate_type_def<E: Into<Self>>(
        nc: &NameCache,
        def_id: NamedTypeId,
    ) -> impl FnOnce(E) -> Self + '_ {
        move |err| {
            let (mod_, name) = &nc.named_types[def_id];
            Error::annotate_mod_def(err, mod_.clone(), DefName::Type(name.clone()))
        }
    }

    fn annotate_func_def<E: Into<Self>>(
        nc: &NameCache,
        def_id: FuncId,
    ) -> impl FnOnce(E) -> Self + '_ {
        move |err| {
            let (mod_, name) = &nc.funcs[def_id];
            Error::annotate_mod_def(err, mod_.clone(), DefName::Func(name.clone()))
        }
    }

    fn annotate_const_def<E: Into<Self>>(
        nc: &NameCache,
        def_id: ConstId,
    ) -> impl FnOnce(E) -> Self + '_ {
        move |err| {
            let (mod_, name) = &nc.consts[def_id];
            Error::annotate_mod_def(err, mod_.clone(), DefName::Const(name.clone()))
        }
    }

    fn annotate_entry_point<E: Into<Self>>(
        nc: &NameCache,
        def_id: EntryPointId,
    ) -> impl FnOnce(E) -> Self + '_ {
        move |err| {
            let mut err = err.into();
            if err.def.is_none() {
                err.def = Some(DefName::EntryPoint(nc.entry_points[def_id].clone()));
            }
            err
        }
    }

    fn annotate_binding<E: Into<Self>>(binding: BindingLocation) -> impl FnOnce(E) -> Self {
        move |err| {
            let mut err = err.into();
            if err.binding.is_none() {
                err.binding = Some(binding);
            }
            err
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        let mut loc_prefix = |f: &mut std::fmt::Formatter| -> std::fmt::Result {
            if first {
                write!(f, "error in ")?;
            } else {
                write!(f, ", ")?;
            }
            first = false;
            Ok(())
        };

        if let Some(mod_) = &self.mod_ {
            loc_prefix(f)?;
            write!(f, "module {mod_:?}")?;
        }

        if let Some(def) = &self.def {
            loc_prefix(f)?;
            match def {
                DefName::Type(name) => {
                    write!(f, "named type definition {name:?}")?;
                }
                DefName::Func(name) => {
                    write!(f, "function definition {name:?}")?;
                }
                DefName::Const(name) => {
                    write!(f, "constant definition {name:?}")?;
                }
                DefName::EntryPoint(name) => {
                    write!(f, "entry point definition {name:?}")?;
                }
            }
        }

        if let Some(binding) = &self.binding {
            loc_prefix(f)?;
            match binding {
                BindingLocation::Type(id) => {
                    write!(f, "definition of type binding {id:?}")?;
                }
                BindingLocation::Value(id) => {
                    write!(f, "definition of value binding {id:?}")?;
                }
                BindingLocation::Continuation(id) => {
                    write!(f, "definition of continuation binding {id:?}")?;
                }
            }
        }

        if !first {
            write!(f, ": ")?;
        }

        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for Error {}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error {
            kind,
            mod_: None,
            def: None,
            binding: None,
        }
    }
}

fn named_type_id(
    nc: &NameCache,
    mod_name: &api::ModNameBuf,
    type_name: &api::TypeNameBuf,
) -> Result<NamedTypeId, Error> {
    nc.named_types
        .get_by_val(&(mod_name.clone(), type_name.clone()))
        .ok_or_else(|| ErrorKind::NamedTypeNotFound(mod_name.clone(), type_name.clone()).into())
}

pub(crate) fn resolve_types(
    nc: &NameCache,
    tc: &mut TypeCache,
    types_: &OpGraph<api::TypeId, api::TypeOp>,
) -> Result<IdVec<api::TypeId, TypeId>, Error> {
    let mut result = IdVec::with_capacity(types_.len());
    for api_id in types_.count().iter() {
        let node = types_.node(api_id);
        let inputs: SmallVec<_> = node.inputs.iter().map(|input| result[input]).collect();
        let type_data = match node.op {
            api::TypeOp::Named { named_mod, named } => {
                let named_id = named_type_id(nc, named_mod, named)
                    .map_err(Error::annotate_binding(BindingLocation::Type(api_id)))?;
                debug_assert_eq!(inputs.len(), 0);
                TypeData::Named { named: named_id }
            }
            api::TypeOp::Tuple => TypeData::Tuple { fields: inputs },
            api::TypeOp::Union => TypeData::Union { variants: inputs },
            api::TypeOp::HeapCell => {
                debug_assert_eq!(inputs.len(), 0);
                TypeData::HeapCell
            }
            api::TypeOp::Bag => {
                debug_assert_eq!(inputs.len(), 1);
                TypeData::Bag { item: inputs[0] }
            }
        };
        let type_id = tc.types.get_or_insert(type_data);
        let pushed_id = result.push(type_id);
        debug_assert_eq!(pushed_id, api_id);
    }
    debug_assert_eq!(result.count(), types_.count());
    Ok(result)
}

#[derive(Clone, Debug)]
struct FuncSig {
    arg_type: TypeId,
    ret_type: TypeId,
}

pub(crate) fn preprocess(
    program: &api::Program,
) -> Result<(NameCache, TypeCache, ir::Program), Error> {
    let mut nc = NameCache::default();
    let mut tc = TypeCache::default();

    let mut named_types = IdVec::<NamedTypeId, &api::TypeDef>::new();
    let mut funcs = IdVec::<FuncId, &api::FuncDef>::new();
    let mut consts = IdVec::<ConstId, &api::ConstDef>::new();

    for (mod_name, mod_) in &program.mods {
        for (type_name, type_def) in &mod_.type_defs {
            let nc_id = nc
                .named_types
                .insert((mod_name.clone(), type_name.clone()))
                .expect("Builder API should check for duplicate names");
            let pushed_id = named_types.push(type_def);
            debug_assert_eq!(nc_id, pushed_id);
        }
        for (func_name, func_def) in &mod_.func_defs {
            let nc_id = nc
                .funcs
                .insert((mod_name.clone(), func_name.clone()))
                .expect("Builder API should check for duplicate names");
            let pushed_id = funcs.push(func_def);
            debug_assert_eq!(nc_id, pushed_id);
        }
        for (const_name, const_def) in &mod_.const_defs {
            let nc_id = nc
                .consts
                .insert((mod_name.clone(), const_name.clone()))
                .expect("Builder API should check for duplicate names");
            let pushed_id = consts.push(const_def);
            debug_assert_eq!(nc_id, pushed_id);
        }
    }

    // TODO: We should ideally perform fewer passes over the program here.

    let typedef_body_types = named_types.try_map(|named_id, type_def| {
        resolve_types(&nc, &mut tc, &type_def.builder.types)
            .map_err(Error::annotate_type_def(&nc, named_id))
    })?;

    let typedef_contents =
        named_types.map(|named_type_id, type_def| typedef_body_types[named_type_id][type_def.root]);

    let func_body_types = funcs.try_map(|func_id, func_def| {
        resolve_types(
            &nc,
            &mut tc,
            &func_def.builder.expr_builder.type_builder.types,
        )
        .map_err(Error::annotate_func_def(&nc, func_id))
    })?;

    let func_sigs = funcs.map(|func_id, func_def| {
        let body_types = &func_body_types[func_id];
        FuncSig {
            arg_type: body_types[func_def.arg_type],
            ret_type: body_types[func_def.ret_type],
        }
    });

    let const_body_types = consts.try_map(|const_id, const_def| {
        resolve_types(
            &nc,
            &mut tc,
            &const_def.builder.expr_builder.type_builder.types,
        )
        .map_err(Error::annotate_const_def(&nc, const_id))
    })?;

    let const_sigs = consts.map(|const_id, const_def| {
        let body_types = &const_body_types[const_id];
        body_types[&const_def.type_]
    });

    let ctx = Context {
        nc: &nc,
        typedef_contents: &typedef_contents,
        func_sigs: &func_sigs,
        const_sigs: &const_sigs,
    };

    let preprocessed_funcs = funcs.try_map(|func_id, func_def| {
        preprocess_func_def(&mut tc, ctx, func_def, &func_body_types[func_id])
            .map_err(Error::annotate_func_def(&nc, func_id))
    })?;

    let preprocessed_consts = consts.try_map(|const_id, const_def| {
        preprocess_const_def(&mut tc, ctx, const_def, &const_body_types[const_id])
            .map_err(Error::annotate_const_def(&nc, const_id))
    })?;

    let mut entry_points = IdVec::<EntryPointId, FuncId>::new();
    for (entry_point_name, (mod_, func)) in &program.entry_points {
        let nc_id = nc
            .entry_points
            .insert(entry_point_name.clone())
            .expect("builder API should check for duplicate names");
        // TODO: Avoid the clones here
        let func_id = nc
            .funcs
            .get_by_val(&(mod_.clone(), func.clone()))
            .ok_or_else(|| ErrorKind::FuncNotFound(mod_.clone(), func.clone()))
            .map_err(Error::annotate_entry_point(&nc, nc_id))?;
        let func_sig = &func_sigs[func_id];
        let unit_type = tc.types.get_or_insert(TypeData::Tuple {
            fields: smallvec![],
        });
        if func_sig.arg_type != unit_type || func_sig.ret_type != unit_type {
            return Err(Error::annotate_entry_point(&nc, nc_id)(
                ErrorKind::EntryPointDisallowedSig,
            ));
        }
        let pushed_id = entry_points.push(func_id);
        debug_assert_eq!(nc_id, pushed_id);
    }

    Ok((
        nc,
        tc,
        ir::Program {
            named_types: typedef_contents,
            funcs: preprocessed_funcs,
            consts: preprocessed_consts,
            entry_points,
        },
    ))
}

#[derive(Clone, Copy, Debug)]
struct Context<'a> {
    nc: &'a NameCache,
    typedef_contents: &'a IdVec<NamedTypeId, TypeId>,
    func_sigs: &'a IdVec<FuncId, FuncSig>,
    const_sigs: &'a IdVec<ConstId, TypeId>,
}

#[derive(Clone, Copy, Debug)]
struct ValueBinding {
    id: ir::ValueId,
    type_: TypeId,
}

#[derive(Clone, Copy, Debug)]
struct ContinuationBinding {
    entry_block: ir::BlockId,
    arg_type: TypeId,
}

// TODO: Consolidate this logic with render_api_ir.rs
fn render_byte_string(target: &mut String, bytes: &[u8]) {
    target.push('"');
    for &byte in bytes {
        use std::fmt::Write;
        write!(target, "{}", std::ascii::escape_default(byte)).unwrap();
    }
    target.push('"');
}

// TODO: Consolidate this logic with render_api_ir.rs
fn render_type_rec(nc: &NameCache, tc: &TypeCache, target: &mut String, type_: TypeId) {
    match &tc.types[type_] {
        TypeData::Named { named } => {
            let (mod_name, type_name) = &nc.named_types[named];
            render_byte_string(target, &mod_name.0);
            target.push_str("::");
            render_byte_string(target, &type_name.0);
        }

        TypeData::Tuple { fields } => {
            target.push('(');
            let mut first = true;
            for &field in fields {
                if !first {
                    target.push_str(", ");
                }
                first = false;
                render_type_rec(nc, tc, target, field);
            }
            if fields.len() == 1 {
                target.push(',');
            }
            target.push(')');
        }

        TypeData::Union { variants } => {
            if variants.is_empty() {
                // Special case to avoid extra space inside braces
                target.push_str("union { }");
                return;
            }
            target.push_str("union { ");
            let mut first = true;
            for &variant in variants {
                if !first {
                    target.push_str(", ");
                }
                first = false;
                render_type_rec(nc, tc, target, variant);
            }
            target.push_str(" }");
        }

        TypeData::HeapCell => {
            target.push_str("heap_cell");
        }

        TypeData::Bag { item } => {
            target.push_str("bag<");
            render_type_rec(nc, tc, target, *item);
            target.push('>');
        }
    }
}

fn render_type(nc: &NameCache, tc: &TypeCache, type_: TypeId) -> String {
    let mut target = String::new();
    render_type_rec(nc, tc, &mut target, type_);
    target
}

fn type_error<T>(
    nc: &NameCache,
    tc: &TypeCache,
    expected: TypeId,
    actual: TypeId,
) -> Result<T, Error> {
    debug_assert_ne!(expected, actual);
    Err(ErrorKind::TypeMismatch {
        expected: render_type(nc, tc, expected),
        actual: render_type(nc, tc, actual),
    }
    .into())
}

fn check_type(
    nc: &NameCache,
    tc: &TypeCache,
    expected: TypeId,
    actual: TypeId,
) -> Result<(), Error> {
    if expected != actual {
        type_error(nc, tc, expected, actual)
    } else {
        Ok(())
    }
}

fn try_get_bag_item_type(
    nc: &NameCache,
    tc: &TypeCache,
    bag_type: TypeId,
) -> Result<TypeId, Error> {
    if let TypeData::Bag { item } = tc.types[bag_type] {
        Ok(item)
    } else {
        Err(ErrorKind::ExpectedBagType(render_type(nc, tc, bag_type)).into())
    }
}

fn try_get_tuple_field_types<'a>(
    nc: &NameCache,
    tc: &'a TypeCache,
    tuple_type: TypeId,
) -> Result<&'a [TypeId], Error> {
    if let TypeData::Tuple { fields } = &tc.types[tuple_type] {
        Ok(fields)
    } else {
        Err(ErrorKind::ExpectedTupleType(render_type(nc, tc, tuple_type)).into())
    }
}

fn try_get_union_variant_types<'a>(
    nc: &NameCache,
    tc: &'a TypeCache,
    union_type: TypeId,
) -> Result<&'a [TypeId], Error> {
    if let TypeData::Union { variants } = &tc.types[union_type] {
        Ok(variants)
    } else {
        Err(ErrorKind::ExpectedUnionType(render_type(nc, tc, union_type)).into())
    }
}

fn try_get_named_type_id(
    nc: &NameCache,
    tc: &TypeCache,
    named_type: TypeId,
) -> Result<NamedTypeId, Error> {
    if let TypeData::Named { named } = tc.types[named_type] {
        Ok(named)
    } else {
        Err(ErrorKind::ExpectedNamedType(render_type(nc, tc, named_type)).into())
    }
}

fn get_value(
    values_in_scope: &IdVec<api::ValueId, Option<ir::ValueId>>,
    api_value: api::ValueId,
) -> Result<ir::ValueId, Error> {
    values_in_scope[api_value].ok_or_else(|| ErrorKind::ValueNotInScope(api_value).into())
}

fn preprocess_block_expr(
    tc: &mut TypeCache,
    ctx: Context,
    api_builder: &api::ExprBuilder,
    body_types: &IdVec<api::TypeId, TypeId>,
    graph_builder: &mut ir::GraphBuilder,
    values_in_scope: &mut IdVec<api::ValueId, Option<ir::ValueId>>,
    continuations_in_scope: &mut IdVec<api::ContinuationId, Option<ContinuationBinding>>,
    mut block: ir::BlockId,
    block_expr: api::BlockExpr,
) -> Result<(ir::BlockId, ir::ValueId), Error> {
    let api::BlockExpr(api_block, api_ret_val) = block_expr;

    let mut join_info = None;

    let mut api_value_ids = api_builder.blocks.block_values(api_block).peekable();
    loop {
        let mut continuation_group = SmallVec::<[_; 32]>::new();
        loop {
            if let Some(api_value_id) = api_value_ids.peek() {
                if let api::Op::DeclareContinuation { continuation } =
                    api_builder.vals.node(api_value_id).op
                {
                    let info = api_builder.continuations[continuation];
                    let arg_type = body_types[info.arg_type];
                    let (entry_block, arg) = graph_builder.add_block_with_param(arg_type);
                    replace_none(
                        &mut continuations_in_scope[continuation],
                        ContinuationBinding {
                            entry_block,
                            arg_type,
                        },
                    )
                    .unwrap();
                    continuation_group.push((continuation, arg));
                    api_value_ids.next();
                    continue;
                }
            }
            break;
        }

        for (continuation, arg) in continuation_group {
            let ContinuationBinding {
                entry_block,
                arg_type: _,
            } = continuations_in_scope[continuation].unwrap();

            let api::ContinuationInfo {
                arg_type: _,
                ret_type: api_ret_type,
                arg: api_arg,
                body: api_body,
            } = api_builder.continuations[continuation];

            replace_none(&mut values_in_scope[api_arg], arg).unwrap();
            let (continuation_final_block, continuation_ret_val) = preprocess_block_expr(
                tc,
                ctx,
                api_builder,
                body_types,
                graph_builder,
                values_in_scope,
                continuations_in_scope,
                entry_block,
                api_body
                    .expect("builder API should have checked that all continuations are defined"),
            )
            .map_err(Error::annotate_binding(BindingLocation::Continuation(
                *continuation,
            )))?;
            values_in_scope[api_arg] = None;

            let ret_type = body_types[api_ret_type];

            check_type(
                ctx.nc,
                tc,
                ret_type,
                graph_builder
                    .values()
                    .node(continuation_ret_val)
                    .op
                    .result_type,
            )
            .map_err(Error::annotate_binding(BindingLocation::Continuation(
                *continuation,
            )))?;

            let (join_block, join_val_type, _) = *join_info.get_or_insert_with(|| {
                let (join_block, join_val) = graph_builder.add_block_with_param(ret_type);
                (join_block, ret_type, join_val)
            });

            check_type(ctx.nc, tc, join_val_type, ret_type).map_err(Error::annotate_binding(
                BindingLocation::Continuation(*continuation),
            ))?;

            graph_builder.set_jump_targets(
                continuation_final_block,
                Some(continuation_ret_val),
                smallvec![ir::JumpTarget::Block(join_block)],
            );
        }

        match api_value_ids.next() {
            None => break,
            Some(api_value_id) => {
                let api_node = api_builder.vals.node(api_value_id);
                debug_assert!(!matches!(&api_node.op, api::Op::DeclareContinuation { .. }));
                let (new_block, value) = preprocess_op(
                    tc,
                    ctx,
                    api_builder,
                    body_types,
                    graph_builder,
                    values_in_scope,
                    continuations_in_scope,
                    block,
                    api_node.op,
                    api_node.inputs,
                )
                .map_err(Error::annotate_binding(BindingLocation::Value(
                    api_value_id,
                )))?;
                block = new_block;
                replace_none(&mut values_in_scope[api_value_id], value).unwrap();
            }
        }
    }

    let (final_block, final_value) = match join_info {
        None => (block, get_value(values_in_scope, api_ret_val)?),
        Some((join_block, join_val_type, join_val)) => {
            let ret_val = get_value(values_in_scope, api_ret_val)?;
            check_type(
                ctx.nc,
                tc,
                join_val_type,
                graph_builder.values().node(ret_val).op.result_type,
            )?;
            graph_builder.set_jump_targets(
                block,
                Some(ret_val),
                smallvec![ir::JumpTarget::Block(join_block)],
            );
            (join_block, join_val)
        }
    };

    for api_value_id in api_builder.blocks.block_values(api_block) {
        match &api_builder.vals.node(api_value_id).op {
            api::Op::DeclareContinuation { continuation } => {
                debug_assert!(values_in_scope[api_value_id].is_none());
                debug_assert!(continuations_in_scope[continuation].is_some());
                continuations_in_scope[continuation] = None;
            }

            _ => {
                debug_assert!(values_in_scope[api_value_id].is_some());
                values_in_scope[api_value_id] = None;
            }
        }
    }

    Ok((final_block, final_value))
}

fn preprocess_op(
    tc: &mut TypeCache,
    ctx: Context,
    api_builder: &api::ExprBuilder,
    body_types: &IdVec<api::TypeId, TypeId>,
    graph_builder: &mut ir::GraphBuilder,
    values_in_scope: &mut IdVec<api::ValueId, Option<ir::ValueId>>,
    continuations_in_scope: &mut IdVec<api::ContinuationId, Option<ContinuationBinding>>,
    block: ir::BlockId,
    api_op: &api::Op,
    api_inputs: &[api::ValueId],
) -> Result<(ir::BlockId, ir::ValueId), Error> {
    let mut inputs = SmallVec::<[_; 32]>::with_capacity(api_inputs.len());
    let mut input_types = SmallVec::<[_; 32]>::with_capacity(api_inputs.len());
    for &api_input in api_inputs {
        let id = get_value(values_in_scope, api_input)?;
        inputs.push(id);
        input_types.push(graph_builder.values().node(id).op.result_type);
    }

    match api_op {
        api::Op::Arg | api::Op::ContinuationArg | api::Op::DeclareContinuation { .. } => {
            unreachable!(
                "op {:?} should never appear as a value binding in a block",
                api_op,
            )
        }

        api::Op::Jump {
            continuation,
            unreachable_result_type,
        } => {
            debug_assert_eq!(inputs.len(), 1);

            let cont_binding = continuations_in_scope[continuation]
                .ok_or(ErrorKind::ContinuationNotInScope(*continuation))?;

            check_type(ctx.nc, tc, cont_binding.arg_type, input_types[0])?;

            graph_builder.set_jump_targets(
                block,
                Some(inputs[0]),
                smallvec![ir::JumpTarget::Block(cont_binding.entry_block)],
            );

            let (unreachable_block, unreachable_result) =
                graph_builder.add_block_with_param(body_types[unreachable_result_type]);

            Ok((unreachable_block, unreachable_result))
        }

        api::Op::UnknownWith { result_type } => {
            let value = graph_builder.add_op(
                block,
                ir::OpKind::UnknownWith,
                &inputs,
                body_types[result_type],
            );
            Ok((block, value))
        }

        api::Op::Call {
            callee_spec_var,
            callee_mod,
            callee,
        } => {
            debug_assert_eq!(inputs.len(), 1);

            // TODO: Avoid the clones here
            let callee_id = ctx
                .nc
                .funcs
                .get_by_val(&(callee_mod.clone(), callee.clone()))
                .ok_or_else(|| ErrorKind::FuncNotFound(callee_mod.clone(), callee.clone()))?;

            let callee_sig = &ctx.func_sigs[callee_id];

            check_type(ctx.nc, tc, callee_sig.arg_type, input_types[0])?;

            let value = graph_builder.add_op(
                block,
                ir::OpKind::Call {
                    callee_spec_var: *callee_spec_var,
                    callee: callee_id,
                },
                &inputs,
                callee_sig.ret_type,
            );
            Ok((block, value))
        }

        api::Op::ConstRef { const_mod, const_ } => {
            debug_assert_eq!(inputs.len(), 0);

            let const_id = ctx
                .nc
                .consts
                .get_by_val(&(const_mod.clone(), const_.clone()))
                .ok_or_else(|| ErrorKind::ConstNotFound(const_mod.clone(), const_.clone()))?;

            let const_type = ctx.const_sigs[const_id];

            let value = graph_builder.add_op(
                block,
                ir::OpKind::ConstRef { const_: const_id },
                &[],
                const_type,
            );
            Ok((block, value))
        }

        api::Op::Choice { cases } => {
            debug_assert_eq!(inputs.len(), 0);
            debug_assert_ne!(cases.len(), 0);

            let case_results: SmallVec<[_; 32]> = cases
                .iter()
                .map(|&case| {
                    let case_entry_block = graph_builder.add_block();
                    let (case_final_block, case_value) = preprocess_block_expr(
                        tc,
                        ctx,
                        api_builder,
                        body_types,
                        graph_builder,
                        values_in_scope,
                        continuations_in_scope,
                        case_entry_block,
                        case,
                    )?;
                    Ok((case_entry_block, case_final_block, case_value))
                })
                .collect::<Result<_, Error>>()?;

            graph_builder.set_jump_targets(
                block,
                None,
                case_results
                    .iter()
                    .map(|&(case_entry_block, _, _)| ir::JumpTarget::Block(case_entry_block))
                    .collect(),
            );

            let mut result_type = None;
            for &(_, _, case_value) in &case_results {
                let case_value_type = graph_builder.values().node(case_value).op.result_type;
                if let Some(prev_result_type) = result_type {
                    check_type(ctx.nc, tc, prev_result_type, case_value_type)?;
                } else {
                    result_type = Some(case_value_type);
                }
            }
            let result_type = result_type.expect("case_results is nonempty");

            let (successor_block, choice_value) = graph_builder.add_block_with_param(result_type);

            for &(_, case_final_block, case_value) in &case_results {
                graph_builder.set_jump_targets(
                    case_final_block,
                    Some(case_value),
                    smallvec![JumpTarget::Block(successor_block)],
                );
            }

            Ok((successor_block, choice_value))
        }

        api::Op::SubBlock { sub_block } => {
            debug_assert_eq!(inputs.len(), 0);
            preprocess_block_expr(
                tc,
                ctx,
                api_builder,
                body_types,
                graph_builder,
                values_in_scope,
                continuations_in_scope,
                block,
                *sub_block,
            )
        }

        api::Op::Terminate {
            unreachable_result_type,
        } => {
            debug_assert_eq!(inputs.len(), 0);
            graph_builder.set_jump_targets(block, None, smallvec![]);
            let (unreachable_block, unreachable_value) =
                graph_builder.add_block_with_param(body_types[unreachable_result_type]);
            Ok((unreachable_block, unreachable_value))
        }

        api::Op::NewHeapCell => {
            debug_assert_eq!(inputs.len(), 0);
            let value = graph_builder.add_op(
                block,
                ir::OpKind::NewHeapCell,
                &[],
                tc.types.get_or_insert(TypeData::HeapCell),
            );
            Ok((block, value))
        }

        api::Op::Touch => {
            debug_assert_eq!(inputs.len(), 1);
            let heap_cell_type = tc.types.get_or_insert(TypeData::HeapCell);
            check_type(ctx.nc, tc, heap_cell_type, input_types[0])?;
            let value = graph_builder.add_op(
                block,
                ir::OpKind::RecursiveTouch,
                &inputs,
                tc.types.get_or_insert(TypeData::Tuple {
                    fields: smallvec![],
                }),
            );
            Ok((block, value))
        }

        api::Op::RecursiveTouch => {
            debug_assert_eq!(inputs.len(), 1);
            let value = graph_builder.add_op(
                block,
                ir::OpKind::RecursiveTouch,
                &inputs,
                tc.types.get_or_insert(TypeData::Tuple {
                    fields: smallvec![],
                }),
            );
            Ok((block, value))
        }

        api::Op::UpdateWriteOnly { update_mode_var } => {
            debug_assert_eq!(inputs.len(), 1);
            let heap_cell_type = tc.types.get_or_insert(TypeData::HeapCell);
            check_type(ctx.nc, tc, heap_cell_type, input_types[0])?;
            let value = graph_builder.add_op(
                block,
                ir::OpKind::UpdateWriteOnly {
                    update_mode_var: *update_mode_var,
                },
                &inputs,
                tc.types.get_or_insert(TypeData::Tuple {
                    fields: smallvec![],
                }),
            );
            Ok((block, value))
        }

        api::Op::EmptyBag { item_type } => {
            debug_assert_eq!(inputs.len(), 0);
            let value = graph_builder.add_op(
                block,
                ir::OpKind::EmptyBag,
                &[],
                tc.types.get_or_insert(TypeData::Bag {
                    item: body_types[item_type],
                }),
            );
            Ok((block, value))
        }

        api::Op::BagInsert => {
            debug_assert_eq!(inputs.len(), 2);
            let bag_type = input_types[0];
            let expected_item_type = try_get_bag_item_type(ctx.nc, tc, bag_type)?;
            check_type(ctx.nc, tc, expected_item_type, input_types[1])?;
            let value = graph_builder.add_op(block, ir::OpKind::BagInsert, &inputs, bag_type);
            Ok((block, value))
        }

        api::Op::BagGet => {
            debug_assert_eq!(inputs.len(), 1);
            let item_type = try_get_bag_item_type(ctx.nc, tc, input_types[0])?;
            let value = graph_builder.add_op(block, ir::OpKind::BagGet, &inputs, item_type);
            Ok((block, value))
        }

        api::Op::BagRemove => {
            debug_assert_eq!(inputs.len(), 1);
            let bag_type = input_types[0];
            let item_type = try_get_bag_item_type(ctx.nc, tc, bag_type)?;
            let value = graph_builder.add_op(
                block,
                ir::OpKind::BagRemove,
                &inputs,
                tc.types.get_or_insert(TypeData::Tuple {
                    fields: smallvec![bag_type, item_type],
                }),
            );
            Ok((block, value))
        }

        api::Op::MakeTuple => {
            let tuple_type = tc.types.get_or_insert(TypeData::Tuple {
                fields: SmallVec::from_slice(&input_types),
            });
            let value = graph_builder.add_op(block, ir::OpKind::MakeTuple, &inputs, tuple_type);
            Ok((block, value))
        }

        api::Op::GetTupleField { field_idx } => {
            debug_assert_eq!(inputs.len(), 1);
            let tuple_type = input_types[0];
            let field_types = try_get_tuple_field_types(ctx.nc, tc, tuple_type)?;
            let field_type = *field_types
                .get(*field_idx as usize)
                .ok_or(ErrorKind::TupleFieldOutOfRange(*field_idx))?;
            let value = graph_builder.add_op(
                block,
                ir::OpKind::GetTupleField {
                    field_idx: *field_idx,
                },
                &inputs,
                field_type,
            );
            Ok((block, value))
        }

        api::Op::MakeUnion {
            variant_types,
            variant_idx,
        } => {
            debug_assert_eq!(inputs.len(), 1);
            let tc_variant_types: SmallVec<_> = variant_types
                .iter()
                .map(|api_type_id| body_types[api_type_id])
                .collect();
            let this_variant_type = *tc_variant_types
                .get(*variant_idx as usize)
                .ok_or(ErrorKind::UnionVariantOutOfRange(*variant_idx))?;
            check_type(ctx.nc, tc, this_variant_type, input_types[0])?;
            let union_type = tc.types.get_or_insert(TypeData::Union {
                variants: tc_variant_types,
            });
            let value = graph_builder.add_op(
                block,
                ir::OpKind::MakeUnion {
                    variant_idx: *variant_idx,
                },
                &inputs,
                union_type,
            );
            Ok((block, value))
        }

        api::Op::UnwrapUnion { variant_idx } => {
            debug_assert_eq!(inputs.len(), 1);
            let variant_types = try_get_union_variant_types(ctx.nc, tc, input_types[0])?;
            let this_variant_type = *variant_types
                .get(*variant_idx as usize)
                .ok_or(ErrorKind::UnionVariantOutOfRange(*variant_idx))?;
            let value = graph_builder.add_op(
                block,
                ir::OpKind::UnwrapUnion {
                    variant_idx: *variant_idx,
                },
                &inputs,
                this_variant_type,
            );
            Ok((block, value))
        }

        api::Op::MakeNamed { named_mod, named } => {
            debug_assert_eq!(inputs.len(), 1);
            // TODO: Avoid the clones here
            let named_id = ctx
                .nc
                .named_types
                .get_by_val(&(named_mod.clone(), named.clone()))
                .ok_or_else(|| ErrorKind::NamedTypeNotFound(named_mod.clone(), named.clone()))?;
            let content_type = ctx.typedef_contents[named_id];
            check_type(ctx.nc, tc, content_type, input_types[0])?;
            let value = graph_builder.add_op(
                block,
                ir::OpKind::MakeNamed,
                &inputs,
                tc.types.get_or_insert(TypeData::Named { named: named_id }),
            );
            Ok((block, value))
        }

        api::Op::UnwrapNamed { named_mod, named } => {
            debug_assert_eq!(inputs.len(), 1);
            let named_id = try_get_named_type_id(ctx.nc, tc, input_types[0])?;
            let expected_named_id = ctx
                .nc
                .named_types
                .get_by_val(&(named_mod.clone(), named.clone()))
                .ok_or_else(|| ErrorKind::NamedTypeNotFound(named_mod.clone(), named.clone()))?;
            if named_id != expected_named_id {
                let expected = tc.types.get_or_insert(TypeData::Named {
                    named: expected_named_id,
                });
                type_error(ctx.nc, tc, expected, input_types[0])?;
            }
            let value = graph_builder.add_op(
                block,
                ir::OpKind::UnwrapNamed,
                &inputs,
                ctx.typedef_contents[named_id],
            );
            Ok((block, value))
        }
    }
}

fn preprocess_func_def(
    tc: &mut TypeCache,
    ctx: Context,
    func_def: &api::FuncDef,
    body_types: &IdVec<api::TypeId, TypeId>,
) -> Result<ir::FuncDef, Error> {
    let api_builder = &func_def.builder.expr_builder;
    let mut graph_builder = ir::GraphBuilder::new();
    let mut values_in_scope = IdVec::filled_with(api_builder.vals.count(), || None);
    let mut continuations_in_scope = IdVec::filled_with(api_builder.continuations.count(), || None);
    let (entry_block, arg_val) = graph_builder.add_block_with_param(body_types[func_def.arg_type]);
    values_in_scope[func_def.builder.argument] = Some(arg_val);
    let (final_block, ret_val) = preprocess_block_expr(
        tc,
        ctx,
        api_builder,
        body_types,
        &mut graph_builder,
        &mut values_in_scope,
        &mut continuations_in_scope,
        entry_block,
        func_def.root,
    )?;
    check_type(
        ctx.nc,
        tc,
        body_types[func_def.ret_type],
        graph_builder.values().node(ret_val).op.result_type,
    )?;
    graph_builder.set_jump_targets(final_block, Some(ret_val), smallvec![ir::JumpTarget::Ret]);
    Ok(ir::FuncDef {
        graph: graph_builder.build(
            entry_block,
            body_types[func_def.ret_type],
            func_def.builder.expr_builder.update_mode_vars.count(),
            func_def.builder.expr_builder.callee_spec_vars.count(),
        ),
    })
}

fn preprocess_const_def(
    tc: &mut TypeCache,
    ctx: Context,
    const_def: &api::ConstDef,
    body_types: &IdVec<api::TypeId, TypeId>,
) -> Result<ir::ConstDef, Error> {
    let api_builder = &const_def.builder.expr_builder;
    let mut graph_builder = ir::GraphBuilder::new();
    let mut values_in_scope = IdVec::filled_with(api_builder.vals.count(), || None);
    let mut continuations_in_scope = IdVec::filled_with(api_builder.continuations.count(), || None);
    let entry_block = graph_builder.add_block();
    let (final_block, ret_val) = preprocess_block_expr(
        tc,
        ctx,
        api_builder,
        body_types,
        &mut graph_builder,
        &mut values_in_scope,
        &mut continuations_in_scope,
        entry_block,
        const_def.root,
    )?;
    check_type(
        ctx.nc,
        tc,
        body_types[const_def.type_],
        graph_builder.values().node(ret_val).op.result_type,
    )?;
    graph_builder.set_jump_targets(final_block, Some(ret_val), smallvec![ir::JumpTarget::Ret]);
    Ok(ir::ConstDef {
        graph: graph_builder.build(
            entry_block,
            body_types[const_def.type_],
            const_def.builder.expr_builder.update_mode_vars.count(),
            const_def.builder.expr_builder.callee_spec_vars.count(),
        ),
    })
}
