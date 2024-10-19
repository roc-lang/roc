use crate::{
    mono_ir::{MonoExpr, MonoExprId, MonoExprs},
    mono_num::Number,
    mono_type::{MonoType, MonoTypes, Primitive},
    specialize_type::{MonoCache, Problem, RecordFieldIds, TupleElemIds},
    DebugInfo,
};
use roc_can::expr::{Expr, IntValue};
use roc_collections::Push;
use roc_types::subs::Subs;

pub struct Env<'c, 'd, 's, 't, P> {
    subs: &'s mut Subs,
    types_cache: &'c mut MonoCache,
    mono_types: &'t mut MonoTypes,
    mono_exprs: &'t mut MonoExprs,
    record_field_ids: RecordFieldIds,
    tuple_elem_ids: TupleElemIds,
    debug_info: &'d mut Option<DebugInfo>,
    problems: P,
}

impl<'c, 'd, 's, 't, P: Push<Problem>> Env<'c, 'd, 's, 't, P> {
    pub fn to_mono_expr(&mut self, can_expr: Expr) -> Option<MonoExprId> {
        let problems = &mut self.problems;
        let mono_types = &mut self.mono_types;
        let mut mono_from_var = |var| {
            self.types_cache.monomorphize_var(
                self.subs,
                mono_types,
                &mut self.record_field_ids,
                &mut self.tuple_elem_ids,
                problems,
                &mut self.debug_info,
                var,
            )
        };

        let mut add = |expr| self.mono_exprs.add(expr);
        macro_rules! compiler_bug {
            ($problem:expr) => {{
                problems.push($problem);
                Some(add(MonoExpr::CompilerBug($problem)))
            }};
        }

        let mono_expr = match can_expr {
            Expr::Num(var, _str, int_value, _bound) => match mono_from_var(var) {
                Some(mono_id) => match mono_types.get(mono_id) {
                    MonoType::Primitive(primitive) => to_num(*primitive, int_value, problems),
                    other => {
                        return compiler_bug!(Problem::NumSpecializedToWrongType(Some(*other)));
                    }
                },
                None => {
                    return compiler_bug!(Problem::NumSpecializedToWrongType(None));
                }
            },
            Expr::Int(var, _precision_var, _str, val, _bound) => match mono_from_var(var) {
                Some(mono_id) => match mono_types.get(mono_id) {
                    MonoType::Primitive(primitive) => to_int(*primitive, val, problems),
                    other => {
                        return compiler_bug!(Problem::NumSpecializedToWrongType(Some(*other)));
                    }
                },
                None => {
                    return compiler_bug!(Problem::NumSpecializedToWrongType(None));
                }
            },
            _ => todo!(),
            // Expr::Float(var, _precision_var, _str, val, _bound) => {
            //     match mono_from_var(var) {
            //         Some(mono_id) => {
            //             match mono_types.get(mono_id) {
            //                 MonoType::Number(number) => {
            //                     Some(mono_types.add(to_frac(number, val, problems)))
            //                 }
            //                 _ => {
            //                     // TODO push problem and return Malformed expr: Num specialized to something that wasn't a number, namely: _____
            //                 }
            //             }
            //         }
            //         None => {
            //             // TODO push problem and return Malformed expr: Num's type param specialized to Unit somehow
            //         }
            //     }
            // }
            // Expr::SingleQuote(variable, _precision_var, _char, _bound) => {
            //     // Single quote monomorphizes to an integer.
            //     // TODO let's just start writing some tests for converting numbers and single quotes and strings.
            //     // TODO also, verify that doing nonsense like a type annotation of Num {} is handled gracefully.
            // }
            // Expr::Str(_) => todo!(),
            // Expr::List {
            //     elem_var,
            //     loc_elems,
            // } => todo!(),
            // Expr::IngestedFile(path_buf, arc, variable) => todo!(),
            // Expr::Var(symbol, variable) => todo!(),
            // Expr::ParamsVar {
            //     symbol,
            //     var,
            //     params_symbol,
            //     params_var,
            // } => todo!(),
            // Expr::AbilityMember(symbol, specialization_id, variable) => todo!(),
            // Expr::When {
            //     loc_cond,
            //     cond_var,
            //     expr_var,
            //     region,
            //     branches,
            //     branches_cond_var,
            //     exhaustive,
            // } => todo!(),
            // Expr::If {
            //     cond_var,
            //     branch_var,
            //     branches,
            //     final_else,
            // } => todo!(),
            // Expr::LetRec(vec, loc, illegal_cycle_mark) => todo!(),
            // Expr::LetNonRec(def, loc) => todo!(),
            // Expr::Call(_, vec, called_via) => todo!(),
            // Expr::RunLowLevel { op, args, ret_var } => todo!(),
            // Expr::ForeignCall {
            //     foreign_symbol,
            //     args,
            //     ret_var,
            // } => todo!(),
            // Expr::Closure(closure_data) => todo!(),
            // Expr::Record { record_var, fields } => todo!(),
            // Expr::EmptyRecord => todo!(),
            // Expr::Tuple { tuple_var, elems } => todo!(),
            // Expr::ImportParams(module_id, region, _) => todo!(),
            // Expr::Crash { msg, ret_var } => todo!(),
            // Expr::RecordAccess {
            //     record_var,
            //     ext_var,
            //     field_var,
            //     loc_expr,
            //     field,
            // } => todo!(),
            // Expr::RecordAccessor(struct_accessor_data) => todo!(),
            // Expr::TupleAccess {
            //     tuple_var,
            //     ext_var,
            //     elem_var,
            //     loc_expr,
            //     index,
            // } => todo!(),
            // Expr::RecordUpdate {
            //     record_var,
            //     ext_var,
            //     symbol,
            //     updates,
            // } => todo!(),
            // Expr::Tag {
            //     tag_union_var,
            //     ext_var,
            //     name,
            //     arguments,
            // } => todo!(),
            // Expr::ZeroArgumentTag {
            //     closure_name,
            //     variant_var,
            //     ext_var,
            //     name,
            // } => todo!(),
            // Expr::OpaqueRef {
            //     opaque_var,
            //     name,
            //     argument,
            //     specialized_def_type,
            //     type_arguments,
            //     lambda_set_variables,
            // } => todo!(),
            // Expr::OpaqueWrapFunction(opaque_wrap_function_data) => todo!(),
            // Expr::Expect {
            //     loc_condition,
            //     loc_continuation,
            //     lookups_in_cond,
            // } => todo!(),
            // Expr::ExpectFx {
            //     loc_condition,
            //     loc_continuation,
            //     lookups_in_cond,
            // } => todo!(),
            // Expr::Dbg {
            //     source_location,
            //     source,
            //     loc_message,
            //     loc_continuation,
            //     variable,
            //     symbol,
            // } => todo!(),
            // Expr::TypedHole(variable) => todo!(),
            Expr::RuntimeError(_runtime_error) => {
                todo!("generate a MonoExpr::Crash based on the runtime error");
            }
        };

        Some(add(mono_expr))
    }
}

/// Convert a number literal (e.g. `42`) to a monomorphized type.
/// This is allowed to convert to either an integer or a fraction.
fn to_num(primitive: Primitive, val: IntValue, problems: &mut impl Push<Problem>) -> MonoExpr {
    match primitive {
        Primitive::Dec => MonoExpr::Number(Number::Dec(val.as_i128())),
        Primitive::F32 => MonoExpr::Number(Number::F32(val.as_i128() as f32)),
        Primitive::F64 => MonoExpr::Number(Number::F64(val.as_i128() as f64)),
        Primitive::U8 => MonoExpr::Number(Number::U8(val.as_i128() as u8)),
        Primitive::I8 => MonoExpr::Number(Number::I8(val.as_i128() as i8)),
        Primitive::U16 => MonoExpr::Number(Number::U16(val.as_i128() as u16)),
        Primitive::I16 => MonoExpr::Number(Number::I16(val.as_i128() as i16)),
        Primitive::U32 => MonoExpr::Number(Number::U32(val.as_i128() as u32)),
        Primitive::I32 => MonoExpr::Number(Number::I32(val.as_i128() as i32)),
        Primitive::U64 => MonoExpr::Number(Number::U64(val.as_i128() as u64)),
        Primitive::I64 => MonoExpr::Number(Number::I64(val.as_i128() as i64)),
        Primitive::U128 => MonoExpr::Number(Number::U128(val.as_u128())),
        Primitive::I128 => MonoExpr::Number(Number::I128(val.as_i128())),
        Primitive::Str => {
            let problem = Problem::NumSpecializedToWrongType(Some(MonoType::Primitive(primitive)));
            problems.push(problem);
            MonoExpr::CompilerBug(problem)
        }
    }
}

/// Convert an integer literal (e.g. `0x5`) to a monomorphized type.
/// If somehow its type was not an integer type, that's a compiler bug!
fn to_int(primitive: Primitive, val: IntValue, problems: &mut impl Push<Problem>) -> MonoExpr {
    match primitive {
        // These are ordered roughly by most to least common integer types
        Primitive::U64 => MonoExpr::Number(Number::U64(val.as_u64())),
        Primitive::U8 => MonoExpr::Number(Number::U8(val.as_u8())),
        Primitive::I64 => MonoExpr::Number(Number::I64(val.as_i64())),
        Primitive::I32 => MonoExpr::Number(Number::I32(val.as_i32())),
        Primitive::U16 => MonoExpr::Number(Number::U16(val.as_u16())),
        Primitive::U32 => MonoExpr::Number(Number::U32(val.as_u32())),
        Primitive::I128 => MonoExpr::Number(Number::I128(val.as_i128())),
        Primitive::U128 => MonoExpr::Number(Number::U128(val.as_u128())),
        Primitive::I16 => MonoExpr::Number(Number::I16(val.as_i16())),
        Primitive::I8 => MonoExpr::Number(Number::I8(val.as_i8())),
        Primitive::Str | Primitive::Dec | Primitive::F32 | Primitive::F64 => {
            let problem = Problem::NumSpecializedToWrongType(Some(MonoType::Primitive(primitive)));
            problems.push(problem);
            MonoExpr::CompilerBug(problem)
        }
    }
}

// /// Convert a fraction literal (e.g. `4.2`) to a monomorphized type.
// /// If somehow its type was not a fraction type, that's a compiler bug!
// fn to_frac(
//     number: mono_type::Number,
//     val: FracValue,
//     problems: &mut impl Push<Problem>,
// ) -> MonoExpr {
//     match number {
//         mono_type::Number::Dec => Number::Dec(val.to_dec()),
//         mono_type::Number::F32 => Number::F32(val.to_f32()),
//         mono_type::Number::F64 => Number::F64(val.to_f64()),
//         mono_type::Number::U8
//         | mono_type::Number::I8
//         | mono_type::Number::U16
//         | mono_type::Number::I16
//         | mono_type::Number::U32
//         | mono_type::Number::I32
//         | mono_type::Number::U64
//         | mono_type::Number::I64
//         | mono_type::Number::U128
//         | mono_type::Number::I128 => {
//             // TODO push problem of frac monomorphizing to int, return Malformed expr
//         }
//     }
// }
