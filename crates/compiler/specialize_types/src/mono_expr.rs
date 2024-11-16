use crate::{
    mono_ir::{sort_fields, MonoExpr, MonoExprId, MonoExprs},
    mono_module::Interns,
    mono_num::Number,
    mono_type::{MonoType, MonoTypes, Primitive},
    specialize_type::{MonoCache, Problem, RecordFieldIds, TupleElemIds},
    DebugInfo,
};
use bumpalo::Bump;
use roc_can::expr::{Expr, IntValue};
use roc_collections::Push;
use roc_region::all::Region;
use roc_solve::module::Solved;
use roc_types::subs::Subs;
use soa::{Index, Slice};

pub struct Env<'a, 'c, 'd, 'i, 's, 't, P> {
    arena: &'a Bump,
    subs: &'s mut Subs,
    types_cache: &'c mut MonoCache,
    mono_types: &'t mut MonoTypes,
    mono_exprs: &'t mut MonoExprs,
    record_field_ids: RecordFieldIds,
    tuple_elem_ids: TupleElemIds,
    debug_info: &'d mut Option<DebugInfo>,
    string_interns: &'i mut Interns<'a>,
    problems: P,
}

impl<'a, 'c, 'd, 'i, 's, 't, P: Push<Problem>> Env<'a, 'c, 'd, 'i, 's, 't, P> {
    pub fn new(
        arena: &'a Bump,
        subs: &'s mut Solved<Subs>,
        types_cache: &'c mut MonoCache,
        mono_types: &'t mut MonoTypes,
        mono_exprs: &'t mut MonoExprs,
        record_field_ids: RecordFieldIds,
        tuple_elem_ids: TupleElemIds,
        string_interns: &'i mut Interns<'a>,
        debug_info: &'d mut Option<DebugInfo>,
        problems: P,
    ) -> Self {
        Env {
            arena,
            subs: subs.inner_mut(),
            types_cache,
            mono_types,
            mono_exprs,
            record_field_ids,
            tuple_elem_ids,
            string_interns,
            debug_info,
            problems,
        }
    }

    pub fn to_mono_expr(
        &mut self,
        can_expr: Expr,
        region: Region,
        get_expr_id: impl FnOnce() -> Option<MonoExprId>,
    ) -> Option<MonoExprId> {
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

        macro_rules! compiler_bug {
            ($problem:expr, $region:expr) => {{
                problems.push($problem);
                Some(
                    self.mono_exprs
                        .add(MonoExpr::CompilerBug($problem), $region),
                )
            }};
        }

        let mono_expr = match can_expr {
            Expr::Float(var, _precision_var, _str, val, _bound) => match mono_from_var(var) {
                Some(mono_id) => match mono_types.get(mono_id) {
                    MonoType::Primitive(primitive) => to_frac(*primitive, val, problems),
                    other => {
                        return compiler_bug!(
                            Problem::NumSpecializedToWrongType(Some(*other)),
                            region
                        );
                    }
                },
                None => {
                    return compiler_bug!(Problem::NumSpecializedToWrongType(None), region);
                }
            },
            Expr::Num(var, _str, int_value, _) | Expr::Int(var, _, _str, int_value, _) => {
                // Numbers can specialize
                match mono_from_var(var) {
                    Some(mono_id) => match mono_types.get(mono_id) {
                        MonoType::Primitive(primitive) => to_num(*primitive, int_value, problems),
                        other => {
                            return compiler_bug!(
                                Problem::NumSpecializedToWrongType(Some(*other)),
                                region
                            );
                        }
                    },
                    None => {
                        return compiler_bug!(Problem::NumSpecializedToWrongType(None), region);
                    }
                }
            }
            Expr::SingleQuote(var, _precision_var, char, _bound) => match mono_from_var(var) {
                // Single-quote characters monomorphize to an integer.
                // TODO if we store these using the same representation as other ints (e.g. Expr::Int,
                // or keeping a separate value but storing an IntValue instead of a char), then
                // even though we verify them differently, then we can combine this branch with Num and Int.
                Some(mono_id) => match mono_types.get(mono_id) {
                    MonoType::Primitive(primitive) => char_to_int(*primitive, char, problems),
                    other => {
                        return compiler_bug!(
                            Problem::CharSpecializedToWrongType(Some(*other)),
                            region
                        );
                    }
                },
                None => {
                    return compiler_bug!(Problem::CharSpecializedToWrongType(None), region);
                }
            },
            Expr::Str(contents) => MonoExpr::Str(
                self.string_interns
                    .get(self.arena, self.arena.alloc(contents)),
            ),
            Expr::EmptyRecord => {
                // Empty records are zero-sized and should be discarded.
                return None;
            }
            Expr::Record { record_var, fields } => {
                // Sort the fields alphabetically by name.
                let mut fields = sort_fields(fields, self.arena);

                // Reserve a slice of IDs up front. This is so that we have a contiguous array
                // of field IDs at the end of this, each corresponding to the appropriate record field.
                let field_ids: Slice<MonoExprId> = self.mono_exprs.reserve_ids(fields.len() as u16);
                let mut next_field_id = field_ids.start();

                // Generate a MonoExpr for each field, using the reserved IDs so that we end up with
                // that Slice being populated with the exprs in the fields, with the correct ordering.
                fields.retain(|(_name, field)| {
                    let loc_expr = field.loc_expr;
                    self.to_mono_expr(loc_expr.value, loc_expr.region, || unsafe {
                        // Safety: This will run *at most* field.len() times, possibly less,
                        // so this will never create an index that's out of bounds.
                        let answer = MonoExprId::new_unchecked(Index::new(next_field_id));
                        next_field_id += 1;
                        Some(answer)
                    })
                    // Discard all the zero-sized fields as we go. We don't need to keep the contents
                    // of the Option because we already know it's the ID we passed in.
                    .is_some()
                });

                // If we dropped any fields because they were being zero-sized,
                // drop the same number of reserved IDs so that they still line up.
                field_ids.truncate(fields.len() as u16);

                // If all fields ended up being zero-sized, this would compile to an empty record; return None.
                let field_ids = field_ids.into_nonempty_slice()?;

                let todo = (); // TODO: store debuginfo for the record type, including ideally type alias and/or opaque type names.

                MonoExpr::Struct(field_ids)
            }
            _ => todo!(),
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

        let mono_expr_id = get_expr_id()?;

        self.mono_exprs.insert(mono_expr_id, mono_expr, region);

        Some(mono_expr_id)
    }
}

/// Convert a number literal (e.g. `42`) or integer literal (e.g. `0x42`) to a monomorphized type.
/// Nums are allowed to convert to either an integer or a fraction. Integer literals should have
/// given a compile-time error if they ended up unifying to a fractional type, but we can
/// gracefully allow them to compile to that type anyway.
fn to_num(primitive: Primitive, val: IntValue, problems: &mut impl Push<Problem>) -> MonoExpr {
    match primitive {
        // These are ordered roughly by most to least common integer types
        Primitive::U8 => MonoExpr::Number(Number::U8(val.as_i128() as u8)),
        Primitive::I8 => MonoExpr::Number(Number::I8(val.as_i128() as i8)),
        Primitive::U16 => MonoExpr::Number(Number::U16(val.as_i128() as u16)),
        Primitive::I16 => MonoExpr::Number(Number::I16(val.as_i128() as i16)),
        Primitive::U32 => MonoExpr::Number(Number::U32(val.as_i128() as u32)),
        Primitive::I32 => MonoExpr::Number(Number::I32(val.as_i128() as i32)),
        Primitive::U64 => MonoExpr::Number(Number::U64(val.as_i128() as u64)),
        Primitive::I64 => MonoExpr::Number(Number::I64(val.as_i128() as i64)),
        Primitive::F32 => MonoExpr::Number(Number::F32(val.as_i128() as f32)),
        Primitive::F64 => MonoExpr::Number(Number::F64(val.as_i128() as f64)),
        Primitive::Dec => MonoExpr::Number(Number::Dec(match val {
            IntValue::I128(bytes) => i128::from_ne_bytes(bytes) as f64,
            IntValue::U128(bytes) => u128::from_ne_bytes(bytes) as f64,
        })),
        Primitive::U128 => MonoExpr::Number(Number::U128(val.as_u128())),
        Primitive::I128 => MonoExpr::Number(Number::I128(val.as_i128())),
        Primitive::Str | Primitive::Crash => {
            let problem = Problem::NumSpecializedToWrongType(Some(MonoType::Primitive(primitive)));
            problems.push(problem);
            MonoExpr::CompilerBug(problem)
        }
    }
}

/// Convert a fractional literal (e.g. `0.5`) to a monomorphized type.
/// If somehow its type was not a fractional type, that's a compiler bug!
fn to_frac(primitive: Primitive, val: f64, problems: &mut impl Push<Problem>) -> MonoExpr {
    match primitive {
        // These are ordered roughly by most to least common fractional types
        Primitive::F32 => MonoExpr::Number(Number::F32(val as f32)),
        Primitive::F64 => MonoExpr::Number(Number::F64(val)),
        Primitive::Dec => MonoExpr::Number(Number::Dec(val)),
        Primitive::U8
        | Primitive::I8
        | Primitive::U16
        | Primitive::I16
        | Primitive::U32
        | Primitive::I32
        | Primitive::U64
        | Primitive::I64
        | Primitive::U128
        | Primitive::I128
        | Primitive::Str
        | Primitive::Crash => {
            let problem = Problem::NumSpecializedToWrongType(Some(MonoType::Primitive(primitive)));
            problems.push(problem);
            MonoExpr::CompilerBug(problem)
        }
    }
}

/// Convert a single-quote character (e.g. `'r'`) to a monomorphized type.
/// If somehow its type was not an integer type, that's a compiler bug!
fn char_to_int(primitive: Primitive, ch: char, problems: &mut impl Push<Problem>) -> MonoExpr {
    match primitive {
        // These are ordered roughly by most to least common character types
        Primitive::U8 => MonoExpr::Number(Number::U8(ch as u8)),
        Primitive::U64 => MonoExpr::Number(Number::U64(ch as u64)),
        Primitive::U16 => MonoExpr::Number(Number::U16(ch as u16)),
        Primitive::U32 => MonoExpr::Number(Number::U32(ch as u32)),
        Primitive::U128 => MonoExpr::Number(Number::U128(ch as u128)),
        Primitive::I64 => MonoExpr::Number(Number::I64(ch as i64)),
        Primitive::I32 => MonoExpr::Number(Number::I32(ch as i32)),
        Primitive::I128 => MonoExpr::Number(Number::I128(ch as i128)),
        Primitive::I16 => MonoExpr::Number(Number::I16(ch as i16)),
        Primitive::I8 => MonoExpr::Number(Number::I8(ch as i8)),
        Primitive::Str | Primitive::Dec | Primitive::F32 | Primitive::F64 | Primitive::Crash => {
            let problem = Problem::CharSpecializedToWrongType(Some(MonoType::Primitive(primitive)));
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
