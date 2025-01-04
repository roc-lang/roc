use crate::{
    mono_ir::{MonoExpr, MonoExprId, MonoExprs, WhenBranch, WhenBranches},
    mono_module::Interns,
    mono_num::Number,
    mono_type::{MonoType, MonoTypes, Primitive},
    specialize_type::{MonoTypeCache, Problem, RecordFieldIds, TupleElemIds},
    DebugInfo, MonoPattern, MonoPatterns, MonoTypeId,
};
use bumpalo::{collections::Vec, Bump};
use roc_can::{
    expr::{Expr, IntValue},
    pattern::Pattern,
};
use roc_collections::{Push, VecMap};
use roc_module::symbol::ModuleId;
use roc_region::all::Region;
use roc_solve::module::Solved;
use roc_types::subs::{Content, Subs, Variable};
use soa::NonEmptySlice;

/// Function bodies that have already been specialized.
pub struct MonoFnCache {
    inner: VecMap<(ModuleId, Variable, MonoTypeId), MonoExprId>,
}

impl MonoFnCache {
    pub fn monomorphize_fn<'a, F: 'a + FnOnce(ModuleId, Variable) -> &'a Expr>(
        &mut self,
        // Sometimes we need to create specializations of functions that are defined in other modules.
        module_id: ModuleId,
        // The function Variable stored in the original function's canonical Expr. We use this as a way to
        // uniquely identify the function expr within its module, since each fn Expr gets its own unique var.
        // Doing it with Variable instead of IdentId lets us cache specializations of anonymous functions too.
        fn_var: Variable,
        // Given a ModuleId and Variable (to uniquely identify the canonical fn Expr within its module),
        // get the canonical Expr of the function itself. We need this to create a specialization of it.
        // TODO [mono2]
        _get_fn_expr: F,
        // This tells us which specialization of the function we want.
        mono_type_id: MonoTypeId,
    ) -> MonoExprId {
        *self
            .inner
            .get_or_insert((module_id, fn_var, mono_type_id), || {
                todo!("TODO lower the fn_expr using Env etc. (May need to add args to this method, not sure.)");
            })
    }
}

pub struct Env<'a, 'c, 'd, 'i, 's, 't, 'p, 'w, P> {
    arena: &'a Bump,
    subs: &'s mut Subs,
    types_cache: &'c mut MonoTypeCache,
    mono_types: &'t mut MonoTypes,
    mono_exprs: &'t mut MonoExprs,
    mono_patterns: &'p mut MonoPatterns,
    when_branches: &'w mut WhenBranches,
    record_field_ids: RecordFieldIds,
    tuple_elem_ids: TupleElemIds,
    debug_info: &'d mut Option<DebugInfo>,
    string_interns: &'i mut Interns<'a>,
    problems: P,
}

impl<'a, 'c, 'd, 'i, 's, 't, 'p, 'w, P: Push<Problem>> Env<'a, 'c, 'd, 'i, 's, 't, 'p, 'w, P> {
    pub fn new(
        arena: &'a Bump,
        subs: &'s mut Solved<Subs>,
        types_cache: &'c mut MonoTypeCache,
        mono_types: &'t mut MonoTypes,
        mono_exprs: &'t mut MonoExprs,
        mono_patterns: &'p mut MonoPatterns,
        when_branches: &'w mut WhenBranches,
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
            mono_patterns,
            when_branches,
            record_field_ids,
            tuple_elem_ids,
            string_interns,
            debug_info,
            problems,
        }
    }

    fn mono_from_var(&mut self, var: Variable) -> MonoTypeId {
        self.types_cache.monomorphize_var(
            self.arena,
            self.subs,
            self.mono_types,
            &mut self.record_field_ids,
            &mut self.tuple_elem_ids,
            &mut self.problems,
            self.debug_info,
            var,
        )
    }

    pub fn to_mono_expr(&mut self, can_expr: &Expr) -> MonoExpr {
        macro_rules! compiler_bug {
            ($problem:expr) => {{
                self.problems.push($problem);
                MonoExpr::CompilerBug($problem)
            }};
        }

        match can_expr {
            Expr::Float(var, _precision_var, _str, val, _bound) => {
                match self.subs.get_content_without_compacting(*var) {
                    Content::FlexVar(_) => {
                        // Plain decimal number literals like `4.2` can still have an unbound var.
                        MonoExpr::Number(Number::Dec(*val))
                    }
                    _ => {
                        let mono_type = self.mono_from_var(*var);
                        match self.mono_types.get(mono_type) {
                            MonoType::Primitive(primitive) => match to_frac(*primitive, *val) {
                                Ok(num) => MonoExpr::Number(num),
                                Err(problem) => compiler_bug!(problem),
                            },
                            other => {
                                compiler_bug!(Problem::NumSpecializedToWrongType(Some(*other)))
                            }
                        }
                    }
                }
            }
            Expr::Num(var, _, int_value, _) | Expr::Int(var, _, _, int_value, _) => {
                let mono_type = self.mono_from_var(*var);

                // Number literals and int literals both specify integer numbers, so to_num() can work on both.
                match self.mono_types.get(mono_type) {
                    MonoType::Primitive(primitive) => match to_num(*primitive, *int_value) {
                        Ok(num) => MonoExpr::Number(num),
                        Err(problem) => compiler_bug!(problem),
                    },
                    other => compiler_bug!(Problem::NumSpecializedToWrongType(Some(*other))),
                }
            }
            Expr::SingleQuote(var, _, char, _) => {
                let mono_type = self.mono_from_var(*var);

                // Single-quote characters monomorphize to an integer.
                // TODO [mono2] if we store these using the same representation as other ints (e.g. Expr::Int,
                // or keeping a separate value but storing an IntValue instead of a char), then
                // even though we verify them differently, we can combine this branch with Num and Int.
                match self.mono_types.get(mono_type) {
                    MonoType::Primitive(primitive) => {
                        char_to_int(*primitive, *char, &mut self.problems)
                    }
                    other => compiler_bug!(Problem::CharSpecializedToWrongType(Some(*other))),
                }
            }
            Expr::Str(contents) => MonoExpr::Str(self.string_interns.get_id(
                self.arena,
                // TODO should be able to remove this alloc_str() once canonical Expr stores an arena-allocated string.
                self.arena.alloc_str(contents),
            )),
            Expr::EmptyRecord => {
                // Empty records are zero-sized and should be discarded.
                MonoExpr::Unit
            }
            Expr::Record {
                record_var: _,
                fields,
            } => {
                // TODO [mono2]: store debuginfo for the record type, including ideally type alias and/or opaque type names. Do this before early-returning for single-field records.

                // Check for records with 0-1 fields before sorting or reserving a slice of IDs (which might be unnecessary).
                // We'll check again after discarding zero-sized fields, because we might end up with 0 or 1 fields remaining.
                if fields.len() <= 1 {
                    return match fields.into_iter().next() {
                        Some((_, field)) => self.to_mono_expr(&field.loc_expr.value),
                        None => MonoExpr::Unit,
                    };
                }

                // Sort the fields alphabetically by name.
                let mut fields = Vec::from_iter_in(fields, self.arena);

                fields.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));

                // We want to end up with a Slice<MonoExpr> of these, so accumulate a buffer of them
                // and then add them to MonoExprs all at once, so they're added as one contiguous slice
                // regardless of what else got added to MonoExprs as we were monomorphizing them.
                let mut buf: Vec<(MonoExpr, Region)> =
                    Vec::with_capacity_in(fields.len(), self.arena);

                buf.extend(fields.into_iter().map(|(_name, field)| {
                    (
                        self.to_mono_expr(&field.loc_expr.value),
                        field.loc_expr.region,
                    )
                }));

                let slice = unsafe {
                    NonEmptySlice::from_slice_unchecked(self.mono_exprs.extend(buf.into_iter()))
                };

                MonoExpr::Struct(slice)
            }
            Expr::If {
                cond_var: _,
                branch_var,
                branches,
                final_else,
            } => {
                let branch_type = self.mono_from_var(*branch_var);

                let mono_final_else = self.to_mono_expr(&final_else.value);
                let final_else = self.mono_exprs.add(mono_final_else, final_else.region);

                let mut branch_pairs: Vec<((MonoExpr, Region), (MonoExpr, Region))> =
                    Vec::with_capacity_in(branches.len(), self.arena);

                for (cond, body) in branches {
                    let mono_cond = self.to_mono_expr(&cond.value);
                    let mono_body = self.to_mono_expr(&body.value);

                    branch_pairs.push(((mono_cond, cond.region), (mono_body, body.region)));
                }

                let branches = self.mono_exprs.extend_pairs(branch_pairs.into_iter());

                MonoExpr::If {
                    branch_type,
                    branches,
                    final_else,
                }
            }
            Expr::Var(symbol, var) | Expr::ParamsVar { symbol, var, .. } => {
                MonoExpr::Lookup(*symbol, self.mono_from_var(*var))
            }
            Expr::When {
                loc_cond,
                cond_var,
                expr_var,
                region: _,
                branches,
                branches_cond_var: _,
                exhaustive: _,
            } => {
                let value_type = self.mono_from_var(*cond_var);
                let branch_type = self.mono_from_var(*expr_var);
                let value = self.to_mono_expr(&loc_cond.value);
                let value_id = self.mono_exprs.add(value, loc_cond.region);

                let branches_slice = self.when_branches.reserve(branches.len());

                for (branch_index, branch) in branches_slice.into_iter().zip(branches.iter()) {
                    let patterns_slice = self.mono_patterns.reserve(branch.patterns.len());

                    for (pattern_id, pattern) in
                        patterns_slice.into_iter().zip(branch.patterns.iter())
                    {
                        let mono_pattern = self.to_mono_pattern(&pattern.pattern.value);
                        self.mono_patterns
                            .insert(pattern_id, mono_pattern, pattern.pattern.region);
                    }

                    let value = self.to_mono_expr(&branch.value.value);

                    let Some(patterns_slice) = NonEmptySlice::from_slice(patterns_slice) else {
                        return compiler_bug!(Problem::WhenBranchHasNoPatterns);
                    };

                    let guard = branch.guard.as_ref().map(|guard| {
                        let mono_guard = self.to_mono_expr(&guard.value);
                        self.mono_exprs.add(mono_guard, guard.region)
                    });

                    let mono_branch = WhenBranch {
                        patterns: patterns_slice,
                        guard,
                        value: self.mono_exprs.add(value, branch.value.region),
                    };

                    self.when_branches.insert(branch_index, mono_branch);
                }

                let Some(branches_slice) = NonEmptySlice::from_slice(branches_slice) else {
                    return compiler_bug!(Problem::WhenHasNoBranches);
                };

                MonoExpr::When {
                    value: value_id,
                    value_type,
                    branch_type,
                    branches: branches_slice,
                }
            }
            Expr::Crash { msg, ret_var } => {
                let mono_msg = self.to_mono_expr(&msg.value);

                MonoExpr::Crash {
                    msg: self.mono_exprs.add(mono_msg, msg.region),
                    ret_type: self.mono_from_var(*ret_var),
                }
            }
            // Expr::Call((fn_var, fn_expr, capture_var, ret_var), args, called_via) => {
            //     let opt_ret_type = self.mono_from_var(*var);

            //     if opt_ret_type.is_none() {
            //         let fn_type = match self.subs.get_content_without_compacting(fn_var) {
            //             Content::Structure(FlatType::Func(arg_vars, closure_var, ret_var)) => {
            //                 let todo = (); // TODO make is_effectful actually use the function's effectfulness!
            //                 let is_effectful = false;

            //                 // Calls to pure functions that return zero-sized types should be discarded.
            //                 if !is_effectful {
            //                     return None;
            //                 }

            //                 // Use the Content we already have to directly monomorphize the function, rather than
            //                 // calling monomorphize_var and having it redo the Subs lookup and conditionals we just did.
            //                 self.types_cache.monomorphize_fn(
            //                     self.subs,
            //                     self.mono_types,
            //                     &mut self.record_field_ids,
            //                     &mut self.tuple_elem_ids,
            //                     &mut self.problems,
            //                     self.debug_info,
            //                     *arg_vars,
            //                     *ret_var,
            //                 )?
            //             }
            //             _ => {
            //                 // This function didn't have a function type. Compiler bug!
            //                 return Some(MonoExpr::CompilerBug(Problem::FnDidNotHaveFnType));
            //             }
            //         };

            //         let todo = (); // TODO this is where we need to specialize, which means...duplicating the fn expr body maybe? and caching it under the mono type?
            //         let fn_expr = self.to_mono_expr(can_expr, stmts)?;
            //         let args = todo!(); // TODO compute the args. This is tricky because of preallocated slices!
            //         let capture_type = self.mono_from_var(*capture_var);

            //         let todo = (); // How do we pre-reserve the statements? Is that possible? It does seem necessary...might not be possible though. Maybe we just need to make Vec rather than Slice on these.

            //         // We aren't returning anything, and this is an effectful function, so just push a statement to call it and move on.
            //         stmts.push(self.mono_stmts.add(MonoStmt::CallVoid {
            //             fn_type,
            //             fn_expr,
            //             args,
            //             capture_type,
            //         }));

            //         None
            //     } else {
            //         let fn_type = self.mono_from_var(*fn_var)?;
            //         let todo = (); // TODO this is where we need to specialize, which means...duplicating the fn expr body maybe? and caching it under the mono type?
            //         let fn_expr = self.to_mono_expr(can_expr, stmts)?;
            //         let args = todo!(); // TODO compute the args. This is tricky because of preallocated slices!
            //         let capture_type = self.mono_from_var(*capture_var);

            //         Some(MonoExpr::Call {
            //             fn_type,
            //             fn_expr,
            //             args,
            //             capture_type,
            //         })
            //     }
            // }
            // Expr::LetNonRec(def, loc) => {
            //     let expr = self.to_mono_expr(def.loc_expr.value, stmts)?;
            //     let todo = (); // TODO if this is an underscore pattern and we're doing a fn call, convert it to Stmt::CallVoid
            //     let pattern = self.to_mono_pattern(def.loc_pattern.value);

            //     // TODO do we need to use any of these other fields? e.g. for the types?
            //     // pub struct Def {
            //     //     pub loc_pattern: Loc<Pattern>,
            //     //     pub loc_expr: Loc<Expr>,
            //     //     pub expr_var: Variable,
            //     //     pub pattern_vars: SendMap<Symbol, Variable>,
            //     //     pub annotation: Option<Annotation>,
            //     // }

            //     todo!("split up the pattern into various Assign statements.");
            // }
            // Expr::LetRec(vec, loc, illegal_cycle_mark) => todo!(),
            _ => todo!("{:?}", can_expr),
            // Expr::List {
            //     elem_var,
            //     loc_elems,
            // } => todo!(),
            // Expr::IngestedFile(path_buf, arc, variable) => todo!(),
            // Expr::ParamsVar {
            //     symbol,
            //     var,
            //     params_symbol,
            //     params_var,
            // } => todo!(),
            // Expr::AbilityMember(symbol, specialization_id, variable) => todo!(),
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
            // Expr::RuntimeError(_runtime_error) => {
            //     todo!("generate a MonoExpr::Crash based on the runtime error");
            // }
        }
    }

    pub fn to_mono_pattern(&mut self, can_pattern: &Pattern) -> MonoPattern {
        macro_rules! compiler_bug {
            ($problem:expr) => {{
                self.problems.push($problem);
                MonoPattern::CompilerBug($problem)
            }};
        }

        match can_pattern {
            Pattern::Identifier(ident) => MonoPattern::Identifier(*ident),
            Pattern::As(loc_pattern, ident) => {
                let mono_pattern = self.to_mono_pattern(&loc_pattern.value);
                let pattern_id = self.mono_patterns.add(mono_pattern, loc_pattern.region);
                MonoPattern::As(pattern_id, *ident)
            }
            Pattern::AppliedTag { .. } => todo!(),
            Pattern::UnwrappedOpaque { .. } => todo!(),
            Pattern::RecordDestructure { .. } => todo!(),
            Pattern::TupleDestructure { .. } => todo!(),
            Pattern::List { .. } => todo!(),
            Pattern::NumLiteral(var, _, int_value, _)
            | Pattern::IntLiteral(var, _, _, int_value, _) => {
                let mono_type = self.mono_from_var(*var);

                match self.mono_types.get(mono_type) {
                    MonoType::Primitive(primitive) => match to_num(*primitive, *int_value) {
                        Ok(num) => MonoPattern::NumberLiteral(num),
                        Err(problem) => MonoPattern::CompilerBug(problem),
                    },
                    other => {
                        compiler_bug!(Problem::NumSpecializedToWrongType(Some(*other)))
                    }
                }
            }
            Pattern::FloatLiteral(var, _, _, float_value, _) => {
                let mono_type = self.mono_from_var(*var);

                match self.mono_types.get(mono_type) {
                    MonoType::Primitive(primitive) => match to_frac(*primitive, *float_value) {
                        Ok(num) => MonoPattern::NumberLiteral(num),
                        Err(problem) => MonoPattern::CompilerBug(problem),
                    },
                    other => {
                        compiler_bug!(Problem::NumSpecializedToWrongType(Some(*other)))
                    }
                }
            }
            Pattern::StrLiteral(contents) => {
                MonoPattern::StrLiteral(self.string_interns.get_id(
                    self.arena,
                    // TODO [mono2]: remove by making can store arena-allocated strings.
                    self.arena.alloc_str(contents),
                ))
            }
            Pattern::SingleQuote(_, _, _, _) => todo!(),
            Pattern::Underscore => MonoPattern::Underscore,
            Pattern::AbilityMemberSpecialization { .. } => todo!(),
            Pattern::Shadowed(_, _, _) => todo!(),
            Pattern::OpaqueNotInScope(_) => todo!(),
            Pattern::UnsupportedPattern(_) => todo!(),
            Pattern::MalformedPattern(_, _) => todo!(),
        }
    }
}

/// Convert a number literal (e.g. `42`) or integer literal (e.g. `0x42`) to a monomorphized type.
/// Nums are allowed to convert to either an integer or a fraction. Integer literals should have
/// given a compile-time error if they ended up unifying to a fractional type, but we can
/// gracefully allow them to compile to that type anyway.
fn to_num(primitive: Primitive, val: IntValue) -> Result<Number, Problem> {
    match primitive {
        // These are ordered roughly by most to least common integer types
        Primitive::U8 => Ok(Number::U8(val.as_i128() as u8)),
        Primitive::I8 => Ok(Number::I8(val.as_i128() as i8)),
        Primitive::U16 => Ok(Number::U16(val.as_i128() as u16)),
        Primitive::I16 => Ok(Number::I16(val.as_i128() as i16)),
        Primitive::U32 => Ok(Number::U32(val.as_i128() as u32)),
        Primitive::I32 => Ok(Number::I32(val.as_i128() as i32)),
        Primitive::U64 => Ok(Number::U64(val.as_i128() as u64)),
        Primitive::I64 => Ok(Number::I64(val.as_i128() as i64)),
        Primitive::F32 => Ok(Number::F32(val.as_i128() as f32)),
        Primitive::F64 => Ok(Number::F64(val.as_i128() as f64)),
        Primitive::Dec => Ok(Number::Dec(match val {
            IntValue::I128(bytes) => i128::from_ne_bytes(bytes) as f64,
            IntValue::U128(bytes) => u128::from_ne_bytes(bytes) as f64,
        })),
        Primitive::U128 => Ok(Number::U128(val.as_u128())),
        Primitive::I128 => Ok(Number::I128(val.as_i128())),
        Primitive::Str | Primitive::Crash | Primitive::Bool => Err(
            Problem::NumSpecializedToWrongType(Some(MonoType::Primitive(primitive))),
        ),
    }
}

/// Convert a fractional literal (e.g. `0.5`) to a monomorphized type.
/// If somehow its type was not a fractional type, that's a compiler bug!
fn to_frac(primitive: Primitive, val: f64) -> Result<Number, Problem> {
    match primitive {
        // These are ordered roughly by most to least common fractional types
        Primitive::F32 => Ok(Number::F32(val as f32)),
        Primitive::F64 => Ok(Number::F64(val)),
        Primitive::Dec => Ok(Number::Dec(val)),
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
        | Primitive::Crash
        | Primitive::Bool => Err(Problem::NumSpecializedToWrongType(Some(
            MonoType::Primitive(primitive),
        ))),
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
        Primitive::Str
        | Primitive::Dec
        | Primitive::F32
        | Primitive::F64
        | Primitive::Crash
        | Primitive::Bool => {
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
