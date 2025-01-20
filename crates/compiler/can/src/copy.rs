use crate::{
    def::{Def, DefKind},
    expr::{
        ClosureData, Expr, Field, OpaqueWrapFunctionData, StructAccessorData, WhenBranchPattern,
    },
    pattern::{
        DestructType, ListPatterns, Pattern, RecordDestruct, RecordDestructureSpread, TupleDestruct,
    },
};
use roc_collections::soa::{index_push_new, slice_extend_new};
use roc_module::{
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_types::{
    subs::{
        self, AliasVariables, Descriptor, GetSubsSlice, OptVariable, RecordFields, Subs, SubsIndex,
        SubsSlice, TupleElems, UnionLambdas, UnionTags, Variable,
    },
    types::{RecordField, Uls},
};

trait CopyEnv {
    #[inline(always)]
    fn clear_source_copy(&mut self, var: Variable) {
        self.mut_source().modify(var, |descriptor| {
            if descriptor.copy.into_variable().is_some() {
                descriptor.copy = OptVariable::NONE;
            } else {
                debug_assert!(false, "{var:?} marked as copied but it wasn't");
            }
        })
    }

    #[inline(always)]
    fn source_root_var(&self, var: Variable) -> Variable {
        self.source().get_root_key_without_compacting(var)
    }

    #[inline(always)]
    fn source_desc(&self, var: Variable) -> Descriptor {
        self.source().get_without_compacting(var)
    }

    #[inline(always)]
    fn set_source_copy(&mut self, var: Variable, copy: OptVariable) {
        self.mut_source().set_copy(var, copy)
    }

    #[inline(always)]
    fn get_copy(&self, var: Variable) -> OptVariable {
        self.source().get_copy(var)
    }

    #[inline(always)]
    fn target_fresh(&mut self, descriptor: Descriptor) -> Variable {
        self.target().fresh(descriptor)
    }

    fn mut_source(&mut self) -> &mut Subs;

    fn source(&self) -> &Subs;

    fn target(&mut self) -> &mut Subs;

    fn clone_name(&mut self, name: SubsIndex<Lowercase>) -> SubsIndex<Lowercase>;

    fn clone_field_names(&mut self, field_names: SubsSlice<Lowercase>) -> SubsSlice<Lowercase>;

    fn clone_tuple_elem_indices(
        &mut self,
        tuple_elem_indices: SubsSlice<usize>,
    ) -> SubsSlice<usize>;

    fn clone_tag_names(&mut self, tag_names: SubsSlice<TagName>) -> SubsSlice<TagName>;

    fn clone_lambda_names(&mut self, lambda_names: SubsSlice<Symbol>) -> SubsSlice<Symbol>;

    fn clone_record_fields(
        &mut self,
        record_fields: SubsSlice<RecordField<()>>,
    ) -> SubsSlice<RecordField<()>>;
}

impl CopyEnv for Subs {
    #[inline(always)]
    fn mut_source(&mut self) -> &mut Subs {
        self
    }

    #[inline(always)]
    fn source(&self) -> &Subs {
        self
    }

    #[inline(always)]
    fn target(&mut self) -> &mut Subs {
        self
    }

    #[inline(always)]
    fn clone_name(&mut self, name: SubsIndex<Lowercase>) -> SubsIndex<Lowercase> {
        name
    }

    #[inline(always)]
    fn clone_field_names(&mut self, field_names: SubsSlice<Lowercase>) -> SubsSlice<Lowercase> {
        field_names
    }

    #[inline(always)]
    fn clone_tuple_elem_indices(
        &mut self,
        tuple_elem_indices: SubsSlice<usize>,
    ) -> SubsSlice<usize> {
        tuple_elem_indices
    }

    #[inline(always)]
    fn clone_tag_names(&mut self, tag_names: SubsSlice<TagName>) -> SubsSlice<TagName> {
        tag_names
    }

    #[inline(always)]
    fn clone_lambda_names(&mut self, lambda_names: SubsSlice<Symbol>) -> SubsSlice<Symbol> {
        lambda_names
    }

    #[inline(always)]
    fn clone_record_fields(
        &mut self,
        record_fields: SubsSlice<RecordField<()>>,
    ) -> SubsSlice<RecordField<()>> {
        record_fields
    }
}

struct AcrossSubs<'a> {
    source: &'a mut Subs,
    target: &'a mut Subs,
}

impl<'a> CopyEnv for AcrossSubs<'a> {
    #[inline(always)]
    fn mut_source(&mut self) -> &mut Subs {
        self.source
    }

    #[inline(always)]
    fn source(&self) -> &Subs {
        self.source
    }

    #[inline(always)]
    fn target(&mut self) -> &mut Subs {
        self.target
    }

    #[inline(always)]
    fn clone_name(&mut self, name: SubsIndex<Lowercase>) -> SubsIndex<Lowercase> {
        index_push_new(&mut self.target.field_names, self.source[name].clone())
    }

    #[inline(always)]
    fn clone_field_names(&mut self, field_names: SubsSlice<Lowercase>) -> SubsSlice<Lowercase> {
        slice_extend_new(
            &mut self.target.field_names,
            self.source.get_subs_slice(field_names).iter().cloned(),
        )
    }

    #[inline(always)]
    fn clone_tuple_elem_indices(
        &mut self,
        tuple_elem_indices: SubsSlice<usize>,
    ) -> SubsSlice<usize> {
        slice_extend_new(
            &mut self.target.tuple_elem_indices,
            self.source
                .get_subs_slice(tuple_elem_indices)
                .iter()
                .cloned(),
        )
    }

    #[inline(always)]
    fn clone_tag_names(&mut self, tag_names: SubsSlice<TagName>) -> SubsSlice<TagName> {
        slice_extend_new(
            &mut self.target.tag_names,
            self.source.get_subs_slice(tag_names).iter().cloned(),
        )
    }

    #[inline(always)]
    fn clone_lambda_names(&mut self, lambda_names: SubsSlice<Symbol>) -> SubsSlice<Symbol> {
        slice_extend_new(
            &mut self.target.symbol_names,
            self.source.get_subs_slice(lambda_names).iter().cloned(),
        )
    }

    #[inline(always)]
    fn clone_record_fields(
        &mut self,
        record_fields: SubsSlice<RecordField<()>>,
    ) -> SubsSlice<RecordField<()>> {
        slice_extend_new(
            &mut self.target.record_fields,
            self.source.get_subs_slice(record_fields).iter().copied(),
        )
    }
}

pub fn deep_copy_type_vars_into_expr(
    subs: &mut Subs,
    var: Variable,
    expr: &Expr,
) -> Option<(Variable, Expr)> {
    deep_copy_expr_top(subs, var, expr)
}

pub fn deep_copy_expr_across_subs(
    source: &mut Subs,
    target: &mut Subs,
    var: Variable,
    expr: &Expr,
) -> (Variable, Expr) {
    let mut across_subs = AcrossSubs { source, target };
    deep_copy_expr_top(&mut across_subs, var, expr).unwrap()
}

/// Deep copies all type variables in [`expr`].
/// Returns [`None`] if the expression does not need to be copied.
fn deep_copy_expr_top<C: CopyEnv>(
    env: &mut C,
    var: Variable,
    expr: &Expr,
) -> Option<(Variable, Expr)> {
    // Always deal with the root, so that aliases propagate correctly.
    let expr_var = env.source_root_var(var);

    let mut copied = Vec::with_capacity(16);

    let copy_expr_var = deep_copy_type_vars(env, &mut copied, expr_var);

    if copied.is_empty() {
        return None;
    }

    let copied_expr = deep_copy_expr_help(env, &mut copied, expr);

    // we have tracked all visited variables, and can now traverse them
    // in one go (without looking at the UnificationTable) and clear the copy field
    for var in copied {
        env.clear_source_copy(var);
    }

    Some((copy_expr_var, copied_expr))
}

fn deep_copy_expr_help<C: CopyEnv>(env: &mut C, copied: &mut Vec<Variable>, expr: &Expr) -> Expr {
    use Expr::*;

    macro_rules! sub {
        ($var:expr) => {{
            deep_copy_type_vars(env, copied, $var)
        }};
    }

    macro_rules! go_help {
        ($expr:expr) => {{
            deep_copy_expr_help(env, copied, $expr)
        }};
    }

    match expr {
        Num(var, str, val, bound) => Num(sub!(*var), str.clone(), *val, *bound),
        Int(v1, v2, str, val, bound) => Int(sub!(*v1), sub!(*v2), str.clone(), *val, *bound),
        Float(v1, v2, str, val, bound) => Float(sub!(*v1), sub!(*v2), str.clone(), *val, *bound),
        Str(str) => Str(str.clone()),
        SingleQuote(v1, v2, char, bound) => SingleQuote(sub!(*v1), sub!(*v2), *char, *bound),
        IngestedFile(file_path, bytes, var) => {
            IngestedFile(file_path.clone(), bytes.clone(), sub!(*var))
        }
        List {
            elem_var,
            loc_elems,
        } => List {
            elem_var: sub!(*elem_var),
            loc_elems: loc_elems.iter().map(|le| le.map(|e| go_help!(e))).collect(),
        },
        Var(sym, var) => Var(*sym, sub!(*var)),
        ParamsVar {
            symbol,
            var,
            params_symbol,
            params_var,
        } => ParamsVar {
            symbol: *symbol,
            var: sub!(*var),
            params_symbol: *params_symbol,
            params_var: sub!(*params_var),
        },
        ImportParams(module_id, region, opt_provided) => ImportParams(
            *module_id,
            *region,
            opt_provided
                .as_ref()
                .map(|(var, expr)| (sub!(*var), Box::new(go_help!(&expr)))),
        ),
        &AbilityMember(sym, specialization, specialization_var) => {
            AbilityMember(sym, specialization, sub!(specialization_var))
        }
        When {
            loc_cond,
            cond_var,
            expr_var,
            region,
            branches,
            branches_cond_var,
            exhaustive,
        } => When {
            loc_cond: Box::new(loc_cond.map(|e| go_help!(e))),
            cond_var: sub!(*cond_var),
            expr_var: sub!(*expr_var),
            region: *region,
            branches: branches
                .iter()
                .map(
                    |crate::expr::WhenBranch {
                         patterns,
                         value,
                         guard,
                         redundant,
                     }| crate::expr::WhenBranch {
                        patterns: patterns
                            .iter()
                            .map(
                                |WhenBranchPattern {
                                     pattern,
                                     degenerate,
                                 }| WhenBranchPattern {
                                    pattern: pattern
                                        .map(|p| deep_copy_pattern_help(env, copied, p)),
                                    degenerate: *degenerate,
                                },
                            )
                            .collect(),
                        value: value.map(|e| go_help!(e)),
                        guard: guard.as_ref().map(|le| le.map(|e| go_help!(e))),
                        redundant: *redundant,
                    },
                )
                .collect(),
            branches_cond_var: sub!(*branches_cond_var),
            exhaustive: *exhaustive,
        },
        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => If {
            cond_var: sub!(*cond_var),
            branch_var: sub!(*branch_var),
            branches: branches
                .iter()
                .map(|(c, e)| (c.map(|e| go_help!(e)), e.map(|e| go_help!(e))))
                .collect(),
            final_else: Box::new(final_else.map(|e| go_help!(e))),
        },

        LetRec(defs, body, cycle_mark) => LetRec(
            defs.iter()
                .map(
                    |Def {
                         loc_pattern,
                         loc_expr,
                         expr_var,
                         pattern_vars,
                         annotation,
                         kind,
                     }| Def {
                        loc_pattern: loc_pattern.map(|p| deep_copy_pattern_help(env, copied, p)),
                        loc_expr: loc_expr.map(|e| go_help!(e)),
                        expr_var: sub!(*expr_var),
                        pattern_vars: pattern_vars.iter().map(|(s, v)| (*s, sub!(*v))).collect(),
                        // Annotation should only be used in constraining, don't clone before
                        // constraining :)
                        annotation: annotation.clone(),
                        kind: match kind {
                            DefKind::Let => DefKind::Let,
                            DefKind::Stmt(v) => DefKind::Stmt(sub!(*v)),
                            DefKind::Ignored(v) => DefKind::Ignored(sub!(*v)),
                        },
                    },
                )
                .collect(),
            Box::new(body.map(|e| go_help!(e))),
            *cycle_mark,
        ),
        LetNonRec(def, body) => {
            let Def {
                loc_pattern,
                loc_expr,
                expr_var,
                pattern_vars,
                annotation,
                kind,
            } = &**def;
            let def = Def {
                loc_pattern: loc_pattern.map(|p| deep_copy_pattern_help(env, copied, p)),
                loc_expr: loc_expr.map(|e| go_help!(e)),
                expr_var: sub!(*expr_var),
                pattern_vars: pattern_vars.iter().map(|(s, v)| (*s, sub!(*v))).collect(),
                // Annotation should only be used in constraining, don't clone before
                // constraining :)
                annotation: annotation.clone(),
                kind: *kind,
            };
            LetNonRec(Box::new(def), Box::new(body.map(|e| go_help!(e))))
        }

        Call(f, args, called_via) => {
            let (fn_var, fn_expr, clos_var, ret_var, fx_var) = &**f;
            Call(
                Box::new((
                    sub!(*fn_var),
                    fn_expr.map(|e| go_help!(e)),
                    sub!(*clos_var),
                    sub!(*ret_var),
                    sub!(*fx_var),
                )),
                args.iter()
                    .map(|(var, expr)| (sub!(*var), expr.map(|e| go_help!(e))))
                    .collect(),
                *called_via,
            )
        }
        Crash { msg, ret_var } => Crash {
            msg: Box::new(msg.map(|m| go_help!(m))),
            ret_var: sub!(*ret_var),
        },
        RunLowLevel { op, args, ret_var } => RunLowLevel {
            op: *op,
            args: args
                .iter()
                .map(|(var, expr)| (sub!(*var), go_help!(expr)))
                .collect(),
            ret_var: sub!(*ret_var),
        },
        ForeignCall {
            foreign_symbol,
            args,
            ret_var,
        } => ForeignCall {
            foreign_symbol: foreign_symbol.clone(),
            args: args
                .iter()
                .map(|(var, expr)| (sub!(*var), go_help!(expr)))
                .collect(),
            ret_var: sub!(*ret_var),
        },

        Closure(ClosureData {
            function_type,
            closure_type,
            return_type,
            fx_type,
            early_returns,
            name,
            captured_symbols,
            recursive,
            arguments,
            loc_body,
        }) => Closure(ClosureData {
            function_type: sub!(*function_type),
            closure_type: sub!(*closure_type),
            return_type: sub!(*return_type),
            fx_type: sub!(*fx_type),
            early_returns: early_returns
                .iter()
                .map(|(var, region, type_)| (sub!(*var), *region, *type_))
                .collect(),
            name: *name,
            captured_symbols: captured_symbols
                .iter()
                .map(|(s, v)| (*s, sub!(*v)))
                .collect(),
            recursive: *recursive,
            arguments: arguments
                .iter()
                .map(|(v, mark, pat)| {
                    (
                        sub!(*v),
                        *mark,
                        pat.map(|p| deep_copy_pattern_help(env, copied, p)),
                    )
                })
                .collect(),
            loc_body: Box::new(loc_body.map(|e| go_help!(e))),
        }),

        Record { record_var, fields } => Record {
            record_var: sub!(*record_var),
            fields: fields
                .iter()
                .map(
                    |(
                        k,
                        Field {
                            var,
                            region,
                            loc_expr,
                        },
                    )| {
                        (
                            k.clone(),
                            Field {
                                var: sub!(*var),
                                region: *region,
                                loc_expr: Box::new(loc_expr.map(|e| go_help!(e))),
                            },
                        )
                    },
                )
                .collect(),
        },

        EmptyRecord => EmptyRecord,

        Tuple { tuple_var, elems } => Tuple {
            tuple_var: sub!(*tuple_var),
            elems: elems
                .iter()
                .map(|(var, loc_expr)| (sub!(*var), Box::new(loc_expr.map(|e| go_help!(e)))))
                .collect(),
        },

        RecordAccess {
            record_var,
            ext_var,
            field_var,
            loc_expr,
            field,
        } => RecordAccess {
            record_var: sub!(*record_var),
            ext_var: sub!(*ext_var),
            field_var: sub!(*field_var),
            loc_expr: Box::new(loc_expr.map(|e| go_help!(e))),
            field: field.clone(),
        },

        RecordAccessor(StructAccessorData {
            name,
            function_var,
            record_var,
            closure_var,
            ext_var,
            field_var,
            field,
        }) => RecordAccessor(StructAccessorData {
            name: *name,
            function_var: sub!(*function_var),
            record_var: sub!(*record_var),
            closure_var: sub!(*closure_var),
            ext_var: sub!(*ext_var),
            field_var: sub!(*field_var),
            field: field.clone(),
        }),

        TupleAccess {
            tuple_var,
            ext_var,
            elem_var,
            loc_expr,
            index,
        } => TupleAccess {
            tuple_var: sub!(*tuple_var),
            ext_var: sub!(*ext_var),
            elem_var: sub!(*elem_var),
            loc_expr: Box::new(loc_expr.map(|e| go_help!(e))),
            index: *index,
        },

        RecordUpdate {
            record_var,
            ext_var,
            symbol,
            updates,
        } => RecordUpdate {
            record_var: sub!(*record_var),
            ext_var: sub!(*ext_var),
            symbol: *symbol,
            updates: updates
                .iter()
                .map(
                    |(
                        k,
                        Field {
                            var,
                            region,
                            loc_expr,
                        },
                    )| {
                        (
                            k.clone(),
                            Field {
                                var: sub!(*var),
                                region: *region,
                                loc_expr: Box::new(loc_expr.map(|e| go_help!(e))),
                            },
                        )
                    },
                )
                .collect(),
        },

        Tag {
            tag_union_var: variant_var,
            ext_var,
            name,
            arguments,
        } => Tag {
            tag_union_var: sub!(*variant_var),
            ext_var: sub!(*ext_var),
            name: name.clone(),
            arguments: arguments
                .iter()
                .map(|(v, e)| (sub!(*v), e.map(|e| go_help!(e))))
                .collect(),
        },

        ZeroArgumentTag {
            closure_name,
            variant_var,
            ext_var,
            name,
        } => ZeroArgumentTag {
            closure_name: *closure_name,
            variant_var: sub!(*variant_var),
            ext_var: sub!(*ext_var),
            name: name.clone(),
        },

        OpaqueRef {
            opaque_var,
            name,
            argument,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        } => OpaqueRef {
            opaque_var: sub!(*opaque_var),
            name: *name,
            argument: Box::new((sub!(argument.0), argument.1.map(|e| go_help!(e)))),
            // These shouldn't matter for opaques during mono, because they are only used for reporting
            // and pretty-printing to the user. During mono we decay immediately into the argument.
            // NB: if there are bugs, check if not substituting here is the problem!
            specialized_def_type: specialized_def_type.clone(),
            type_arguments: type_arguments.clone(),
            lambda_set_variables: lambda_set_variables.clone(),
        },

        OpaqueWrapFunction(OpaqueWrapFunctionData {
            opaque_name,
            opaque_var,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
            function_name,
            function_var,
            argument_var,
            closure_var,
        }) => OpaqueWrapFunction(OpaqueWrapFunctionData {
            opaque_name: *opaque_name,
            opaque_var: sub!(*opaque_var),
            function_name: *function_name,
            function_var: sub!(*function_var),
            argument_var: sub!(*argument_var),
            closure_var: sub!(*closure_var),
            // The following three are only used for constraining
            specialized_def_type: specialized_def_type.clone(),
            type_arguments: type_arguments.clone(),
            lambda_set_variables: lambda_set_variables.clone(),
        }),

        Expect {
            loc_condition,
            loc_continuation,
            lookups_in_cond,
        } => Expect {
            loc_condition: Box::new(loc_condition.map(|e| go_help!(e))),
            loc_continuation: Box::new(loc_continuation.map(|e| go_help!(e))),
            lookups_in_cond: lookups_in_cond.to_vec(),
        },

        Return {
            return_value,
            return_var,
        } => Return {
            return_value: Box::new(return_value.map(|e| go_help!(e))),
            return_var: sub!(*return_var),
        },

        Dbg {
            source_location,
            source,
            loc_message,
            loc_continuation,
            variable,
            symbol,
        } => Dbg {
            source_location: source_location.clone(),
            source: source.clone(),
            loc_message: Box::new(loc_message.map(|e| go_help!(e))),
            loc_continuation: Box::new(loc_continuation.map(|e| go_help!(e))),
            variable: sub!(*variable),
            symbol: *symbol,
        },

        Try {
            result_expr,
            result_var,
            return_var,
            ok_payload_var,
            err_payload_var,
            err_ext_var,
            kind,
        } => Try {
            result_expr: Box::new(result_expr.map(|e| go_help!(e))),
            result_var: sub!(*result_var),
            return_var: sub!(*return_var),
            ok_payload_var: sub!(*ok_payload_var),
            err_payload_var: sub!(*err_payload_var),
            err_ext_var: sub!(*err_ext_var),
            kind: *kind,
        },

        RuntimeError(err) => RuntimeError(err.clone()),
    }
}

fn deep_copy_pattern_help<C: CopyEnv>(
    env: &mut C,
    copied: &mut Vec<Variable>,
    pat: &Pattern,
) -> Pattern {
    use Pattern::*;

    macro_rules! sub {
        ($var:expr) => {{
            deep_copy_type_vars(env, copied, $var)
        }};
    }

    macro_rules! go_help {
        ($pat:expr) => {{
            deep_copy_pattern_help(env, copied, $pat)
        }};
    }

    match pat {
        Identifier(s) => Identifier(*s),
        As(subpattern, s) => As(Box::new(subpattern.map(|p| go_help!(p))), *s),
        AppliedTag {
            whole_var,
            ext_var,
            tag_name,
            arguments,
        } => AppliedTag {
            whole_var: sub!(*whole_var),
            ext_var: sub!(*ext_var),
            tag_name: tag_name.clone(),
            arguments: arguments
                .iter()
                .map(|(var, lp)| (sub!(*var), lp.map(|p| go_help!(p))))
                .collect(),
        },
        UnwrappedOpaque {
            whole_var,
            opaque,
            argument,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        } => {
            let (arg_var, arg_pat) = &**argument;
            UnwrappedOpaque {
                whole_var: sub!(*whole_var),
                opaque: *opaque,
                argument: Box::new((sub!(*arg_var), arg_pat.map(|p| go_help!(p)))),
                // These are only used for constraining, they shouldn't matter where pattern
                // cloning is useful (in monomorphization or during deriving)
                specialized_def_type: specialized_def_type.clone(),
                type_arguments: type_arguments.clone(),
                lambda_set_variables: lambda_set_variables.clone(),
            }
        }
        RecordDestructure {
            whole_var,
            ext_var,
            destructs,
            opt_spread,
        } => RecordDestructure {
            whole_var: sub!(*whole_var),
            ext_var: sub!(*ext_var),
            destructs: destructs
                .iter()
                .map(|lrd| {
                    lrd.map(
                        |RecordDestruct {
                             var,
                             label,
                             symbol,
                             typ,
                         }| RecordDestruct {
                            var: sub!(*var),
                            label: label.clone(),
                            symbol: *symbol,
                            typ: match typ {
                                DestructType::Required => DestructType::Required,
                                DestructType::Optional(var, expr) => DestructType::Optional(
                                    sub!(*var),
                                    expr.map(|e| deep_copy_expr_help(env, copied, e)),
                                ),
                                DestructType::Guard(var, pat) => {
                                    DestructType::Guard(sub!(*var), pat.map(|p| go_help!(p)))
                                }
                            },
                        },
                    )
                })
                .collect(),
            opt_spread: Box::new(opt_spread.clone().map(
                |RecordDestructureSpread {
                     opt_pattern,
                     spread_var,
                 }| RecordDestructureSpread {
                    opt_pattern: opt_pattern.map(|opt_pat| {
                        opt_pat
                            .as_ref()
                            .map(|loc_pat| loc_pat.map(|pat| go_help!(&pat)))
                    }),
                    spread_var: sub!(spread_var),
                },
            )),
        },
        TupleDestructure {
            whole_var,
            ext_var,
            destructs,
        } => TupleDestructure {
            whole_var: sub!(*whole_var),
            ext_var: sub!(*ext_var),
            destructs: destructs
                .iter()
                .map(|lrd| {
                    lrd.map(
                        |TupleDestruct {
                             destruct_index: index,
                             var,
                             typ: (tyvar, pat),
                         }: &crate::pattern::TupleDestruct| TupleDestruct {
                            destruct_index: *index,
                            var: sub!(*var),
                            typ: (sub!(*tyvar), pat.map(|p| go_help!(p))),
                        },
                    )
                })
                .collect(),
        },
        List {
            list_var,
            elem_var,
            patterns: ListPatterns { patterns, opt_rest },
        } => List {
            list_var: sub!(*list_var),
            elem_var: sub!(*elem_var),
            patterns: ListPatterns {
                patterns: patterns.iter().map(|lp| lp.map(|p| go_help!(p))).collect(),
                opt_rest: *opt_rest,
            },
        },
        NumLiteral(var, s, n, bound) => NumLiteral(sub!(*var), s.clone(), *n, *bound),
        IntLiteral(v1, v2, s, n, bound) => IntLiteral(sub!(*v1), sub!(*v2), s.clone(), *n, *bound),
        FloatLiteral(v1, v2, s, n, bound) => {
            FloatLiteral(sub!(*v1), sub!(*v2), s.clone(), *n, *bound)
        }
        StrLiteral(s) => StrLiteral(s.clone()),
        SingleQuote(v1, v2, c, bound) => SingleQuote(sub!(*v1), sub!(*v2), *c, *bound),
        Underscore => Underscore,
        AbilityMemberSpecialization { ident, specializes } => AbilityMemberSpecialization {
            ident: *ident,
            specializes: *specializes,
        },
        Shadowed(region, ident, symbol) => Shadowed(*region, ident.clone(), *symbol),
        OpaqueNotInScope(ident) => OpaqueNotInScope(ident.clone()),
        UnsupportedPattern(region) => UnsupportedPattern(*region),
        MalformedPattern(problem, region) => MalformedPattern(*problem, *region),
    }
}

/// Deep copies the type variables in [`var`], returning a map of original -> new type variable for
/// all type variables copied.
#[inline]
fn deep_copy_type_vars<C: CopyEnv>(
    env: &mut C,
    copied: &mut Vec<Variable>,
    var: Variable,
) -> Variable {
    // Always deal with the root, so that unified variables are treated the same.
    let var = env.source_root_var(var);

    let cloned_var = help(env, copied, var);

    return cloned_var;

    #[must_use]
    #[inline]
    fn help<C: CopyEnv>(env: &mut C, visited: &mut Vec<Variable>, var: Variable) -> Variable {
        use roc_types::subs::Content::*;
        use roc_types::subs::FlatType::*;

        // Always deal with the root, so that unified variables are treated the same.
        let var = env.source_root_var(var);

        let desc = env.source_desc(var);

        // Unlike `deep_copy_var` in solve, here we are cloning *all* flex and rigid vars.
        // So we only want to short-circuit if we've already done the cloning work for a particular
        // var.
        if let Some(copy) = desc.copy.into_variable() {
            return copy;
        }

        let content = desc.content;

        let copy_descriptor = Descriptor {
            content: Error, // we'll update this below
            rank: desc.rank,
            mark: desc.mark,
            copy: OptVariable::NONE,
        };

        let copy = env.target_fresh(copy_descriptor);
        env.set_source_copy(var, copy.into());

        visited.push(var);

        macro_rules! descend_slice {
            ($slice:expr) => {
                for var_index in $slice {
                    let var = env.source()[var_index];
                    let _ = help(env, visited, var);
                }
            };
        }

        macro_rules! descend_var {
            ($var:expr) => {{
                help(env, visited, $var)
            }};
        }

        macro_rules! clone_var_slice {
            ($slice:expr) => {{
                let new_arguments = env.target().reserve_into_vars($slice.len());
                for (target_index, var_index) in (new_arguments.indices()).zip($slice) {
                    let var = env.source()[var_index];
                    let copy_var = env.get_copy(var).into_variable().unwrap_or(var);

                    env.target().variables[target_index] = copy_var;
                }
                new_arguments
            }};
        }

        macro_rules! perform_clone {
            ($do_clone:expr) => {{
                // It may the case that while deep-copying nested variables of this type, we
                // ended up copying the type itself (notably if it was self-referencing, in a
                // recursive type). In that case, short-circuit with the known copy.
                // if let Some(copy) = subs.get_ref(var).copy.into_variable() {
                //     return copy;
                // }
                // Perform the clone.
                $do_clone
            }};
        }

        // Now we recursively copy the content of the variable.
        // We have already marked the variable as copied, so we
        // will not repeat this work or crawl this variable again.
        let new_content = match content {
            // The vars for which we want to do something interesting.
            FlexVar(opt_name) => FlexVar(opt_name.map(|n| env.clone_name(n))),
            FlexAbleVar(opt_name, abilities) => FlexAbleVar(
                opt_name.map(|n| env.clone_name(n)),
                env.clone_lambda_names(abilities),
            ),
            RigidVar(name) => RigidVar(env.clone_name(name)),
            RigidAbleVar(name, abilities) => {
                RigidAbleVar(env.clone_name(name), env.clone_lambda_names(abilities))
            }

            // Everything else is a mechanical descent.
            Structure(flat_type) => match flat_type {
                EmptyRecord | EmptyTagUnion | EffectfulFunc => Structure(flat_type),
                Apply(symbol, arguments) => {
                    descend_slice!(arguments);

                    perform_clone!({
                        let new_arguments = clone_var_slice!(arguments);
                        Structure(Apply(symbol, new_arguments))
                    })
                }
                Func(arguments, closure_var, ret_var, fx_var) => {
                    descend_slice!(arguments);

                    let new_closure_var = descend_var!(closure_var);
                    let new_ret_var = descend_var!(ret_var);
                    let new_fx_var = descend_var!(fx_var);

                    perform_clone!({
                        let new_arguments = clone_var_slice!(arguments);
                        Structure(Func(
                            new_arguments,
                            new_closure_var,
                            new_ret_var,
                            new_fx_var,
                        ))
                    })
                }
                Record(fields, ext_var) => {
                    let new_ext_var = descend_var!(ext_var);

                    descend_slice!(fields.variables());

                    perform_clone!({
                        let new_variables = clone_var_slice!(fields.variables());
                        let new_field_names = env.clone_field_names(fields.field_names());
                        let new_record_fields = env.clone_record_fields(fields.record_fields());

                        let new_fields = {
                            RecordFields {
                                length: fields.length,
                                field_names_start: new_field_names.start(),
                                variables_start: new_variables.start(),
                                field_types_start: new_record_fields.start(),
                            }
                        };

                        Structure(Record(new_fields, new_ext_var))
                    })
                }
                Tuple(elems, ext_var) => {
                    let new_ext_var = descend_var!(ext_var);

                    descend_slice!(elems.variables());

                    perform_clone!({
                        let new_variables = clone_var_slice!(elems.variables());
                        let new_elem_indices = env.clone_tuple_elem_indices(elems.elem_indices());

                        let new_elems = {
                            TupleElems {
                                length: elems.length,
                                variables_start: new_variables.start(),
                                elem_index_start: new_elem_indices.start(),
                            }
                        };

                        Structure(Tuple(new_elems, new_ext_var))
                    })
                }
                TagUnion(tags, ext_var) => {
                    let new_ext_var = ext_var.map(|v| descend_var!(v));

                    for variables_slice_index in tags.variables() {
                        let variables_slice = env.source()[variables_slice_index];
                        descend_slice!(variables_slice);
                    }

                    perform_clone!({
                        let new_variable_slices = env.target().reserve_variable_slices(tags.len());
                        let it = (new_variable_slices.indices()).zip(tags.variables());
                        for (target_index, index) in it {
                            let slice = env.source()[index];
                            let new_variables = clone_var_slice!(slice);
                            env.target().variable_slices[target_index] = new_variables;
                        }
                        let new_tag_names = env.clone_tag_names(tags.labels());

                        let new_union_tags =
                            UnionTags::from_slices(new_tag_names, new_variable_slices);

                        Structure(TagUnion(new_union_tags, new_ext_var))
                    })
                }
                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let new_ext_var = ext_var.map(|v| descend_var!(v));
                    let new_rec_var = descend_var!(rec_var);

                    for variables_slice_index in tags.variables() {
                        let variables_slice = env.source()[variables_slice_index];
                        descend_slice!(variables_slice);
                    }

                    perform_clone!({
                        let new_variable_slices = env.target().reserve_variable_slices(tags.len());
                        let it = (new_variable_slices.indices()).zip(tags.variables());
                        for (target_index, index) in it {
                            let slice = env.source()[index];
                            let new_variables = clone_var_slice!(slice);

                            env.target().variable_slices[target_index] = new_variables;
                        }
                        let new_tag_names = env.clone_tag_names(tags.labels());

                        let new_union_tags =
                            UnionTags::from_slices(new_tag_names, new_variable_slices);

                        Structure(RecursiveTagUnion(new_rec_var, new_union_tags, new_ext_var))
                    })
                }
                FunctionOrTagUnion(tag_names, symbols, ext_var) => {
                    let new_ext_var = ext_var.map(|v| descend_var!(v));
                    let new_tag_names = env.clone_tag_names(tag_names);
                    let new_symbols = env.clone_lambda_names(symbols);
                    perform_clone!(Structure(FunctionOrTagUnion(
                        new_tag_names,
                        new_symbols,
                        new_ext_var
                    )))
                }
            },

            RecursionVar {
                opt_name,
                structure,
            } => {
                let new_structure = descend_var!(structure);
                let new_opt_name = opt_name.map(|n| env.clone_name(n));

                perform_clone!({
                    RecursionVar {
                        opt_name: new_opt_name,
                        structure: new_structure,
                    }
                })
            }

            Alias(symbol, arguments, real_type_var, kind) => {
                let new_real_type_var = descend_var!(real_type_var);
                descend_slice!(arguments.all_variables());

                perform_clone!({
                    let new_variables = clone_var_slice!(arguments.all_variables());
                    let new_arguments = AliasVariables {
                        variables_start: new_variables.start(),
                        ..arguments
                    };

                    Alias(symbol, new_arguments, new_real_type_var, kind)
                })
            }

            LambdaSet(subs::LambdaSet {
                solved,
                recursion_var,
                unspecialized,
                ambient_function,
            }) => {
                let new_rec_var = recursion_var.map(|var| descend_var!(var));
                for variables_slice_index in solved.variables() {
                    let variables_slice = env.source()[variables_slice_index];
                    descend_slice!(variables_slice);
                }
                for uls_index in unspecialized {
                    let Uls(var, _, _) = env.source()[uls_index];
                    let _ignored = descend_var!(var);
                }
                let new_ambient_function = descend_var!(ambient_function);

                perform_clone!({
                    let new_variable_slices = env.target().reserve_variable_slices(solved.len());
                    let it = (new_variable_slices.indices()).zip(solved.variables());
                    for (target_index, index) in it {
                        let slice = env.source()[index];
                        let new_variables = clone_var_slice!(slice);

                        env.target().variable_slices[target_index] = new_variables;
                    }

                    let new_solved_labels = env.clone_lambda_names(solved.labels());

                    let new_solved =
                        UnionLambdas::from_slices(new_solved_labels, new_variable_slices);

                    let new_unspecialized = env.target().reserve_uls_slice(unspecialized.len());
                    for (target_index, uls_index) in
                        (new_unspecialized.into_iter()).zip(unspecialized.into_iter())
                    {
                        let Uls(var, sym, region) = env.source()[uls_index];
                        let copy_var = env.get_copy(var).into_variable().unwrap_or(var);

                        env.target()[target_index] = Uls(copy_var, sym, region);
                        env.target().uls_of_var.add(copy_var, copy);
                    }

                    LambdaSet(subs::LambdaSet {
                        solved: new_solved,
                        recursion_var: new_rec_var,
                        unspecialized: new_unspecialized,
                        ambient_function: new_ambient_function,
                    })
                })
            }
            ErasedLambda => ErasedLambda,
            Pure => Pure,
            Effectful => Effectful,

            RangedNumber(range) => {
                perform_clone!(RangedNumber(range))
            }
            Error => Error,
        };

        env.target().set_content(copy, new_content);

        copy
    }
}

#[cfg(test)]
mod test {
    use crate::{
        copy::{deep_copy_type_vars_into_expr, AcrossSubs},
        expr::Expr,
    };

    use super::{deep_copy_expr_across_subs, deep_copy_type_vars};
    use roc_collections::soa::{index_push_new, slice_extend_new};
    use roc_error_macros::internal_error;
    use roc_module::{ident::TagName, symbol::Symbol};
    use roc_region::all::Loc;
    use roc_types::{
        subs::{
            self, Content, Content::*, Descriptor, FlatType, GetSubsSlice, Mark, OptVariable, Rank,
            Subs, Variable,
        },
        types::Uls,
    };

    #[cfg(test)]
    fn new_var(subs: &mut Subs, content: Content) -> Variable {
        subs.fresh(Descriptor {
            content,
            rank: Rank::toplevel(),
            mark: Mark::NONE,
            copy: OptVariable::NONE,
        })
    }

    #[test]
    fn copy_flex_var() {
        let mut subs = Subs::new();

        let field_name = index_push_new(&mut subs.field_names, "a".into());
        let var = new_var(&mut subs, FlexVar(Some(field_name)));

        let mut copied = vec![];

        let copy = deep_copy_type_vars(&mut subs, &mut copied, var);

        assert_ne!(var, copy);

        match subs.get_content_without_compacting(copy) {
            FlexVar(Some(name)) => {
                assert_eq!(subs[*name].as_str(), "a");
            }
            it => unreachable!("{:?}", it),
        }
    }

    #[test]
    fn copy_rigid_var() {
        let mut subs = Subs::new();

        let field_name = index_push_new(&mut subs.field_names, "a".into());
        let var = new_var(&mut subs, RigidVar(field_name));

        let mut copied = vec![];

        let copy = deep_copy_type_vars(&mut subs, &mut copied, var);

        assert_ne!(var, copy);

        match subs.get_content_without_compacting(var) {
            RigidVar(name) => {
                assert_eq!(subs[*name].as_str(), "a");
            }
            it => unreachable!("{:?}", it),
        }
    }

    #[test]
    fn copy_flex_able_var() {
        let mut subs = Subs::new();

        let field_name = index_push_new(&mut subs.field_names, "a".into());
        let abilities = slice_extend_new(&mut subs.symbol_names, [Symbol::UNDERSCORE]);
        let var = new_var(&mut subs, FlexAbleVar(Some(field_name), abilities));

        let mut copied = vec![];

        let copy = deep_copy_type_vars(&mut subs, &mut copied, var);

        assert_ne!(var, copy);

        match subs.get_content_without_compacting(var) {
            FlexAbleVar(Some(name), abilities) => {
                assert_eq!(subs[*name].as_str(), "a");
                assert_eq!(subs.get_subs_slice(*abilities), [Symbol::UNDERSCORE]);
            }
            it => unreachable!("{:?}", it),
        }
    }

    #[test]
    fn copy_rigid_able_var() {
        let mut subs = Subs::new();

        let field_name = index_push_new(&mut subs.field_names, "a".into());
        let abilities = slice_extend_new(&mut subs.symbol_names, [Symbol::UNDERSCORE]);
        let var = new_var(&mut subs, RigidAbleVar(field_name, abilities));

        let mut copied = vec![];

        let copy = deep_copy_type_vars(&mut subs, &mut copied, var);

        assert_ne!(var, copy);
        match subs.get_content_without_compacting(var) {
            RigidAbleVar(name, abilities) => {
                assert_eq!(subs[*name].as_str(), "a");
                assert_eq!(subs.get_subs_slice(*abilities), [Symbol::UNDERSCORE]);
            }
            it => internal_error!("{:?}", it),
        }
    }

    #[test]
    fn copy_deep_expr() {
        let mut subs = Subs::new();

        let a = index_push_new(&mut subs.field_names, "a".into());
        let b = index_push_new(&mut subs.field_names, "b".into());
        let var1 = new_var(&mut subs, FlexVar(Some(a)));
        let var2 = new_var(&mut subs, FlexVar(Some(b)));

        let expr = Expr::Tag {
            tag_union_var: var1,
            ext_var: Variable::EMPTY_TAG_UNION,
            name: TagName("F".into()),
            arguments: vec![(
                var2,
                Loc::at_zero(Expr::Tag {
                    tag_union_var: var2,
                    ext_var: Variable::EMPTY_TAG_UNION,
                    name: TagName("G".into()),
                    arguments: vec![],
                }),
            )],
        };

        let (var, expr) = deep_copy_type_vars_into_expr(&mut subs, var1, &expr).unwrap();

        assert!(subs.get_copy(var1).is_none());
        assert!(subs.get_copy(var2).is_none());

        match expr {
            Expr::Tag {
                tag_union_var: variant_var,
                ext_var,
                name,
                mut arguments,
            } => {
                assert_ne!(var1, variant_var);
                assert_ne!(var2, variant_var);

                match subs.get_content_without_compacting(variant_var) {
                    FlexVar(Some(name)) => {
                        assert_eq!(subs[*name].as_str(), "a");
                    }
                    it => panic!("{it:?}"),
                }
                assert_eq!(var, variant_var);
                assert!(matches!(
                    subs.get_content_without_compacting(ext_var),
                    Content::Structure(FlatType::EmptyTagUnion)
                ));
                assert_eq!(name.0.as_str(), "F");

                assert_eq!(arguments.len(), 1);
                let (v2, arg) = arguments.pop().unwrap();
                assert_ne!(var1, v2);
                assert_ne!(var2, v2);
                match subs.get_content_without_compacting(v2) {
                    FlexVar(Some(name)) => {
                        assert_eq!(subs[*name].as_str(), "b");
                    }
                    it => panic!("{it:?}"),
                }

                match arg.value {
                    Expr::Tag {
                        tag_union_var: variant_var,
                        ext_var,
                        name,
                        arguments,
                    } => {
                        assert_eq!(variant_var, v2);
                        assert!(matches!(
                            subs.get_content_without_compacting(ext_var),
                            Content::Structure(FlatType::EmptyTagUnion)
                        ));
                        assert_eq!(name.0.as_str(), "G");
                        assert_eq!(arguments.len(), 0);
                    }
                    e => panic!("{e:?}"),
                }
            }
            e => panic!("{e:?}"),
        }
    }

    #[test]
    fn copy_deep_expr_across_subs() {
        let mut source = Subs::new();
        let mut target = Subs::new();

        let a = index_push_new(&mut source.field_names, "a".into());
        let b = index_push_new(&mut source.field_names, "b".into());
        let var1 = new_var(&mut source, FlexVar(Some(a)));
        let var2 = new_var(&mut source, FlexVar(Some(b)));

        let expr = Expr::Tag {
            tag_union_var: var1,
            ext_var: Variable::EMPTY_TAG_UNION,
            name: TagName("F".into()),
            arguments: vec![(
                var2,
                Loc::at_zero(Expr::Tag {
                    tag_union_var: var2,
                    ext_var: Variable::EMPTY_TAG_UNION,
                    name: TagName("G".into()),
                    arguments: vec![],
                }),
            )],
        };

        let (var, expr) = deep_copy_expr_across_subs(&mut source, &mut target, var1, &expr);

        assert!(source.get_copy(var1).is_none());
        assert!(source.get_copy(var2).is_none());

        match expr {
            Expr::Tag {
                tag_union_var: variant_var,
                ext_var,
                name,
                mut arguments,
            } => {
                match target.get_content_without_compacting(variant_var) {
                    FlexVar(Some(name)) => {
                        assert_eq!(target[*name].as_str(), "a");
                    }
                    it => panic!("{it:?}"),
                }
                assert_eq!(var, variant_var);
                assert!(matches!(
                    target.get_content_without_compacting(ext_var),
                    Content::Structure(FlatType::EmptyTagUnion)
                ));
                assert_eq!(name.0.as_str(), "F");

                assert_eq!(arguments.len(), 1);
                let (v2, arg) = arguments.pop().unwrap();
                match target.get_content_without_compacting(v2) {
                    FlexVar(Some(name)) => {
                        assert_eq!(target[*name].as_str(), "b");
                    }
                    it => panic!("{it:?}"),
                }

                match arg.value {
                    Expr::Tag {
                        tag_union_var: variant_var,
                        ext_var,
                        name,
                        arguments,
                    } => {
                        assert_eq!(variant_var, v2);
                        assert!(matches!(
                            target.get_content_without_compacting(ext_var),
                            Content::Structure(FlatType::EmptyTagUnion)
                        ));
                        assert_eq!(name.0.as_str(), "G");
                        assert_eq!(arguments.len(), 0);
                    }
                    e => panic!("{e:?}"),
                }
            }
            e => panic!("{e:?}"),
        }
    }

    #[test]
    fn copy_across_subs_preserve_uls_of_var() {
        let mut source = Subs::new();
        let mut target = Subs::new();

        let a = new_var(&mut source, FlexVar(None));
        let uls = slice_extend_new(
            &mut source.unspecialized_lambda_sets,
            vec![Uls(a, Symbol::UNDERSCORE, 3)],
        );
        let lambda_set_var = new_var(
            &mut source,
            LambdaSet(subs::LambdaSet {
                solved: Default::default(),
                recursion_var: OptVariable::NONE,
                unspecialized: uls,
                ambient_function: Variable::NULL,
            }),
        );

        source.uls_of_var.add(a, lambda_set_var);

        assert!(target.uls_of_var.is_empty());

        let mut env = AcrossSubs {
            source: &mut source,
            target: &mut target,
        };
        let mut copied = vec![];
        let copy_uls = deep_copy_type_vars(&mut env, &mut copied, lambda_set_var);
        let copy_a = deep_copy_type_vars(&mut env, &mut copied, a);

        assert!(matches!(
            target.get_content_without_compacting(copy_a),
            Content::FlexVar(None)
        ));
        assert!(matches!(
            target.get_content_without_compacting(copy_uls),
            Content::LambdaSet(..)
        ));

        let uls_of_var: Vec<_> = target
            .remove_dependent_unspecialized_lambda_sets(copy_a)
            .collect();
        assert_eq!(uls_of_var.len(), 1);
        assert_eq!(uls_of_var[0], copy_uls);
    }
}
