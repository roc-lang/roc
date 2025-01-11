use roc_can::expr::{
    ClosureData, Expr, Field, OpaqueWrapFunctionData, StructAccessorData, WhenBranch,
    WhenBranchPattern,
};
use roc_region::all::Loc;
use roc_types::subs::{
    AliasVariables, Content, Descriptor, FlatType, LambdaSet, RecordFields, Subs, SubsSlice,
    TupleElems, UnionLabels, Variable, VariableSubsSlice,
};
use roc_types::types::{Type, Uls};

pub fn monomorphize(expr: Loc<Expr>, subs: &mut Subs) -> Loc<Expr> {
    Loc {
        region: expr.region,
        value: monomorphize_expr(expr.value, subs),
    }
}

fn monomorphize_expr(expr: Expr, subs: &mut Subs) -> Expr {
    match expr {
        Expr::Num(var, str, int_value, bound) => {
            Expr::Num(monomorphize_var(var, subs), str, int_value, bound)
        }
        Expr::Int(var1, var2, str, int_value, bound) => Expr::Int(
            monomorphize_var(var1, subs),
            monomorphize_var(var2, subs),
            str,
            int_value,
            bound,
        ),
        Expr::Float(var1, var2, str, float_value, bound) => Expr::Float(
            monomorphize_var(var1, subs),
            monomorphize_var(var2, subs),
            str,
            float_value,
            bound,
        ),
        Expr::Str(s) => Expr::Str(s),
        Expr::IngestedFile(path, bytes, var) => {
            Expr::IngestedFile(path, bytes, monomorphize_var(var, subs))
        }
        Expr::SingleQuote(var1, var2, c, bound) => Expr::SingleQuote(
            monomorphize_var(var1, subs),
            monomorphize_var(var2, subs),
            c,
            bound,
        ),
        Expr::List {
            elem_var,
            loc_elems,
        } => Expr::List {
            elem_var: monomorphize_var(elem_var, subs),
            loc_elems: loc_elems
                .into_iter()
                .map(|loc_elem| monomorphize(loc_elem, subs))
                .collect(),
        },
        Expr::Var(symbol, var) => Expr::Var(symbol, monomorphize_var(var, subs)),
        Expr::ParamsVar {
            symbol,
            var,
            params_symbol,
            params_var,
        } => Expr::ParamsVar {
            symbol,
            var: monomorphize_var(var, subs),
            params_symbol,
            params_var: monomorphize_var(params_var, subs),
        },
        Expr::AbilityMember(symbol, spec_id, var) => {
            Expr::AbilityMember(symbol, spec_id, monomorphize_var(var, subs))
        }
        Expr::When {
            cond_var,
            expr_var,
            region,
            loc_cond,
            branches,
            branches_cond_var,
            exhaustive,
        } => Expr::When {
            cond_var: monomorphize_var(cond_var, subs),
            expr_var: monomorphize_var(expr_var, subs),
            region,
            loc_cond: Box::new(monomorphize(*loc_cond, subs)),
            branches: branches
                .into_iter()
                .map(|branch| monomorphize_when_branch(branch, subs))
                .collect(),
            branches_cond_var: monomorphize_var(branches_cond_var, subs),
            exhaustive,
        },
        Expr::If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => Expr::If {
            cond_var: monomorphize_var(cond_var, subs),
            branch_var: monomorphize_var(branch_var, subs),
            branches: branches
                .into_iter()
                .map(|(cond, expr)| (monomorphize(cond, subs), monomorphize(expr, subs)))
                .collect(),
            final_else: Box::new(monomorphize(*final_else, subs)),
        },
        Expr::LetRec(defs, expr, cycle_mark) => Expr::LetRec(
            defs.into_iter()
                .map(|def| monomorphize_def(def, subs))
                .collect(),
            Box::new(monomorphize(*expr, subs)),
            cycle_mark,
        ),
        Expr::LetNonRec(def, expr) => Expr::LetNonRec(
            Box::new(monomorphize_def(*def, subs)),
            Box::new(monomorphize(*expr, subs)),
        ),
        Expr::Call(boxed, args, called_via) => {
            let (fn_var, loc_expr, lambda_set_var, ret_var) = *boxed;
            Expr::Call(
                Box::new((
                    monomorphize_var(fn_var, subs),
                    monomorphize(loc_expr, subs),
                    monomorphize_var(lambda_set_var, subs),
                    monomorphize_var(ret_var, subs),
                )),
                args.into_iter()
                    .map(|(var, loc_expr)| {
                        (monomorphize_var(var, subs), monomorphize(loc_expr, subs))
                    })
                    .collect(),
                called_via,
            )
        }
        Expr::Closure(closure_data) => Expr::Closure(monomorphize_closure_data(closure_data, subs)),
        Expr::Record { record_var, fields } => Expr::Record {
            record_var: monomorphize_var(record_var, subs),
            fields: fields
                .into_iter()
                .map(|(k, v)| (k, monomorphize_field(v, subs)))
                .collect(),
        },
        Expr::EmptyRecord => Expr::EmptyRecord,
        Expr::Tuple { tuple_var, elems } => Expr::Tuple {
            tuple_var: monomorphize_var(tuple_var, subs),
            elems: elems
                .into_iter()
                .map(|(var, loc_expr)| {
                    (
                        monomorphize_var(var, subs),
                        Box::new(monomorphize(*loc_expr, subs)),
                    )
                })
                .collect(),
        },
        Expr::ImportParams(module_id, region, params) => Expr::ImportParams(
            module_id,
            region,
            params.map(|(var, expr)| {
                (
                    monomorphize_var(var, subs),
                    Box::new(monomorphize_expr(*expr, subs)),
                )
            }),
        ),
        Expr::Crash { msg, ret_var } => Expr::Crash {
            msg: Box::new(monomorphize(*msg, subs)),
            ret_var: monomorphize_var(ret_var, subs),
        },
        Expr::RecordAccess {
            record_var,
            ext_var,
            field_var,
            loc_expr,
            field,
        } => Expr::RecordAccess {
            record_var: monomorphize_var(record_var, subs),
            ext_var: monomorphize_var(ext_var, subs),
            field_var: monomorphize_var(field_var, subs),
            loc_expr: Box::new(monomorphize(*loc_expr, subs)),
            field,
        },
        Expr::RecordAccessor(data) => {
            Expr::RecordAccessor(monomorphize_struct_accessor_data(data, subs))
        }
        Expr::TupleAccess {
            tuple_var,
            ext_var,
            elem_var,
            loc_expr,
            index,
        } => Expr::TupleAccess {
            tuple_var: monomorphize_var(tuple_var, subs),
            ext_var: monomorphize_var(ext_var, subs),
            elem_var: monomorphize_var(elem_var, subs),
            loc_expr: Box::new(monomorphize(*loc_expr, subs)),
            index,
        },
        Expr::RecordUpdate {
            record_var,
            ext_var,
            symbol,
            updates,
        } => Expr::RecordUpdate {
            record_var: monomorphize_var(record_var, subs),
            ext_var: monomorphize_var(ext_var, subs),
            symbol,
            updates: updates
                .into_iter()
                .map(|(k, v)| (k, monomorphize_field(v, subs)))
                .collect(),
        },
        Expr::Tag {
            tag_union_var,
            ext_var,
            name,
            arguments,
        } => Expr::Tag {
            tag_union_var: monomorphize_var(tag_union_var, subs),
            ext_var: monomorphize_var(ext_var, subs),
            name,
            arguments: arguments
                .into_iter()
                .map(|(var, loc_expr)| (monomorphize_var(var, subs), monomorphize(loc_expr, subs)))
                .collect(),
        },
        Expr::ZeroArgumentTag {
            closure_name,
            variant_var,
            ext_var,
            name,
        } => Expr::ZeroArgumentTag {
            closure_name,
            variant_var: monomorphize_var(variant_var, subs),
            ext_var: monomorphize_var(ext_var, subs),
            name,
        },
        Expr::OpaqueRef {
            opaque_var,
            name,
            argument,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        } => Expr::OpaqueRef {
            opaque_var: monomorphize_var(opaque_var, subs),
            name,
            argument: Box::new((
                monomorphize_var(argument.0, subs),
                monomorphize(argument.1, subs),
            )),
            specialized_def_type: Box::new(monomorphize_type(*specialized_def_type, subs)),
            type_arguments,
            lambda_set_variables,
        },
        Expr::OpaqueWrapFunction(data) => {
            Expr::OpaqueWrapFunction(monomorphize_opaque_wrap_function_data(data, subs))
        }
        Expr::Expect {
            loc_condition,
            loc_continuation,
            lookups_in_cond,
        } => Expr::Expect {
            loc_condition: Box::new(monomorphize(*loc_condition, subs)),
            loc_continuation: Box::new(monomorphize(*loc_continuation, subs)),
            lookups_in_cond,
        },
        Expr::ExpectFx {
            loc_condition,
            loc_continuation,
            lookups_in_cond,
        } => Expr::ExpectFx {
            loc_condition: Box::new(monomorphize(*loc_condition, subs)),
            loc_continuation: Box::new(monomorphize(*loc_continuation, subs)),
            lookups_in_cond,
        },
        Expr::Dbg {
            source_location,
            source,
            loc_message,
            loc_continuation,
            variable,
            symbol,
        } => Expr::Dbg {
            source_location,
            source,
            loc_message: Box::new(monomorphize(*loc_message, subs)),
            loc_continuation: Box::new(monomorphize(*loc_continuation, subs)),
            variable: monomorphize_var(variable, subs),
            symbol,
        },
        Expr::RuntimeError(error) => Expr::RuntimeError(error),
        Expr::RunLowLevel { op, args, ret_var } => Expr::RunLowLevel {
            op,
            args: args
                .into_iter()
                .map(|(var, expr)| (monomorphize_var(var, subs), monomorphize_expr(expr, subs)))
                .collect(),
            ret_var: monomorphize_var(ret_var, subs),
        },
        Expr::ForeignCall {
            foreign_symbol,
            args,
            ret_var,
        } => Expr::ForeignCall {
            foreign_symbol,
            args: args
                .into_iter()
                .map(|(var, expr)| (monomorphize_var(var, subs), monomorphize_expr(expr, subs)))
                .collect(),
            ret_var: monomorphize_var(ret_var, subs),
        },
    }
}

fn monomorphize_var(var: Variable, subs: &mut Subs) -> Variable {
    let root = subs.get_root_key_without_compacting(var);
    let content = subs.get_content_without_compacting(root).clone();

    match content {
        Content::Structure(flat_type) => match flat_type {
            FlatType::Apply(symbol, args) => {
                let new_args: Vec<Variable> = args
                    .into_iter()
                    .map(|arg| monomorphize_var(subs[arg], subs))
                    .collect();
                let new_slice = VariableSubsSlice::insert_into_subs(subs, new_args);
                let new_flat_type = FlatType::Apply(symbol, new_slice);
                subs.fresh(Descriptor::from(Content::Structure(new_flat_type)))
            }
            FlatType::Func(args, closure_var, ret_var) => {
                let new_args: Vec<Variable> = args
                    .into_iter()
                    .map(|arg| monomorphize_var(subs[arg], subs))
                    .collect();
                let new_args_slice = VariableSubsSlice::insert_into_subs(subs, new_args);
                let new_closure_var = monomorphize_var(closure_var, subs);
                let new_ret_var = monomorphize_var(ret_var, subs);
                let new_flat_type = FlatType::Func(new_args_slice, new_closure_var, new_ret_var);
                subs.fresh(Descriptor::from(Content::Structure(new_flat_type)))
            }
            FlatType::Record(record_fields, ext_var) => {
                let new_variables: Vec<Variable> = record_fields
                    .variables()
                    .into_iter()
                    .map(|v| monomorphize_var(subs[v], subs))
                    .collect();
                let new_variables_slice = VariableSubsSlice::insert_into_subs(subs, new_variables);
                let new_record_fields = RecordFields {
                    length: record_fields.length,
                    field_names_start: record_fields.field_names_start,
                    variables_start: new_variables_slice.start,
                    field_types_start: record_fields.field_types_start,
                };
                let new_ext_var = monomorphize_var(ext_var, subs);
                let new_flat_type = FlatType::Record(new_record_fields, new_ext_var);
                subs.fresh(Descriptor::from(Content::Structure(new_flat_type)))
            }
            FlatType::Tuple(tuple_elems, ext_var) => {
                let new_variables: Vec<Variable> = tuple_elems
                    .variables()
                    .into_iter()
                    .map(|v| monomorphize_var(subs[v], subs))
                    .collect();
                let new_variables_slice = VariableSubsSlice::insert_into_subs(subs, new_variables);
                let new_tuple_elems = TupleElems {
                    length: tuple_elems.length,
                    elem_index_start: tuple_elems.elem_index_start,
                    variables_start: new_variables_slice.start,
                };
                let new_ext_var = monomorphize_var(ext_var, subs);
                let new_flat_type = FlatType::Tuple(new_tuple_elems, new_ext_var);
                subs.fresh(Descriptor::from(Content::Structure(new_flat_type)))
            }
            FlatType::TagUnion(union_labels, tag_ext) => {
                let new_variable_slices =
                    SubsSlice::reserve_variable_slices(subs, union_labels.len());
                for (old_slice_index, new_slice_index) in union_labels
                    .variables()
                    .into_iter()
                    .zip(new_variable_slices)
                {
                    let old_slice = subs[old_slice_index];
                    let new_variables: Vec<Variable> = old_slice
                        .into_iter()
                        .map(|v| monomorphize_var(subs[v], subs))
                        .collect();
                    let new_slice = VariableSubsSlice::insert_into_subs(subs, new_variables);
                    subs[new_slice_index] = new_slice;
                }
                let new_union_labels =
                    UnionLabels::from_slices(union_labels.labels(), new_variable_slices);
                let new_tag_ext = tag_ext.map(|v| monomorphize_var(v, subs));
                let new_flat_type = FlatType::TagUnion(new_union_labels, new_tag_ext);
                subs.fresh(Descriptor::from(Content::Structure(new_flat_type)))
            }
            FlatType::FunctionOrTagUnion(tag_names, symbols, tag_ext) => {
                let new_tag_ext = tag_ext.map(|v| monomorphize_var(v, subs));
                let new_flat_type = FlatType::FunctionOrTagUnion(tag_names, symbols, new_tag_ext);
                subs.fresh(Descriptor::from(Content::Structure(new_flat_type)))
            }
            FlatType::RecursiveTagUnion(rec_var, union_labels, tag_ext) => {
                let new_rec_var = monomorphize_var(rec_var, subs);
                let new_variable_slices =
                    SubsSlice::reserve_variable_slices(subs, union_labels.len());
                for (old_slice_index, new_slice_index) in union_labels
                    .variables()
                    .into_iter()
                    .zip(new_variable_slices)
                {
                    let old_slice = subs[old_slice_index];
                    let new_variables: Vec<Variable> = old_slice
                        .into_iter()
                        .map(|v| monomorphize_var(subs[v], subs))
                        .collect();
                    let new_slice = VariableSubsSlice::insert_into_subs(subs, new_variables);
                    subs[new_slice_index] = new_slice;
                }
                let new_union_labels =
                    UnionLabels::from_slices(union_labels.labels(), new_variable_slices);
                let new_tag_ext = tag_ext.map(|v| monomorphize_var(v, subs));
                let new_flat_type =
                    FlatType::RecursiveTagUnion(new_rec_var, new_union_labels, new_tag_ext);
                subs.fresh(Descriptor::from(Content::Structure(new_flat_type)))
            }
            FlatType::EmptyRecord | FlatType::EmptyTuple | FlatType::EmptyTagUnion => var,
        },
        Content::Alias(symbol, alias_variables, aliased_var, alias_kind) => {
            let new_variables: Vec<Variable> = alias_variables
                .all_variables()
                .into_iter()
                .map(|v| monomorphize_var(subs[v], subs))
                .collect();
            let new_variables_slice = VariableSubsSlice::insert_into_subs(subs, new_variables);
            let new_alias_variables = AliasVariables {
                variables_start: new_variables_slice.start,
                all_variables_len: alias_variables.all_variables_len,
                lambda_set_variables_len: alias_variables.lambda_set_variables_len,
                type_variables_len: alias_variables.type_variables_len,
            };
            let new_aliased_var = monomorphize_var(aliased_var, subs);
            let new_content =
                Content::Alias(symbol, new_alias_variables, new_aliased_var, alias_kind);
            subs.fresh(Descriptor::from(new_content))
        }
        Content::LambdaSet(lambda_set) => {
            let new_solved_slices =
                SubsSlice::reserve_variable_slices(subs, lambda_set.solved.len());
            for (old_slice_index, new_slice_index) in lambda_set
                .solved
                .variables()
                .into_iter()
                .zip(new_solved_slices)
            {
                let old_slice = subs[old_slice_index];
                let new_variables: Vec<Variable> = old_slice
                    .into_iter()
                    .map(|v| monomorphize_var(subs[v], subs))
                    .collect();
                let new_slice = VariableSubsSlice::insert_into_subs(subs, new_variables);
                subs[new_slice_index] = new_slice;
            }
            let new_solved =
                UnionLabels::from_slices(lambda_set.solved.labels(), new_solved_slices);
            let new_recursion_var = lambda_set.recursion_var.map(|v| monomorphize_var(v, subs));
            let new_unspecialized =
                SubsSlice::reserve_uls_slice(subs, lambda_set.unspecialized.len());
            for (i, uls) in lambda_set.unspecialized.into_iter().enumerate() {
                let Uls(var, sym, region) = subs[uls];
                let new_var = monomorphize_var(var, subs);

                if let Some(i) = new_unspecialized.into_iter().nth(i) {
                    subs[i] = Uls(new_var, sym, region);
                } else {
                    debug_panic!("new_unspecialized is too short");
                }
            }
            let new_ambient_function = monomorphize_var(lambda_set.ambient_function, subs);
            let new_lambda_set = LambdaSet {
                solved: new_solved,
                recursion_var: new_recursion_var,
                unspecialized: new_unspecialized,
                ambient_function: new_ambient_function,
            };
            let new_content = Content::LambdaSet(new_lambda_set);
            subs.fresh(Descriptor::from(new_content))
        }
        Content::FlexVar(_)
        | Content::RigidVar(_)
        | Content::FlexAbleVar(_, _)
        | Content::RigidAbleVar(_, _)
        | Content::RecursionVar { .. }
        | Content::ErasedLambda
        | Content::RangedNumber(_)
        | Content::Error => var,
    }
}

fn monomorphize_when_branch(branch: WhenBranch, subs: &mut Subs) -> WhenBranch {
    WhenBranch {
        patterns: branch
            .patterns
            .into_iter()
            .map(|p| WhenBranchPattern {
                pattern: p.pattern,
                degenerate: p.degenerate,
            })
            .collect(),
        value: monomorphize(branch.value, subs),
        guard: branch.guard.map(|g| monomorphize(g, subs)),
        redundant: branch.redundant,
    }
}

fn monomorphize_def(def: roc_can::def::Def, subs: &mut Subs) -> roc_can::def::Def {
    roc_can::def::Def {
        loc_pattern: def.loc_pattern,
        loc_expr: monomorphize(def.loc_expr, subs),
        expr_var: monomorphize_var(def.expr_var, subs),
        pattern_vars: def
            .pattern_vars
            .into_iter()
            .map(|(k, v)| (k, monomorphize_var(v, subs)))
            .collect(),
        annotation: def.annotation.map(|a| monomorphize_annotation(a, subs)),
    }
}

fn monomorphize_closure_data(data: ClosureData, subs: &mut Subs) -> ClosureData {
    ClosureData {
        function_type: monomorphize_var(data.function_type, subs),
        closure_type: monomorphize_var(data.closure_type, subs),
        return_type: monomorphize_var(data.return_type, subs),
        name: data.name,
        captured_symbols: data
            .captured_symbols
            .into_iter()
            .map(|(s, v)| (s, monomorphize_var(v, subs)))
            .collect(),
        recursive: data.recursive,
        arguments: data
            .arguments
            .into_iter()
            .map(|(v, m, p)| (monomorphize_var(v, subs), m, p))
            .collect(),
        loc_body: Box::new(monomorphize(*data.loc_body, subs)),
    }
}

fn monomorphize_field(field: Field, subs: &mut Subs) -> Field {
    Field {
        var: monomorphize_var(field.var, subs),
        region: field.region,
        loc_expr: Box::new(monomorphize(*field.loc_expr, subs)),
    }
}

fn monomorphize_struct_accessor_data(
    data: StructAccessorData,
    subs: &mut Subs,
) -> StructAccessorData {
    StructAccessorData {
        name: data.name,
        function_var: monomorphize_var(data.function_var, subs),
        record_var: monomorphize_var(data.record_var, subs),
        closure_var: monomorphize_var(data.closure_var, subs),
        ext_var: monomorphize_var(data.ext_var, subs),
        field_var: monomorphize_var(data.field_var, subs),
        field: data.field,
    }
}

fn monomorphize_opaque_wrap_function_data(
    data: OpaqueWrapFunctionData,
    subs: &mut Subs,
) -> OpaqueWrapFunctionData {
    OpaqueWrapFunctionData {
        opaque_name: data.opaque_name,
        opaque_var: monomorphize_var(data.opaque_var, subs),
        specialized_def_type: monomorphize_type(data.specialized_def_type, subs),
        type_arguments: data.type_arguments,
        lambda_set_variables: data.lambda_set_variables,
        function_name: data.function_name,
        function_var: monomorphize_var(data.function_var, subs),
        argument_var: monomorphize_var(data.argument_var, subs),
        closure_var: monomorphize_var(data.closure_var, subs),
    }
}

fn monomorphize_annotation(
    annotation: roc_can::def::Annotation,
    subs: &mut Subs,
) -> roc_can::def::Annotation {
    roc_can::def::Annotation {
        signature: monomorphize_type(annotation.signature, subs),
        introduced_variables: annotation.introduced_variables,
        aliases: annotation.aliases,
        region: annotation.region,
    }
}

fn monomorphize_type(typ: Type, subs: &mut Subs) -> Type {
    match typ {
        Type::Tuple(elems, ext) => Type::Tuple(
            elems
                .into_iter()
                .map(|(idx, elem)| (idx, monomorphize_type(elem, subs)))
                .collect(),
            ext,
        ),
        Type::Record(fields, ext) => Type::Record(
            fields
                .into_iter()
                .map(|(name, field_type)| {
                    (name, field_type.map(|t| monomorphize_type(t.clone(), subs)))
                })
                .collect(),
            ext,
        ),
        Type::Apply(name, args, region) => Type::Apply(
            name,
            args.into_iter()
                .map(|arg| arg.map(|t| monomorphize_type(t.clone(), subs)))
                .collect(),
            region,
        ),
        Type::Function(args, ret, ext) => Type::Function(
            args.into_iter()
                .map(|arg| monomorphize_type(arg, subs))
                .collect(),
            Box::new(monomorphize_type(*ret, subs)),
            ext,
        ),
        Type::TagUnion(tags, ext) => Type::TagUnion(
            tags.into_iter()
                .map(|(name, tag_types)| {
                    (
                        name,
                        tag_types
                            .into_iter()
                            .map(|t| monomorphize_type(t, subs))
                            .collect(),
                    )
                })
                .collect(),
            ext,
        ),
        other => other,
    }
}
