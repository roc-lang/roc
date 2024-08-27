use roc_can::expected::Expected;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::Loc;
use roc_solve_problem::TypeError;
use roc_types::types::{ErrorType, Reason};

/// Removes the arguments added as a result of lowering params
pub fn remove_module_param_arguments(
    errors: &mut Vec<TypeError>,
    home_has_params: bool,
    home_top_level_symbols: &[Loc<Symbol>],
    imported_modules_with_params: Vec<ModuleId>,
) {
    if errors.is_empty() || (!home_has_params && imported_modules_with_params.is_empty()) {
        return;
    }

    let home_top_level_symbols = if home_has_params {
        home_top_level_symbols
            .iter()
            .map(|loc| loc.value)
            .collect::<Vec<_>>()
    } else {
        vec![]
    };

    let env = Env {
        home_top_level_symbols,
        imported_modules_with_params,
    };

    for error in errors {
        match error {
            TypeError::BadExpr(_, _, found, Expected::ForReason(reason, expected, _)) => {
                remove_with_bad_expr_reason(&env, found, reason, expected);
            }
            TypeError::BadExpr(_, _, _, _) => todo!("{:?}", error),

            // Irrelevant
            TypeError::BadPattern(_, _, _, _)
            | TypeError::CircularType(_, _, _)
            | TypeError::CircularDef(_)
            | TypeError::UnexposedLookup(_, _)
            | TypeError::UnfulfilledAbility(_)
            | TypeError::BadExprMissingAbility(_, _, _, _)
            | TypeError::BadPatternMissingAbility(_, _, _, _)
            | TypeError::Exhaustive(_)
            | TypeError::StructuralSpecialization {
                region: _,
                typ: _,
                ability: _,
                member: _,
            }
            | TypeError::WrongSpecialization {
                region: _,
                ability_member: _,
                expected_opaque: _,
                found_opaque: _,
            }
            | TypeError::IngestedFileBadUtf8(_, _)
            | TypeError::IngestedFileUnsupportedType(_, _)
            | TypeError::UnexpectedModuleParams(_, _)
            | TypeError::MissingModuleParams(_, _, _)
            | TypeError::ModuleParamsMismatch(_, _, _, _) => {}
        }
    }
}

struct Env {
    home_top_level_symbols: Vec<Symbol>,
    imported_modules_with_params: Vec<ModuleId>,
}

impl Env {
    fn is_extended(&self, symbol: &Symbol) -> bool {
        self.home_top_level_symbols.contains(symbol)
            || self
                .imported_modules_with_params
                .contains(&symbol.module_id())
    }
}

fn remove_with_bad_expr_reason(
    env: &Env,
    found_type: &mut ErrorType,
    reason: &mut Reason,
    expected_type: &mut ErrorType,
) {
    match reason {
        Reason::FnCall {
            name: Some(name),
            arity,
            called_via: _,
        } if env.is_extended(name) => {
            *arity -= 1;
            debug_assert_ne!(0, *arity);

            drop_last_argument(found_type);
            drop_last_argument(expected_type);
        }

        Reason::FnCall { .. } => {}

        Reason::FnArg {
            name: Some(name),
            arg_index: _,
            called_via: _,
        } if env.is_extended(name) => {
            todo!()
        }

        Reason::FnArg { .. } => {}

        // Irrelevant
        Reason::TypedArg {
            name: _,
            arg_index: _,
        }
        | Reason::LowLevelOpArg {
            op: _,
            arg_index: _,
        }
        | Reason::ForeignCallArg {
            foreign_symbol: _,
            arg_index: _,
        }
        | Reason::FloatLiteral
        | Reason::IntLiteral
        | Reason::NumLiteral
        | Reason::StrInterpolation
        | Reason::WhenBranches
        | Reason::WhenBranch { index: _ }
        | Reason::WhenGuard
        | Reason::ExpectCondition
        | Reason::IfCondition
        | Reason::IfBranch {
            index: _,
            total_branches: _,
        }
        | Reason::ElemInList { index: _ }
        | Reason::RecordUpdateValue(_)
        | Reason::RecordUpdateKeys(_, _)
        | Reason::RecordDefaultField(_)
        | Reason::NumericLiteralSuffix
        | Reason::InvalidAbilityMemberSpecialization {
            member_name: _,
            def_region: _,
            unimplemented_abilities: _,
        }
        | Reason::GeneralizedAbilityMemberSpecialization {
            member_name: _,
            def_region: _,
        }
        | Reason::CrashArg
        | Reason::ImportParams(_) => {}
    }
}

fn drop_last_argument(err_type: &mut ErrorType) {
    match err_type {
        ErrorType::Function(arguments, _, _) => {
            arguments.pop();
        }
        // Irrelevant
        ErrorType::Infinite
        | ErrorType::Type(_, _)
        | ErrorType::FlexVar(_)
        | ErrorType::RigidVar(_)
        | ErrorType::FlexAbleVar(_, _)
        | ErrorType::RigidAbleVar(_, _)
        | ErrorType::Record(_, _)
        | ErrorType::Tuple(_, _)
        | ErrorType::TagUnion(_, _, _)
        | ErrorType::RecursiveTagUnion(_, _, _, _)
        | ErrorType::Alias(_, _, _, _)
        | ErrorType::Range(_)
        | ErrorType::Error => {}
    }
}
