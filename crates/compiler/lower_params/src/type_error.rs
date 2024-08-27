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

    let is_extended = |symbol: &Symbol| {
        imported_modules_with_params.contains(&symbol.module_id())
            || home_top_level_symbols.contains(symbol)
    };

    for error in errors {
        match error {
            TypeError::BadExpr(
                _,
                _,
                found,
                Expected::ForReason(
                    Reason::FnCall {
                        name: Some(name),
                        arity,
                        called_via: _,
                    },
                    err_type,
                    _,
                ),
            ) if is_extended(name) => {
                *arity -= 1;
                drop_last_argument(found);
                drop_last_argument(err_type);
            }

            TypeError::BadExpr(_, _, _, _) => todo!(),

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
