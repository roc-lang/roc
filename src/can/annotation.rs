use crate::can::ident::Lowercase;
use crate::collections::{ImMap, SendMap};
use crate::parse::ast::{AssignedField, TypeAnnotation};
use crate::subs::{VarStore, Variable};
use crate::types::RecordFieldLabel;
use crate::types::Type;

pub fn canonicalize_annotation(
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
) -> (SendMap<Variable, Lowercase>, crate::types::Type) {
    // NOTE on rigids
    //
    // Rigids must be unique within a type annoation.
    // E.g. in `identity : a -> a`, there should only be one
    // variable (a rigid one, with name "a").
    // Hence `rigids : ImMap<Lowercase, Variable>`
    //
    // But then between annotations, the same name can occur multiple times,
    // but a variable can only have one name. Therefore
    // `ftv : SendMap<Variable, Lowercase>`.
    let mut rigids = ImMap::default();
    let result = can_annotation_help(annotation, var_store, &mut rigids);

    let mut ftv = SendMap::default();

    for (k, v) in rigids {
        ftv.insert(v, k);
    }

    (ftv, result)
}

fn can_annotation_help(
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
) -> crate::types::Type {
    use crate::parse::ast::TypeAnnotation::*;

    match annotation {
        Function(argument_types, return_type) => {
            let mut args = Vec::new();

            for arg in *argument_types {
                args.push(can_annotation_help(&arg.value, var_store, rigids));
            }

            let ret = can_annotation_help(&return_type.value, var_store, rigids);
            Type::Function(args, Box::new(ret))
        }
        Apply(module_name, name, type_arguments) => {
            let mut args = Vec::new();

            for arg in *type_arguments {
                args.push(can_annotation_help(&arg.value, var_store, rigids));
            }

            Type::Apply {
                module_name: module_name.join(".").into(),
                name: (*name).into(),
                args,
            }
        }
        BoundVariable(v) => {
            let name = Lowercase::from(*v);

            if let Some(var) = rigids.get(&name) {
                Type::Variable(*var)
            } else {
                let var = var_store.fresh();
                rigids.insert(name, var);
                Type::Variable(var)
            }
        }
        Record(fields) => {
            let mut field_types = SendMap::default();
            for field in fields {
                can_assigned_field(&field.value, var_store, rigids, &mut field_types);
            }

            // fragment variable is free in this case
            let fragment_var = var_store.fresh();
            let fragment_type = Type::Variable(fragment_var);

            Type::Record(field_types, Box::new(fragment_type))
        }
        RecordFragment(fields, fragment) => {
            let mut field_types = SendMap::default();
            for field in fields {
                can_assigned_field(&field.value, var_store, rigids, &mut field_types);
            }

            let fragment_type = can_annotation_help(&fragment.value, var_store, rigids);

            Type::Record(field_types, Box::new(fragment_type))
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_annotation_help(nested, var_store, rigids)
        }
        Wildcard | Malformed(_) => {
            let var = var_store.fresh();
            Type::Variable(var)
        }
    }
}

fn can_assigned_field<'a>(
    field: &AssignedField<'a, TypeAnnotation<'a>>,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
    field_types: &mut SendMap<RecordFieldLabel, Type>,
) {
    use crate::parse::ast::AssignedField::*;

    match field {
        LabeledValue(field_name, _, annotation) => {
            let field_type = can_annotation_help(&annotation.value, var_store, rigids);
            let label = RecordFieldLabel::Required(Lowercase::from(field_name.value));
            field_types.insert(label, field_type);
        }
        OptionalField(field_name, _, annotation) => {
            let field_type = can_annotation_help(&annotation.value, var_store, rigids);
            let label = RecordFieldLabel::Optional(Lowercase::from(field_name.value));
            field_types.insert(label, field_type);
        }
        LabelOnly(loc_field_name) => {
            // Interpret { a, b } as { a : a, b : b }
            let field_name = Lowercase::from(loc_field_name.value);
            let field_type = {
                if let Some(var) = rigids.get(&field_name) {
                    Type::Variable(*var)
                } else {
                    let field_var = var_store.fresh();
                    rigids.insert(field_name.clone(), field_var);
                    Type::Variable(field_var)
                }
            };
            let label = RecordFieldLabel::Required(field_name);
            field_types.insert(label, field_type);
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_assigned_field(nested, var_store, rigids, field_types)
        }
        Malformed(_) => {}
    }
}
