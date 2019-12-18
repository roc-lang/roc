use crate::can::ident::Lowercase;
use crate::collections::SendMap;
use crate::parse::ast::{AssignedField, TypeAnnotation};
use crate::subs::{VarStore, Variable};
use crate::types::Type;

pub fn canonicalize_annotation(
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
) -> (SendMap<Lowercase, Variable>, crate::types::Type) {
    let mut ftv = SendMap::default();
    let result = can_annotation_help(annotation, var_store, &mut ftv);

    (ftv, result)
}

fn can_annotation_help(
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
    ftv: &mut SendMap<Lowercase, Variable>,
) -> (crate::types::Type) {
    use crate::parse::ast::TypeAnnotation::*;

    match annotation {
        Function(argument_types, return_type) => {
            let mut args = Vec::new();

            for arg in *argument_types {
                args.push(can_annotation_help(&arg.value, var_store, ftv));
            }

            let ret = can_annotation_help(&return_type.value, var_store, ftv);
            Type::Function(args, Box::new(ret))
        }
        Apply(module_name, name, type_arguments) => {
            let mut args = Vec::new();

            for arg in *type_arguments {
                args.push(can_annotation_help(&arg.value, var_store, ftv));
            }

            Type::Apply {
                module_name: module_name.join(".").into(),
                name: (*name).into(),
                args,
            }
        }
        BoundVariable(v) => {
            let var = var_store.fresh();
            let name = Lowercase::from(*v);
            ftv.insert(name, var);
            Type::Variable(var)
        }
        Record(fields) => {
            let mut field_types = SendMap::default();
            for field in fields {
                can_assigned_field(&field.value, var_store, ftv, &mut field_types);
            }

            // fragment variable is free in this case
            let fragment_var = var_store.fresh();
            let fragment_type = Type::Variable(fragment_var);

            Type::Record(field_types, Box::new(fragment_type))
        }
        RecordFragment(fields, fragment) => {
            let mut field_types = SendMap::default();
            for field in fields {
                can_assigned_field(&field.value, var_store, ftv, &mut field_types);
            }

            let fragment_type = can_annotation_help(&fragment.value, var_store, ftv);

            Type::Record(field_types, Box::new(fragment_type))
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_annotation_help(nested, var_store, ftv)
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
    ftv: &mut SendMap<Lowercase, Variable>,
    field_types: &mut SendMap<Lowercase, Type>,
) {
    use crate::parse::ast::AssignedField::*;

    match field {
        LabeledValue(field_name, _, annotation) => {
            let field_type = can_annotation_help(&annotation.value, var_store, ftv);
            field_types.insert(Lowercase::from(field_name.value), field_type);
        }
        LabelOnly(field_name) => {
            // Interpret { a, b } as { a : a, b : b }
            let field_var = var_store.fresh();
            let field_type = Type::Variable(field_var);

            ftv.insert(Lowercase::from(field_name.value), field_var);
            field_types.insert(Lowercase::from(field_name.value), field_type);
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_assigned_field(nested, var_store, ftv, field_types)
        }
        Malformed(_) => {}
    }
}
