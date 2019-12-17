use crate::can::expr::Rigids;
use crate::can::ident::Lowercase;
use crate::collections::{ImMap, SendMap};
use crate::parse::ast::{AssignedField, TypeAnnotation};
use crate::subs::VarStore;
use crate::types::Type;

pub fn canonicalize_annotation(
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
) -> (Rigids, crate::types::Type) {
    let mut ftv = ImMap::default();
    let result = can_annotation_help(annotation, var_store, &mut ftv);

    (ftv, result)
}

fn can_annotation_help(
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
    ftv: &mut Rigids,
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
            // TODO register rigid var here
            let var = var_store.fresh();
            ftv.insert((*v).into(), Type::Variable(var));
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
    ftv: &mut Rigids,
    field_types: &mut SendMap<Lowercase, Type>,
) {
    use crate::parse::ast::AssignedField::*;

    match field {
        LabeledValue(field_name, _, annotation) => {
            let field_type = can_annotation_help(&annotation.value, var_store, ftv);
            field_types.insert(Lowercase::from(field_name.value), field_type);
        }
        LabelOnly(_) => panic!("Illegal at the type level?"),
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_assigned_field(nested, var_store, ftv, field_types)
        }
        Malformed(_) => {}
    }
}
