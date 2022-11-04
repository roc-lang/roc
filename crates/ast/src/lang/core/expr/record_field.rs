use roc_types::subs::Variable;

use crate::mem_pool::pool_str::PoolStr;
use roc_module::symbol::Symbol;

use super::expr2::ExprId;

#[derive(Debug)]
pub enum RecordField {
    InvalidLabelOnly(PoolStr, Variable),
    LabelOnly(PoolStr, Variable, Symbol),
    LabeledValue(PoolStr, Variable, ExprId),
}

use RecordField::*;

impl RecordField {
    pub fn get_record_field_var(&self) -> &Variable {
        match self {
            InvalidLabelOnly(_, var) => var,
            LabelOnly(_, var, _) => var,
            LabeledValue(_, var, _) => var,
        }
    }

    pub fn get_record_field_pool_str(&self) -> &PoolStr {
        match self {
            InvalidLabelOnly(pool_str, _) => pool_str,
            LabelOnly(pool_str, _, _) => pool_str,
            LabeledValue(pool_str, _, _) => pool_str,
        }
    }

    pub fn get_record_field_pool_str_mut(&mut self) -> &mut PoolStr {
        match self {
            InvalidLabelOnly(pool_str, _) => pool_str,
            LabelOnly(pool_str, _, _) => pool_str,
            LabeledValue(pool_str, _, _) => pool_str,
        }
    }

    pub fn get_record_field_val_node_id(&self) -> Option<ExprId> {
        match self {
            InvalidLabelOnly(_, _) => None,
            LabelOnly(_, _, _) => None,
            LabeledValue(_, _, field_val_id) => Some(*field_val_id),
        }
    }
}
