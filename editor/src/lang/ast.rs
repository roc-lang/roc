use crate::lang::pattern::{Pattern2, PatternId};
use crate::lang::pool::Pool;
use crate::lang::pool::{NodeId, PoolStr, PoolVec, ShallowClone};
use crate::lang::types::{Type2, TypeId};
use arraystring::{typenum::U30, ArrayString};
use roc_can::expr::Recursive;
use roc_module::low_level::LowLevel;
use roc_module::operator::CalledVia;
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Problem {
    RanOutOfNodeIds,
}

pub type Res<T> = Result<T, Problem>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntStyle {
    Decimal,
    Octal,
    Hex,
    Binary,
}

impl IntStyle {
    pub fn from_base(base: roc_parse::ast::Base) -> Self {
        use roc_parse::ast::Base;
        match base {
            Base::Decimal => Self::Decimal,
            Base::Octal => Self::Octal,
            Base::Hex => Self::Hex,
            Base::Binary => Self::Binary,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntVal {
    I64(i64),
    U64(u64),
    I32(i32),
    U32(u32),
    I16(i16),
    U16(u16),
    I8(i8),
    U8(u8),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatVal {
    F64(f64),
    F32(f32),
}

#[derive(Debug)]
pub enum RecordField {
    InvalidLabelOnly(PoolStr, Variable),
    LabelOnly(PoolStr, Variable, Symbol),
    LabeledValue(PoolStr, Variable, NodeId<Expr2>),
}

#[test]
fn size_of_intval() {
    assert_eq!(std::mem::size_of::<IntVal>(), 16);
}

pub type ArrString = ArrayString<U30>;

/// An Expr that fits in 32B.
/// It has a 1B discriminant and variants which hold payloads of at most 31B.
#[derive(Debug)]
pub enum Expr2 {
    /// A negative number literal without a dot
    SmallInt {
        number: IntVal,  // 16B
        var: Variable,   // 4B
        style: IntStyle, // 1B
        text: PoolStr,   // 8B
    },
    /// A large (over 64-bit) negative number literal without a dot.
    /// This variant can't use IntVal because if IntVal stored 128-bit
    /// integers, it would be 32B on its own because of alignment.
    I128 {
        number: i128,    // 16B
        var: Variable,   // 4B
        style: IntStyle, // 1B
        text: PoolStr,   // 8B
    },
    /// A large (over 64-bit) nonnegative number literal without a dot
    /// This variant can't use IntVal because if IntVal stored 128-bit
    /// integers, it would be 32B on its own because of alignment.
    U128 {
        number: u128,    // 16B
        var: Variable,   // 4B
        style: IntStyle, // 1B
        text: PoolStr,   // 8B
    },
    /// A floating-point literal (with a dot)
    Float {
        number: FloatVal, // 16B
        var: Variable,    // 4B
        text: PoolStr,    // 8B
    },
    /// string literals of length up to 30B
    SmallStr(ArrString), // 31B
    /// string literals of length 31B or more
    Str(PoolStr), // 8B
    // Lookups
    Var(Symbol),            // 8B
    InvalidLookup(PoolStr), // 8B

    List {
        list_var: Variable,    // 4B - required for uniqueness of the list
        elem_var: Variable,    // 4B
        elems: PoolVec<Expr2>, // 8B
    },
    If {
        cond_var: Variable,                // 4B
        expr_var: Variable,                // 4B
        branches: PoolVec<(Expr2, Expr2)>, // 8B
        final_else: NodeId<Expr2>,         // 4B
    },
    When {
        cond_var: Variable,            // 4B
        expr_var: Variable,            // 4B
        branches: PoolVec<WhenBranch>, // 8B
        cond: NodeId<Expr2>,           // 4B
    },
    LetRec {
        defs: PoolVec<FunctionDef>, // 8B
        body_var: Variable,         // 8B
        body_id: NodeId<Expr2>,     // 4B
    },
    LetFunction {
        def: NodeId<FunctionDef>, // 4B
        body_var: Variable,       // 8B
        body_id: NodeId<Expr2>,   // 4B
    },
    LetValue {
        def_id: NodeId<ValueDef>, // 4B
        body_id: NodeId<Expr2>,   // 4B
        body_var: Variable,       // 4B
    },
    Call {
        args: PoolVec<(Variable, NodeId<Expr2>)>, // 8B
        expr: NodeId<Expr2>,                      // 4B
        expr_var: Variable,                       // 4B
        fn_var: Variable,                         // 4B
        closure_var: Variable,                    // 4B
        called_via: CalledVia,                    // 2B
    },
    RunLowLevel {
        op: LowLevel,                             // 1B
        args: PoolVec<(Variable, NodeId<Expr2>)>, // 8B
        ret_var: Variable,                        // 4B
    },
    Closure {
        args: PoolVec<(Variable, NodeId<Pattern2>)>, // 8B
        name: Symbol,                                // 8B
        body: NodeId<Expr2>,                         // 4B
        function_type: Variable,                     // 4B
        recursive: Recursive,                        // 1B
        extra: NodeId<ClosureExtra>,                 // 4B
    },
    // Product Types
    Record {
        record_var: Variable,         // 4B
        fields: PoolVec<RecordField>, // 8B
    },
    /// Empty record constant
    EmptyRecord,
    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access {
        field: PoolStr,       // 4B
        expr: NodeId<Expr2>,  // 4B
        record_var: Variable, // 4B
        ext_var: Variable,    // 4B
        field_var: Variable,  // 4B
    },

    /// field accessor as a function, e.g. (.foo) expr
    Accessor {
        function_var: Variable, // 4B
        closure_var: Variable,  // 4B
        field: PoolStr,         // 4B
        record_var: Variable,   // 4B
        ext_var: Variable,      // 4B
        field_var: Variable,    // 4B
    },
    Update {
        symbol: Symbol,                // 8B
        updates: PoolVec<RecordField>, // 8B
        record_var: Variable,          // 4B
        ext_var: Variable,             // 4B
    },

    // Sum Types
    GlobalTag {
        name: PoolStr,                                 // 4B
        variant_var: Variable,                         // 4B
        ext_var: Variable,                             // 4B
        arguments: PoolVec<(Variable, NodeId<Expr2>)>, // 8B
    },
    PrivateTag {
        name: Symbol,                                  // 8B
        variant_var: Variable,                         // 4B
        ext_var: Variable,                             // 4B
        arguments: PoolVec<(Variable, NodeId<Expr2>)>, // 8B
    },
    Blank, // Rendered as empty box in editor

    // Compiles, but will crash if reached
    RuntimeError(/* TODO make a version of RuntimeError that fits in 15B */),
}

#[derive(Debug)]
pub struct ValueDef {
    pub pattern: PatternId,                  // 4B
    pub expr_type: Option<(TypeId, Rigids)>, // ?
    pub expr_var: Variable,                  // 4B
}

impl ShallowClone for ValueDef {
    fn shallow_clone(&self) -> Self {
        Self {
            pattern: self.pattern,
            expr_type: match &self.expr_type {
                Some((id, rigids)) => Some((*id, rigids.shallow_clone())),
                None => None,
            },
            expr_var: self.expr_var,
        }
    }
}

#[derive(Debug)]
pub enum FunctionDef {
    WithAnnotation {
        name: Symbol,                          // 8B
        arguments: PoolVec<(Pattern2, Type2)>, // 8B
        rigids: NodeId<Rigids>,                // 4B
        return_type: TypeId,                   // 4B
        body: ExprId,                          // 4B
    },
    NoAnnotation {
        name: Symbol,                             // 8B
        arguments: PoolVec<(Pattern2, Variable)>, // 8B
        return_var: Variable,                     // 4B
        body: ExprId,                             // 4B
    },
}

impl ShallowClone for FunctionDef {
    fn shallow_clone(&self) -> Self {
        match self {
            Self::WithAnnotation {
                name,
                arguments,
                rigids,
                return_type,
                body,
            } => Self::WithAnnotation {
                name: *name,
                arguments: arguments.shallow_clone(),
                rigids: *rigids,
                return_type: *return_type,
                body: *body,
            },

            Self::NoAnnotation {
                name,
                arguments,
                return_var,
                body,
            } => Self::NoAnnotation {
                name: *name,
                arguments: arguments.shallow_clone(),
                return_var: *return_var,
                body: *body,
            },
        }
    }
}

#[derive(Debug)]
pub struct Rigids {
    pub named: PoolVec<(PoolStr, Variable)>, // 8B
    pub unnamed: PoolVec<Variable>,          // 8B
}

/// This is overflow data from a Closure variant, which needs to store
/// more than 32B of total data
#[derive(Debug)]
pub struct ClosureExtra {
    pub return_type: Variable,                         // 4B
    pub captured_symbols: PoolVec<(Symbol, Variable)>, // 8B
    pub closure_type: Variable,                        // 4B
    pub closure_ext_var: Variable,                     // 4B
}

#[derive(Debug)]
pub struct WhenBranch {
    pub patterns: PoolVec<Pattern2>,  // 4B
    pub body: NodeId<Expr2>,          // 3B
    pub guard: Option<NodeId<Expr2>>, // 4B
}

pub type ExprId = NodeId<Expr2>;

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

    pub fn get_record_field_val_node_id(&self) -> Option<NodeId<Expr2>> {
        match self {
            InvalidLabelOnly(_, _) => None,
            LabelOnly(_, _, _) => None,
            LabeledValue(_, _, field_val_id) => Some(*field_val_id),
        }
    }
}

pub fn expr2_to_string(node_id: NodeId<Expr2>, pool: &Pool) -> String {
    let mut full_string = String::new();

    expr2_to_string_helper(node_id, 0, pool, &mut full_string);

    full_string
}

fn get_spacing(indent_level: usize) -> String {
    std::iter::repeat("    ")
        .take(indent_level)
        .collect::<Vec<&str>>()
        .join("")
}

fn expr2_to_string_helper(
    node_id: NodeId<Expr2>,
    indent_level: usize,
    pool: &Pool,
    out_string: &mut String,
) {
    let expr2 = pool.get(node_id);

    out_string.push_str(&get_spacing(indent_level));

    match expr2 {
        Expr2::SmallStr(arr_string) => out_string.push_str(&format!(
            "{}{}{}",
            "SmallStr(\"",
            arr_string.as_str(),
            "\")",
        )),
        Expr2::Str(pool_str) => {
            out_string.push_str(&format!("{}{}{}", "Str(\"", pool_str.as_str(pool), "\")",))
        }
        Expr2::Blank => out_string.push_str("Blank"),
        Expr2::EmptyRecord => out_string.push_str("EmptyRecord"),
        Expr2::Record { record_var, fields } => {
            out_string.push_str("Record:\n");
            out_string.push_str(&format!(
                "{}Var({:?})\n",
                get_spacing(indent_level + 1),
                record_var
            ));

            out_string.push_str(&format!("{}fields: [\n", get_spacing(indent_level + 1)));

            let mut first_child = true;
            
            for field in fields.iter(pool) {
                if !first_child {
                    out_string.push_str(", ")
                } else {
                    first_child = false;
                }

                match field {
                    RecordField::InvalidLabelOnly(pool_str, var) => {
                        out_string.push_str(&format!(
                            "{}({}, Var({:?})",
                            get_spacing(indent_level + 2),
                            pool_str.as_str(pool),
                            var,
                        ));
                    }
                    RecordField::LabelOnly(pool_str, var, symbol) => {
                        out_string.push_str(&format!(
                            "{}({}, Var({:?}), Symbol({:?})",
                            get_spacing(indent_level + 2),
                            pool_str.as_str(pool),
                            var,
                            symbol
                        ));
                    }
                    RecordField::LabeledValue(pool_str, var, val_node_id) => {
                        out_string.push_str(&format!(
                            "{}({}, Var({:?}), Expr2(\n",
                            get_spacing(indent_level + 2),
                            pool_str.as_str(pool),
                            var,
                        ));

                        expr2_to_string_helper(*val_node_id, indent_level + 3, pool, out_string);
                        out_string.push_str(&format!("{})\n", get_spacing(indent_level + 2)));
                    }
                }
            }

            out_string.push_str(&format!("{}]\n", get_spacing(indent_level + 1)));
        }
        Expr2::InvalidLookup(pool_str) => {
            out_string.push_str(&format!("InvalidLookup({})", pool_str.as_str(pool)));
        }
        other => todo!("Implement for {:?}", other),
    }

    out_string.push('\n');
}

#[test]
fn size_of_expr() {
    assert_eq!(std::mem::size_of::<Expr2>(), crate::lang::pool::NODE_BYTES);
}

impl ShallowClone for Rigids {
    fn shallow_clone(&self) -> Self {
        Self {
            named: self.named.shallow_clone(),
            unnamed: self.unnamed.shallow_clone(),
        }
    }
}
