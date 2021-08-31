#![allow(clippy::manual_map)]

use std::collections::{HashMap, HashSet};
use std::hash::BuildHasherDefault;

use crate::editor::ed_error::{EdResult, UnexpectedASTNode};
use crate::lang::pattern::{Pattern2, PatternId};
use crate::lang::pool::Pool;
use crate::lang::pool::{NodeId, PoolStr, PoolVec, ShallowClone};
use crate::lang::types::{Type2, TypeId};
use arraystring::{typenum::U30, ArrayString};
use roc_can::expr::Recursive;
use roc_collections::all::WyHash;
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
    LabeledValue(PoolStr, Variable, ExprId),
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
    // TODO(rvcas): rename this eventually
    /// A large (over 64-bit) negative number literal without a dot.
    /// This variant can't use IntVal because if IntVal stored 128-bit
    /// integers, it would be 32B on its own because of alignment.
    I128 {
        number: i128,    // 16B
        var: Variable,   // 4B
        style: IntStyle, // 1B
        text: PoolStr,   // 8B
    },
    // TODO(rvcas): rename this eventually
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
        elem_var: Variable,     // 4B
        elems: PoolVec<ExprId>, // 8B
    },
    If {
        cond_var: Variable,                  // 4B
        expr_var: Variable,                  // 4B
        branches: PoolVec<(ExprId, ExprId)>, // 8B
        final_else: ExprId,                  // 4B
    },
    When {
        cond_var: Variable,            // 4B
        expr_var: Variable,            // 4B
        branches: PoolVec<WhenBranch>, // 8B
        cond: ExprId,                  // 4B
    },
    LetRec {
        defs: PoolVec<FunctionDef>, // 8B
        body_var: Variable,         // 8B
        body_id: ExprId,            // 4B
    },
    LetFunction {
        def_id: NodeId<FunctionDef>, // 4B
        body_var: Variable,          // 8B
        body_id: ExprId,             // 4B
    },
    LetValue {
        def_id: NodeId<ValueDef>, // 4B
        body_id: ExprId,          // 4B
        body_var: Variable,       // 4B
    },
    Call {
        args: PoolVec<(Variable, ExprId)>, // 8B
        expr: ExprId,                      // 4B
        expr_var: Variable,                // 4B
        fn_var: Variable,                  // 4B
        closure_var: Variable,             // 4B
        called_via: CalledVia,             // 2B
    },
    RunLowLevel {
        op: LowLevel,                      // 1B
        args: PoolVec<(Variable, ExprId)>, // 8B
        ret_var: Variable,                 // 4B
    },
    Closure {
        args: PoolVec<(Variable, NodeId<Pattern2>)>, // 8B
        name: Symbol,                                // 8B
        body: ExprId,                                // 4B
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
        expr: ExprId,         // 4B
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
        name: PoolStr,                          // 4B
        variant_var: Variable,                  // 4B
        ext_var: Variable,                      // 4B
        arguments: PoolVec<(Variable, ExprId)>, // 8B
    },
    PrivateTag {
        name: Symbol,                           // 8B
        variant_var: Variable,                  // 4B
        ext_var: Variable,                      // 4B
        arguments: PoolVec<(Variable, ExprId)>, // 8B
    },
    Blank, // Rendered as empty box in editor

    // Compiles, but will crash if reached
    RuntimeError(/* TODO make a version of RuntimeError that fits in 15B */),
}

// A top level definition, not inside a function. For example: `main = "Hello, world!"`
#[derive(Debug)]
pub enum Def2 {
    // ValueDef example: `main = "Hello, world!"`. identifier -> `main`, expr -> "Hello, world!"
    ValueDef {
        identifier_id: NodeId<Pattern2>,
        expr_id: NodeId<Expr2>,
    },
    Blank,
}

#[derive(Debug)]
pub enum ValueDef {
    WithAnnotation {
        pattern_id: PatternId, // 4B
        expr_id: ExprId,       // 4B
        type_id: TypeId,
        rigids: Rigids,
        expr_var: Variable, // 4B
    },
    NoAnnotation {
        pattern_id: PatternId, // 4B
        expr_id: ExprId,       // 4B
        expr_var: Variable,    // 4B
    },
}

impl ShallowClone for ValueDef {
    fn shallow_clone(&self) -> Self {
        match self {
            Self::WithAnnotation {
                pattern_id,
                expr_id,
                type_id,
                rigids,
                expr_var,
            } => Self::WithAnnotation {
                pattern_id: *pattern_id,
                expr_id: *expr_id,
                type_id: *type_id,
                rigids: rigids.shallow_clone(),
                expr_var: *expr_var,
            },
            Self::NoAnnotation {
                pattern_id,
                expr_id,
                expr_var,
            } => Self::NoAnnotation {
                pattern_id: *pattern_id,
                expr_id: *expr_id,
                expr_var: *expr_var,
            },
        }
    }
}

impl ValueDef {
    pub fn get_expr_id(&self) -> ExprId {
        match self {
            ValueDef::WithAnnotation { expr_id, .. } => *expr_id,
            ValueDef::NoAnnotation { expr_id, .. } => *expr_id,
        }
    }

    pub fn get_pattern_id(&self) -> NodeId<Pattern2> {
        match self {
            ValueDef::WithAnnotation { pattern_id, .. } => *pattern_id,
            ValueDef::NoAnnotation { pattern_id, .. } => *pattern_id,
        }
    }
}

pub fn value_def_to_string(val_def: &ValueDef, pool: &Pool) -> String {
    match val_def {
        ValueDef::WithAnnotation {
            pattern_id,
            expr_id,
            type_id,
            rigids,
            expr_var,
        } => {
            format!("WithAnnotation {{ pattern_id: {:?}, expr_id: {:?}, type_id: {:?}, rigids: {:?}, expr_var: {:?}}}", pool.get(*pattern_id), expr2_to_string(*expr_id, pool), pool.get(*type_id), rigids, expr_var)
        }
        ValueDef::NoAnnotation {
            pattern_id,
            expr_id,
            expr_var,
        } => {
            format!(
                "NoAnnotation {{ pattern_id: {:?}, expr_id: {:?}, expr_var: {:?}}}",
                pool.get(*pattern_id),
                expr2_to_string(*expr_id, pool),
                expr_var
            )
        }
    }
}

#[derive(Debug)]
pub enum FunctionDef {
    WithAnnotation {
        name: Symbol,                           // 8B
        arguments: PoolVec<(PatternId, Type2)>, // 8B
        rigids: NodeId<Rigids>,                 // 4B
        return_type: TypeId,                    // 4B
        body: ExprId,                           // 4B
    },
    NoAnnotation {
        name: Symbol,                              // 8B
        arguments: PoolVec<(PatternId, Variable)>, // 8B
        return_var: Variable,                      // 4B
        body: ExprId,                              // 4B
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
    pub names: PoolVec<(Option<PoolStr>, Variable)>, // 8B
    padding: [u8; 1],
}

#[allow(clippy::needless_collect)]
impl Rigids {
    pub fn new(
        named: HashMap<&str, Variable, BuildHasherDefault<WyHash>>,
        unnamed: HashSet<Variable, BuildHasherDefault<WyHash>>,
        pool: &mut Pool,
    ) -> Self {
        let names = PoolVec::with_capacity((named.len() + unnamed.len()) as u32, pool);

        let mut temp_names = Vec::new();

        temp_names.extend(named.iter().map(|(name, var)| (Some(*name), *var)));

        temp_names.extend(unnamed.iter().map(|var| (None, *var)));

        for (node_id, (opt_name, variable)) in names.iter_node_ids().zip(temp_names) {
            let poolstr = opt_name.map(|name| PoolStr::new(name, pool));

            pool[node_id] = (poolstr, variable);
        }

        Self {
            names,
            padding: Default::default(),
        }
    }

    pub fn named(&self, pool: &mut Pool) -> PoolVec<(PoolStr, Variable)> {
        let named = self
            .names
            .iter(pool)
            .filter_map(|(opt_pool_str, var)| {
                if let Some(pool_str) = opt_pool_str {
                    Some((*pool_str, *var))
                } else {
                    None
                }
            })
            .collect::<Vec<(PoolStr, Variable)>>();

        PoolVec::new(named.into_iter(), pool)
    }

    pub fn unnamed(&self, pool: &mut Pool) -> PoolVec<Variable> {
        let unnamed = self
            .names
            .iter(pool)
            .filter_map(|(opt_pool_str, var)| {
                if opt_pool_str.is_none() {
                    Some(*var)
                } else {
                    None
                }
            })
            .collect::<Vec<Variable>>();

        PoolVec::new(unnamed.into_iter(), pool)
    }
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
    pub patterns: PoolVec<Pattern2>, // 4B
    pub body: ExprId,                // 3B
    pub guard: Option<ExprId>,       // 4B
}

// TODO make the inner types private?
pub type ExprId = NodeId<Expr2>;

pub type DefId = NodeId<Def2>;

use RecordField::*;

use super::parse::ASTNodeId;
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

pub fn ast_node_to_string(node_id: ASTNodeId, pool: &Pool) -> String {
    match node_id {
        ASTNodeId::ADefId(def_id) => def2_to_string(def_id, pool),
        ASTNodeId::AExprId(expr_id) => expr2_to_string(expr_id, pool),
    }
}

pub fn expr2_to_string(node_id: ExprId, pool: &Pool) -> String {
    let mut full_string = String::new();
    let expr2 = pool.get(node_id);

    expr2_to_string_helper(expr2, 0, pool, &mut full_string);

    full_string
}

fn get_spacing(indent_level: usize) -> String {
    std::iter::repeat("    ")
        .take(indent_level)
        .collect::<Vec<&str>>()
        .join("")
}

fn expr2_to_string_helper(
    expr2: &Expr2,
    indent_level: usize,
    pool: &Pool,
    out_string: &mut String,
) {
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
            out_string.push_str(&var_to_string(record_var, indent_level + 1));

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

                        let val_expr2 = pool.get(*val_node_id);
                        expr2_to_string_helper(val_expr2, indent_level + 3, pool, out_string);
                        out_string.push_str(&format!("{})\n", get_spacing(indent_level + 2)));
                    }
                }
            }

            out_string.push_str(&format!("{}]\n", get_spacing(indent_level + 1)));
        }
        Expr2::List { elem_var, elems } => {
            out_string.push_str("List:\n");
            out_string.push_str(&var_to_string(elem_var, indent_level + 1));
            out_string.push_str(&format!("{}elems: [\n", get_spacing(indent_level + 1)));

            let mut first_elt = true;

            for elem_expr2_id in elems.iter(pool) {
                if !first_elt {
                    out_string.push_str(", ")
                } else {
                    first_elt = false;
                }

                let elem_expr2 = pool.get(*elem_expr2_id);

                expr2_to_string_helper(elem_expr2, indent_level + 2, pool, out_string)
            }

            out_string.push_str(&format!("{}]\n", get_spacing(indent_level + 1)));
        }
        Expr2::InvalidLookup(pool_str) => {
            out_string.push_str(&format!("InvalidLookup({})", pool_str.as_str(pool)));
        }
        Expr2::SmallInt { text, .. } => {
            out_string.push_str(&format!("SmallInt({})", text.as_str(pool)));
        }
        Expr2::LetValue {
            def_id, body_id, ..
        } => {
            out_string.push_str(&format!(
                "LetValue(def_id: >>{:?}), body_id: >>{:?})",
                value_def_to_string(pool.get(*def_id), pool),
                pool.get(*body_id)
            ));
        }
        other => todo!("Implement for {:?}", other),
    }

    out_string.push('\n');
}

pub fn def2_to_string(node_id: DefId, pool: &Pool) -> String {
    let mut full_string = String::new();
    let def2 = pool.get(node_id);

    match def2 {
        Def2::ValueDef {
            identifier_id,
            expr_id,
        } => {
            full_string.push_str(&format!(
                "Def2::ValueDef(identifier_id: >>{:?}), expr_id: >>{:?})",
                pool.get(*identifier_id),
                expr2_to_string(*expr_id, pool)
            ));
        }
        Def2::Blank => {
            full_string.push_str("Def2::Blank");
        }
    }

    full_string
}

fn var_to_string(some_var: &Variable, indent_level: usize) -> String {
    format!("{}Var({:?})\n", get_spacing(indent_level + 1), some_var)
}

// get string from SmallStr or Str
pub fn get_string_from_expr2(node_id: ExprId, pool: &Pool) -> EdResult<String> {
    match pool.get(node_id) {
        Expr2::SmallStr(arr_string) => Ok(arr_string.as_str().to_string()),
        Expr2::Str(pool_str) => Ok(pool_str.as_str(pool).to_owned()),
        other => UnexpectedASTNode {
            required_node_type: "SmallStr or Str",
            encountered_node_type: format!("{:?}", other),
        }
        .fail()?,
    }
}

pub fn update_str_expr(
    node_id: ExprId,
    new_char: char,
    insert_index: usize,
    pool: &mut Pool,
) -> EdResult<()> {
    let str_expr = pool.get_mut(node_id);

    enum Either {
        MyString(String),
        MyPoolStr(PoolStr),
        Done,
    }

    let insert_either = match str_expr {
        Expr2::SmallStr(arr_string) => {
            let insert_res = arr_string.try_insert(insert_index as u8, new_char);

            match insert_res {
                Ok(_) => Either::Done,
                _ => {
                    let mut new_string = arr_string.as_str().to_string();
                    new_string.insert(insert_index, new_char);

                    Either::MyString(new_string)
                }
            }
        }
        Expr2::Str(old_pool_str) => Either::MyPoolStr(*old_pool_str),
        other => UnexpectedASTNode {
            required_node_type: "SmallStr or Str",
            encountered_node_type: format!("{:?}", other),
        }
        .fail()?,
    };

    match insert_either {
        Either::MyString(new_string) => {
            let new_pool_str = PoolStr::new(&new_string, pool);

            pool.set(node_id, Expr2::Str(new_pool_str))
        }
        Either::MyPoolStr(old_pool_str) => {
            let mut new_string = old_pool_str.as_str(pool).to_owned();

            new_string.insert(insert_index, new_char);

            let new_pool_str = PoolStr::new(&new_string, pool);

            pool.set(node_id, Expr2::Str(new_pool_str))
        }
        Either::Done => (),
    }

    Ok(())
}

#[test]
fn size_of_expr() {
    assert_eq!(std::mem::size_of::<Expr2>(), crate::lang::pool::NODE_BYTES);
}

impl ShallowClone for Rigids {
    fn shallow_clone(&self) -> Self {
        Self {
            names: self.names.shallow_clone(),
            padding: self.padding,
        }
    }
}
