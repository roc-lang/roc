// TODO [mono2]: re-enable when ready
#![allow(dead_code)]
#![allow(clippy::too_many_arguments)]

mod debug_info;
mod foreign_symbol;
mod mono_expr;
mod mono_ir;
mod mono_module;
mod mono_num;
mod mono_pattern;
mod mono_struct;
mod mono_type;
// mod specialize_expr;
mod specialize_type;

pub use debug_info::DebugInfo;
pub use foreign_symbol::{ForeignSymbolId, ForeignSymbols};
pub use mono_expr::Env;
pub use mono_ir::{MonoExpr, MonoExprId, MonoExprs, WhenBranches};
pub use mono_module::{InternedStrId, Interns};
pub use mono_num::Number;
pub use mono_pattern::{MonoPattern, MonoPatternId, MonoPatterns};
pub use mono_struct::MonoFieldId;
pub use mono_type::{MonoType, MonoTypeId, MonoTypes, Primitive};
pub use specialize_type::{MonoTypeCache, Problem, RecordFieldIds, TupleElemIds};
