//! Generates html documentation from Roc files. Used for things like
//! [roc-lang.org/builtins/Num](https://www.roc-lang.org/builtins/Num).

mod render_doc_url;
mod render_markdown;
mod render_package;
mod render_type;

pub use render_package::{
    AbilityImpl, BodyEntry, Docs, RecordField, SidebarEntry, TypeAnn, TypeAnnVisitor,
};
pub use render_type::TypeRenderer;
