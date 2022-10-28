use crate::lang::core::{expr::expr2::ExprId, header::AppHeader};

// TODO don't use mock struct and actually parse string
pub fn parse_from_string(_header_str: &str, ast_node_id: ExprId) -> AppHeader {
    AppHeader {
        app_name: "\"untitled-app\"".to_owned(),
        packages_base: "\"rust-platform/main.roc\"".to_owned(),
        imports: vec![],
        provides: vec!["main".to_owned()],
        ast_node_id,
    }
}
