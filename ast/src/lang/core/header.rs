use super::expr::expr2::ExprId;

#[derive(Debug)]
pub struct AppHeader {
    pub app_name: String,
    pub packages_base: String,
    pub imports: Vec<String>,
    pub provides: Vec<String>,
    pub ast_node_id: ExprId, // TODO probably want to create and use HeaderId
}
