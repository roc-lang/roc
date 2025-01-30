/// How function kinds should be represented in the type system.
#[derive(Debug, Clone, Copy)]
pub enum FunctionKind {
    /// Function values are solved to lambda sets; lambda sets are the kind.
    LambdaSet,
    /// Function values are erased, no kind is introduced.
    Erased,
}

impl FunctionKind {
    pub fn from_env() -> Self {
        if cfg!(debug_assertions) {
            if std::env::var("EXPERIMENTAL_ROC_ERASE").is_ok() {
                FunctionKind::Erased
            } else {
                FunctionKind::LambdaSet
            }
        } else {
            FunctionKind::LambdaSet
        }
    }
}
