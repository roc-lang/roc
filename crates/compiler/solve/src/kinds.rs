/// How function kinds should be represented in the type system.
#[derive(Debug, Clone, Copy)]
pub enum FunctionKind {
    /// Function values are solved to lambda sets; lambda sets are the kind.
    LambdaSet,
    /// Function values are erased, no kind is introduced.
    Erased,
}
