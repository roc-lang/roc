#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Problem<LcStrId, UcStrId, ShorthandStrId, ModuleStrId, Region> {
    ShadowedTopLevelLc(LcStrId, Region),
    ShadowedTopLevelUc(UcStrId, Region),
    ShadowedModuleImport(ModuleStrId, Region),
    /// This can happen when there's a qualified lookup where the module name isn't in scope.
    ModuleNotInScope(ModuleStrId, Region),

    ModuleNotInPkg(ShorthandStrId, ModuleStrId, Region),

    /// Roc modules can only have u16::MAX top-level lowercase bindings.
    /// (Technically also only u16::MAX top-level uppercsae bindings and module imports,
    /// but since those go on their own lines, and we already restrict to u16::MAX lines,
    /// those cannot overflow. Lowercase can overflow due to destructuring allowing
    /// more than one lowercase binding per line.)
    TooManyBindings,
}
