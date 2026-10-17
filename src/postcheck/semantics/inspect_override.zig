//! Selecting the `to_inspect` override inspection calls (design.md "Inspect
//! Overrides"), shared by Monotype lowering, Boxy planning, and Boxy lowering.
//!
//! Whether a `to_inspect` declaration is an eligible override is checked data:
//! the checked method registry records the decision once per declaration. What
//! remains for post-check stages is choosing whose registry decides, and that
//! is the view that declares `owner.to_inspect`, selected exactly as method
//! dispatch selects it. The first view in search order that declares the
//! method decides: its registry's override, or none when the declaration is
//! ineligible or rejected, in which case inspection renders the default form.
//! The registry records only procedures as overrides, so a local procedure's
//! `to_inspect` (a method of a type declared in a function body) is never one,
//! whichever view is consulted.

/// The override inspection calls for one owner, or null for the default
/// rendering.
///
/// `Ctx` supplies the candidate views in method-dispatch search order:
///
/// ```
/// Found: type                     // the caller's record of a selected override
/// viewCount() usize
/// view(index) View
/// inspectKey(view) ?MethodKey      // owner.to_inspect in the view's names, or
///                                  // null when the view names neither
/// registry(view) *const MethodRegistry
/// found(view, key, override) Found
/// ```
pub fn lookupInspectOverride(ctx: anytype) ?@TypeOf(ctx).Found {
    var index: usize = 0;
    while (index < ctx.viewCount()) : (index += 1) {
        const view = ctx.view(index);
        const key = ctx.inspectKey(view) orelse continue;
        const registry = ctx.registry(view);
        // A declared `to_inspect` decides inspection in this view.
        _ = registry.lookup(key) orelse continue;
        const override = registry.inspectOverride(key) orelse return null;
        return ctx.found(view, key, override);
    }
    return null;
}
