//! Eval cases whose `.boxy` executions do not yet agree with `.lss`, each
//! with the failure it shows and why. The differential runner counts these
//! per reason instead of failing on them, and fails when a listed case
//! agrees (its entry must go) or names no case, so the list only shrinks.
//! A failure a case does not list here fails the run.

/// How a listed case's `.boxy` executions fail to agree with `.lss`.
pub const Status = enum {
    /// The executions disagree.
    diverged,
    /// Compiling or running the case panics.
    child_failed,
};

/// Why a listed case fails, so the runner can count gaps by cause.
pub const Reason = enum {
    pub fn text(reason: Reason) []const u8 {
        return switch (reason) {};
    }
};

/// One listed case, with how and why it fails.
pub const Gap = struct {
    case: []const u8,
    status: Status,
    reason: Reason,
};

/// Every case whose `.boxy` executions do not yet agree with `.lss`.
pub const gaps = [_]Gap{};
