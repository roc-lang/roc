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
    arc_missing_frame_local,
    callable_dictionary_two_dispatchers,
    dictionary_call_result_double_free,
    dictionary_worker_multiple_descriptors,
    dynamic_literal_without_producer,
    recursive_payload_unboxed,
    recursive_storage_inspected_as_box,
    static_dictionary_unresolved_method,
    zero_sized_list_capacity,
    zero_sized_local_into_scalar,

    pub fn text(reason: Reason) []const u8 {
        return switch (reason) {
            .arc_missing_frame_local => ".boxy ARC: a procedure domain lacks a required frame local",
            .callable_dictionary_two_dispatchers => ".boxy plan: one callable dictionary captures evidence for two dispatcher types",
            .dictionary_call_result_double_free => ".boxy dev: materializing a dictionary method call's result drops a Box twice",
            .dictionary_worker_multiple_descriptors => ".boxy plan: a dictionary method worker descriptor maps to several call descriptors",
            .dynamic_literal_without_producer => ".boxy lowering: a dynamic literal target descriptor has no producer template",
            .recursive_payload_unboxed => ".boxy dev codegen: a recursive nominal's payload reaches a Box field slot as its unboxed tag union",
            .recursive_storage_inspected_as_box => ".boxy descriptors do not distinguish a recursive value's storage pointer from a user Box, so inspect renders it as Box(...)",
            .static_dictionary_unresolved_method => ".boxy plan: a static dictionary cannot resolve its method target",
            .zero_sized_list_capacity => ".boxy reports a zero-sized list's capacity as its length instead of 0",
            .zero_sized_local_into_scalar => ".boxy dev codegen: a zero-sized local is copied into a scalar local",
        };
    }
};

/// One listed case, with how and why it fails.
pub const Gap = struct {
    case: []const u8,
    status: Status,
    reason: Reason,
};

/// Every case whose `.boxy` executions do not yet agree with `.lss`.
pub const gaps = [_]Gap{
    .{ .case = "inspect: noncapturing compile-time callable restores attached-method target", .status = .child_failed, .reason = .static_dictionary_unresolved_method },
};
