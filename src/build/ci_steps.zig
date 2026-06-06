//! Build-system helpers for CI checks that are still custom build steps.

const std = @import("std");
const builtin = @import("builtin");

const Step = std.Build.Step;

/// Build step that runs the checked-data audit gate.
/// Skipped on Windows because perl is not preinstalled there; Linux/macOS CI
/// still enforces this gate.
pub const SemanticAuditStep = struct {
    step: Step,

    pub fn create(b: *std.Build) *SemanticAuditStep {
        const self = b.allocator.create(SemanticAuditStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "semantic-audit",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, _: Step.MakeOptions) !void {
        if (builtin.os.tag == .windows) {
            std.debug.print("Skipping checked-data audit on Windows (perl unavailable)\n", .{});
            return;
        }

        const b = step.owner;
        var child = try std.process.spawn(b.graph.io, .{
            .argv = &.{ "perl", "ci/semantic_audit.pl" },
            .environ_map = &b.graph.environ_map,
        });
        const term = try child.wait(b.graph.io);

        switch (term) {
            .exited => |code| {
                if (code != 0) {
                    return step.fail("Checked-data audit failed. Run 'perl ci/semantic_audit.pl' to see details.", .{});
                }
            },
            else => return step.fail("ci/semantic_audit.pl terminated abnormally", .{}),
        }
    }
};
