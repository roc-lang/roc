//! Semantic checks are ordinary declarative build commands.
const std = @import("std");
const builtin = @import("builtin");

pub const SemanticAuditStep = struct {
    pub fn create(b: *std.Build) *std.Build.Step {
        if (builtin.os.tag == .windows) {
            return b.step("semantic-audit-unavailable", "Semantic audit runs on Linux and macOS, where Perl is available");
        }
        const inputs = b.addWriteFiles();
        _ = inputs.addCopyDirectory(b.path("src"), "src", .{ .include_extensions = &.{".zig"} });
        _ = inputs.addCopyFile(b.path("ci/semantic_audit.pl"), "ci/semantic_audit.pl");
        const run = b.addSystemCommand(&.{ "perl", "ci/semantic_audit.pl" });
        run.setCwd(inputs.getDirectory());
        run.expectExitCode(0);
        return &run.step;
    }
};
