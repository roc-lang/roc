//! Regression test for https://github.com/roc-lang/roc/issues/12228.
//!
//! Concrete dispatch replay shares a frozen copy of a settled instance among
//! later uses in place of a fresh instance. Whether a default chose a class
//! is a fact about that class, not about its type, so the frozen copy does
//! not carry it.
const TestEnv = @import("TestEnv.zig");

test "issue 12228: two folds seeded with an unannotated 0 check cleanly" {
    // `check_argv`'s fold accumulator is a default-decided `U64`, and the
    // replayed `+` in `visit_workflow` shares a frozen copy of its instance;
    // `visit_workflow`'s accumulator is determined by `$argv_bytes`.
    const source =
        \\Project :: [].{
        \\    check_argv : List(Str), Str -> Try({}, Str)
        \\    check_argv = |argv, name| {
        \\        if argv.len() > 4096 or argv.fold(0, |size, arg| size + arg.to_utf8().len()) > 1048576 {
        \\            return Err("NUL in argv: ${name}")
        \\        }
        \\        Ok({})
        \\    }
        \\    visit_workflow = |workflows, tasks, builds, name, visiting, done| {
        \\        if visiting.contains(name) {
        \\        }
        \\        workflow = workflows.find_first(|w| w.name == name).map_err(|_| "unknown workflow")?
        \\        var $done = done
        \\        var $height = 1.U64
        \\        var $count = 0.U64
        \\        var $argv_bytes = 0.U64
        \\        var $productive = []
        \\        for step in workflow.steps {
        \\            match step {
        \\                RunTask(task_name, argv) => {
        \\                    task = tasks.find_first(|t| t.name == task_name).map_err(|_| "unknown task")?
        \\                    run = task.run.concat(argv)
        \\                    check_argv(run, task_name)?
        \\                    $argv_bytes = $argv_bytes + run.fold(0, |size, arg| size + arg.to_utf8().len())
        \\                }
        \\                BuildArtifact(build_name) => {
        \\                    if !builds.any(|b| b.name == build_name) {
        \\                    }
        \\                }
        \\            }
        \\        }
        \\        Ok($done.append({ name, height: $height, count: $count, argv_bytes: $argv_bytes, steps: $productive }))
        \\    }
        \\}
    ;
    var env = try TestEnv.init("Project", source);
    defer env.deinit();
    try env.assertNoErrors();
}
