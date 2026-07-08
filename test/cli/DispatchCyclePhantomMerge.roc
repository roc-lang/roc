# Dispatch-cycle matrix (plan.md item 6/6a): two unannotated defs mutually
# reachable ONLY through value-receiver method dispatch. The name graph sees
# just the edge Counter.step -> ping (ping's `.step()` call is type-directed),
# so ping's group is checked first; its dispatch obligation on Counter.step is
# discovered at ping's generalization boundary, Counter.step's group is checked
# in a nested frame there, and its back-reference to the suspended `ping` links
# monomorphically — the groups merge and infer together.
Counter := [Mk(U64)].{
    step = |c, k| match c {
        Counter.Mk(n) => if k == 0 { n } else { ping(c, k - 1) }
    }
}

ping = |c, k| c.step(k)

# NOTE: check-only for now. Running (or compile-time evaluating) mutual
# recursion through a where-clause dispatch trips a PRE-EXISTING postcheck
# evidence bug ('specialization edges disagreed on dispatch evidence') that
# reproduces identically on main before this project; type checking is the
# part under test here. The `args` dependency keeps the recursion out of
# compile-time evaluation.
main! = |args| {
    result = ping(Counter.Mk(7), List.len(args) + 3)
    if result != 7 {
        crash "phantom dispatch cycle returned the wrong value"
    }
    Ok({})
}
