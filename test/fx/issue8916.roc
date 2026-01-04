app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Issue 8916: Pattern matching on recursive nominal type with Box crashes
# with "cast causes pointer to be null" at extractTagValue when the type
# contains a no-payload variant.

Nat := [Zero, Suc(Box(Nat))]

main! = || {
    n0 = Nat.Zero
    n1 = Nat.Suc(Box.box(n0))

    v0 = match n0 {
        Nat.Zero => 0
        Nat.Suc(_) => 999
    }

    # Simpler: just test if n1 matches Suc
    v1_disc = match n1 {
        Nat.Zero => 0
        Nat.Suc(_) => 1
    }

    v1 = match n1 {
        Nat.Zero => 999
        Nat.Suc(boxed) =>
            match Box.unbox(boxed) {
                Nat.Zero => 1
                Nat.Suc(_) => 999
            }
    }

    Stdout.line!("n0: ${I64.to_str(v0)}, n1_disc: ${I64.to_str(v1_disc)}, n1: ${I64.to_str(v1)}")
}
