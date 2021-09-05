interface Rand
    exposes [ nat, natBetween ]
    imports [ fx.Effect, Task.{ Task } ]

nat : Task Nat *
nat = Effect.map (Effect.randNat {}) Ok

natBetween : Nat, Nat -> Task Nat *
natBetween = \lo, hi ->
    Effect.map (Effect.randNatBetween lo hi) Ok
