interface Rand
    exposes [ nat ]
    imports [ fx.Effect, Task.{ Task } ]

nat : Task Nat *
nat = Effect.map (Effect.randNat {}) Ok
