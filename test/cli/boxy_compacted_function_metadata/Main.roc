import Effect
import Media

Main := []

saved : Effect(U64)
saved = Media.play("#player", |done| match done { Ok(_) => 42, Err(_) => 0 })

expect Effect.fire(saved, [0]) == 42

