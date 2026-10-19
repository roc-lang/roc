app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Closed

# Checking and native execution share this closed producer.
initial : U64
initial = Closed.total(10)

# A type declared here specializes an imported procedure whose static data is
# named per specialization.
Planet := [Earth, Mars].{
    name : Planet -> Str
    name = |planet| {
        match planet {
            Earth => "earth"
            Mars => "mars"
        }
    }
}

main! = |args| {
    runtime = Closed.total(args.len().to_u64() + 10)
    planets = if args.len() > 5 [Planet.Mars] else [Planet.Earth, Planet.Mars]
    first = match Closed.first_or_missing(planets) {
        Ok(planet) => planet.name()
        Err(message) => message
    }
    Stdout.line!("original ${initial.to_str()} ${runtime.to_str()} ${first}")
    Ok({})
}
