app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Closed

initial : U64
initial = Closed.total(10)

Planet := [Earth, Mars].{
    name : Planet -> Str
    name = |planet| {
        match planet {
            Earth => "earth"
            Mars => "mars"
        }
    }
}

# Same app path, different filling of the platform's main! requirement.
main! = |args| {
    runtime = Closed.total(args.len().to_u64() + 10)
    planets = if args.len() > 5 [Planet.Mars] else [Planet.Earth, Planet.Mars]
    first = match Closed.first_or_missing(planets) {
        Ok(planet) => planet.name()
        Err(message) => message
    }
    Stdout.line!("edited ${initial.to_str()} ${runtime.to_str()} ${first}")
    Ok({})
}
