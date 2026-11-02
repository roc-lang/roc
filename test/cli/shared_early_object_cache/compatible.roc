app [process_string] { pf: platform "../../str/platform/main.roc" }

import Closed

# Checking and native execution share this closed producer.
initial : U64
initial = {
    dbg "compatible first"
    dbg "compatible second"
    Closed.total(10)
}

undemanded : U64
undemanded = {
    dbg "compatible undemanded"
    Closed.total(20)
}

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

process_string = |input| {
    runtime = Closed.total(input.to_utf8().len())
    planets = if input.to_utf8().len() > 100 [Planet.Mars] else [Planet.Earth, Planet.Mars]
    first = match Closed.first_or_missing(planets) {
        Ok(planet) => planet.name()
        Err(message) => message
    }
    "original ${initial.to_str()} ${runtime.to_str()} ${first} | Got the following from the host: ${input}"
}
