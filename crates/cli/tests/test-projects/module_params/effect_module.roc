module { stdout! } -> [log!]

log! = \msg, level -> stdout!("${level}:${msg}")
