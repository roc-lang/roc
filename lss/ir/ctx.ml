type t = { symbols : Symbol.t; type_cache : Lower_type.type_cache }

let make ~symbols = { symbols; type_cache = Lower_type.new_type_cache () }
