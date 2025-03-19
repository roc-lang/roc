type t = { fresh_tvar : Type.fresh_tvar }

let make () = { fresh_tvar = Type.fresh_tvar_generator () }
