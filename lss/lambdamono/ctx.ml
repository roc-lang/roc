type t = {
  symbols : Symbol.t;
  s_fresh_tvar : Lambdasolved.Type.fresh_tvar;
  fresh_tvar : Type.fresh_tvar;
  specializations : Specializations.t;
}

let make ~symbols ~fresh_tvar program =
  let specializations = Specializations.make symbols program in
  {
    symbols;
    s_fresh_tvar = fresh_tvar;
    fresh_tvar = Type.fresh_tvar_generator ();
    specializations;
  }
