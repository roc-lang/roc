use typ::Type;

// constrainDecls :: Can.Decls -> Constraint -> IO Constraint
// constrainDecls decls finalConstraint =
//   case decls of
//     Can.Declare def otherDecls ->
//       Expr.constrainDef Map.empty def =<< constrainDecls otherDecls finalConstraint

//     Can.DeclareRec defs otherDecls ->
//       Expr.constrainRecursiveDefs Map.empty defs =<< constrainDecls otherDecls finalConstraint

//     Can.SaveTheEnvironment ->
//       return finalConstraint



pub type ExpectedType = Type;


pub enum Constraint {
    True,
    Equal(Type, ExpectedType),
    Batch(Vec<Constraint>),
}
