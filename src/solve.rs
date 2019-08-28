// type Env =
//   Map.Map Name.Name Variable


// type Pools =
//   MVector.IOVector [Variable]


// data State =
//   State
//     { _env :: Env
//     , _mark :: Mark
//     , _errors :: [Error.Error]
//     }

use subs::{Subs, Variable, Descriptor, Content, FlatType};
use types::Constraint::{self, *};
use types::Type::{self, *};

pub fn solve(subs: &mut Subs, constraint: Constraint) {
    println!("\nSolving:\n\n\t{:?}\n\n", constraint);
    match constraint {
        True => (),
        Eq(typ, expected_type, region) => {
            let actual = type_to_variable(subs, typ);
            let expected = type_to_variable(subs, expected_type.unwrap());

            subs.union(actual, expected);
        },
        And(sub_constraints) => {
            for sub_constraint in sub_constraints {
                solve(subs, sub_constraint);
            }
        },
        Let(box_let_constraint) => {
            let let_con = *box_let_constraint;

//     CLet [] flexs _ headerCon CTrue ->
//       do  introduce rank pools flexs
//           solve env rank pools state headerCon

//     CLet [] [] header headerCon subCon ->
//       do  state1 <- solve env rank pools state headerCon
//           locals <- traverse (A.traverse (typeToVariable rank pools)) header
//           let newEnv = Map.union env (Map.map A.toValue locals)
//           state2 <- solve newEnv rank pools state1 subCon
//           foldM occurs state2 $ Map.toList locals

//     CLet rigids flexs header headerCon subCon ->
//       do
//           -- work in the next pool to localize header
//           let nextRank = rank + 1
//           let poolsLength = MVector.length pools
//           nextPools <-
//             if nextRank < poolsLength
//               then return pools
//               else MVector.grow pools poolsLength

//           -- introduce variables
//           let vars = rigids ++ flexs
//           forM_ vars $ \var ->
//             UF.modify var $ \(Descriptor content _ mark copy) ->
//               Descriptor content nextRank mark copy
//           MVector.write nextPools nextRank vars

//           -- run solver in next pool
//           locals <- traverse (A.traverse (typeToVariable nextRank nextPools)) header
//           (State savedEnv mark errors) <-
//             solve env nextRank nextPools state headerCon

//           let youngMark = mark
//           let visitMark = nextMark youngMark
//           let finalMark = nextMark visitMark

//           -- pop pool
//           generalize youngMark visitMark nextRank nextPools
//           MVector.write nextPools nextRank []

//           -- check that things went well
//           mapM_ isGeneric rigids

//           let newEnv = Map.union env (Map.map A.toValue locals)
//           let tempState = State savedEnv finalMark errors
//           newState <- solve newEnv rank nextPools tempState subCon

//           foldM occurs newState (Map.toList locals)


        },
    }
}

fn type_to_variable(subs: &mut Subs, typ: Type) -> Variable {
    match typ {
        Variable(var) => var,
        Apply(module_name, name, arg_types) => {
            let args: Vec<Variable> =
                arg_types.into_iter()
                    .map(|arg| type_to_variable(subs, arg))
                    .collect();

            let flat_type = FlatType::Apply(module_name, name, args);
            let content = Content::Structure(flat_type);

            subs.fresh(Descriptor::from(content))
        },
        EmptyRec => {
            let content = Content::Structure(FlatType::EmptyRecord);

            subs.fresh(Descriptor::from(content))
        },
        _ => panic!("TODO type_to_var")

        // AppN home name args ->
        // do  argVars <- traverse go args
        //     register rank pools (Structure (App1 home name argVars))

        // FunN a b ->
        // do  aVar <- go a
        //     bVar <- go b
        //     register rank pools (Structure (Fun1 aVar bVar))
    }
}

