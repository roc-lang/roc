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

use subs::{Subs, Variable};
use types::Constraint::{self, *};
use types::Type::{self, *};

pub fn solve(subs: &mut Subs, constraint: Constraint) {
    match constraint {
        True => (),
        Eq(region, typ, expected_type) => {
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
        _ => panic!("TODO type_to_var")
    }
}

