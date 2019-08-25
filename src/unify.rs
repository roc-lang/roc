use subs::{Descriptor, FlatType};
use subs::Content::{self, *};

pub fn unify(left: &Descriptor, right: &Descriptor) -> Descriptor {
    match left.content {
        FlexVar(ref opt_name) => {
            unify_flex(opt_name, &right.content)
        },
        RigidVar(ref name) => {
            unify_rigid(name, &right.content)
        },
        Structure(ref flat_type) => {
            unify_structure(flat_type, &right.content)
        }
        Error => {
            // Error propagates. Whatever we're comparing it to doesn't matter!
            from_content(Error)
        }
    }
}

#[inline(always)]
fn unify_structure(flat_type: &FlatType, other: &Content) -> Descriptor {
    match other {
        FlexVar(_) => {
            // If the other is flex, Structure wins!
            from_content(Structure(flat_type.clone()))
        },
        RigidVar(_) => {
            // Type mismatch! Rigid can only unify with flex.
            from_content(Error)
        },
        Structure(ref other_flat_type) => {
            // Type mismatch! Rigid can only unify with flex.
            unify_flat_type(flat_type, other_flat_type)
        },
        Error => {
            // Error propagates.
            from_content(Error)
        },
    }
}

#[inline(always)]
fn unify_flat_type(left: &FlatType, right: &FlatType) -> Descriptor {
    panic!("TODO");
        // case (flatType, otherFlatType) of
        //   (App1 home name args, App1 otherHome otherName otherArgs) | home == otherHome && name == otherName ->
        //       Unify $ \vars ok err ->
        //         let
        //           ok1 vars1 () =
        //             case merge context otherContent of
        //               Unify k ->
        //                 k vars1 ok err
        //         in
        //         unifyArgs vars context args otherArgs ok1 err

        //   (Fun1 arg1 res1, Fun1 arg2 res2) ->
        //       do  subUnify arg1 arg2
        //           subUnify res1 res2
        //           merge context otherContent

        //   (EmptyRecord1, EmptyRecord1) ->
        //       merge context otherContent

        //   (Record1 fields ext, EmptyRecord1) | Map.null fields ->
        //       subUnify ext (_second context)

        //   (EmptyRecord1, Record1 fields ext) | Map.null fields ->
        //       subUnify (_first context) ext

        //   (Record1 fields1 ext1, Record1 fields2 ext2) ->
        //       Unify $ \vars ok err ->
        //         do  structure1 <- gatherFields fields1 ext1
        //             structure2 <- gatherFields fields2 ext2
        //             case unifyRecord context structure1 structure2 of
        //               Unify k ->
        //                 k vars ok err

        //   (Tuple1 a b Nothing, Tuple1 x y Nothing) ->
        //       do  subUnify a x
        //           subUnify b y
        //           merge context otherContent

        //   (Tuple1 a b (Just c), Tuple1 x y (Just z)) ->
        //       do  subUnify a x
        //           subUnify b y
        //           subUnify c z
        //           merge context otherContent

        //   (Unit1, Unit1) ->
        //       merge context otherContent

        //   _ ->
        //       mismatch
}


#[inline(always)]
fn unify_rigid(name: &String, other: &Content) -> Descriptor {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            from_content(RigidVar(name.clone()))
        },
        RigidVar(_) | Structure(_) => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            from_content(Error)
        },
        Error => {
            // Error propagates.
            from_content(Error)
        },
    }
}


#[inline(always)]
fn unify_flex(opt_name: &Option<String>, other: &Content) -> Descriptor {
    match other {
        FlexVar(None) => {
            // If both are flex, and only left has a name, keep the name around.
            from_content(FlexVar(opt_name.clone()))
        },
        FlexVar(Some(_)) | RigidVar(_) | Structure(_) | Error => {
            // In all other cases, if left is flex, defer to right.
            // (This includes using right's name if both are flex and named.)
            from_content(other.clone())
        },
    }
}

/// TODO this was f/k/a merge() - got rid of the rank stuff...good idea? Bad?
/// TODO it used to be { rank: std::cmp::min(left_rank, right_rank), ... }
fn from_content(content: Content) -> Descriptor {
    Descriptor {
        content,
        rank: 0,
        mark: 2, // no mark
        copy: None
    }
}

