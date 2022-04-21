#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[cfg(test)]
use indoc::indoc;

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn hash_specialization() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            Id := U64

            hash = \@Id n -> n

            main = hash (@Id 1234)
            "#
        ),
        1234,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn hash_specialization_multiple_add() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            Id := U64

            hash = \@Id n -> n

            One := {}

            hash = \@One _ -> 1

            main = hash (@Id 1234) + hash (@One {})
            "#
        ),
        1235,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn alias_member_specialization() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            Id := U64

            hash = \@Id n -> n

            main =
                aliasedHash = hash
                aliasedHash (@Id 1234)
            "#
        ),
        1234,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_constrained_in_non_member_usage() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ result ] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes : a, a -> U64 | a has Hash
            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            result = mulHashes (@Id 5) (@Id 7)
            "#
        ),
        35,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_constrained_in_non_member_usage_inferred() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ result ] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            result = mulHashes (@Id 5) (@Id 7)
            "#
        ),
        35,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_constrained_in_non_member_multiple_specializations() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ result ] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes : a, b -> U64 | a has Hash, b has Hash
            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            Three := {}
            hash = \@Three _ -> 3

            result = mulHashes (@Id 100) (@Three {})
            "#
        ),
        300,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_constrained_in_non_member_multiple_specializations_inferred() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ result ] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            Three := {}
            hash = \@Three _ -> 3

            result = mulHashes (@Id 100) (@Three {})
            "#
        ),
        300,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_used_as_type_still_compiles() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ result ] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes : Hash, Hash -> U64
            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            Three := {}
            hash = \@Three _ -> 3

            result = mulHashes (@Id 100) (@Three {})
            "#
        ),
        300,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_member_takes_different_able_variable() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ result ] to "./platform"

            Hash has hash : a -> U64 | a has Hash

            IntoHash has intoHash : a, b -> b | a has IntoHash, b has Hash

            Id := U64
            hash = \$Id n -> n

            User := Id
            intoHash = \$User id, _ -> id

            result = hash (intoHash ($User ($Id 123)) ($Id 1))
            "#
        ),
        123,
        u64
    )
}
