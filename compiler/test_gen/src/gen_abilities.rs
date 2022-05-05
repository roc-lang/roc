#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[cfg(test)]
use indoc::indoc;

#[cfg(test)]
use roc_std::RocList;

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
#[cfg(any(feature = "gen-llvm"))]
fn encode() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ myU8Bytes ] to "./platform"

            Encoder fmt := List U8, fmt -> List U8 | fmt has Format

            Encoding has
              toEncoder : val -> Encoder fmt | val has Encoding, fmt has Format

            Format has
              u8 : U8 -> Encoder fmt | fmt has Format

            appendWith : List U8, Encoder fmt, fmt -> List U8 | fmt has Format
            appendWith = \lst, (@Encoder doFormat), fmt -> doFormat lst fmt

            toBytes : val, fmt -> List U8 | val has Encoding, fmt has Format
            toBytes = \val, fmt -> appendWith [] (toEncoder val) fmt


            Linear := {} 

            # impl Format for Linear
            u8 = \n -> @Encoder (\lst, @Linear {} -> List.append lst n)

            MyU8 := U8

            # impl Encoding for MyU8
            toEncoder = \@MyU8 n -> u8 n

            myU8Bytes = toBytes (@MyU8 15) (@Linear {})
            "#
        ),
        RocList::from_slice(&[15]),
        RocList<u8>
    )
}
