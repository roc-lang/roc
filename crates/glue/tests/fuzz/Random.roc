# Adapted from https://github.com/JanCVanB/roc-random/ by Jan Van Bruggen,
# licensed under UPL 1.0
interface Random
    exposes [
        Generator,
        Seed,
        i8,
        u8,
        i16,
        u16,
        i32,
        u32,
        seed,
        step,
        list,
        constant,
        str,
        map,
        andThen,
        printableAscii,
        printableAsciiByte,
        unicodeScalar,
        uniform,
    ]
    imports []

## # Types

## A psuedorandom value generator
Generator val := Seed -> (val, Seed)
Seed := U32 # TODO use a 64-bit seed and change algorithms accordingly

## A psuedorandom value, paired with its [Generator]'s output state (for chaining)

## Construct a "seed"
##
## A "seed" is an initial [State] for [Generator]s.
##
## This is an alias for [seed32].
seed : U64 -> Seed
seed = \num -> @Seed (Num.toU32 num)

## Generate a [Generation] from a state
step : Seed, Generator val -> (val, Seed)
step = \s, @Generator gen -> gen s

map : Generator a, (a -> b) -> Generator b
map = \@Generator gen, transform ->
    @Generator \s1 ->
        (a, s2) = gen s1
        b = transform a

        (b, s2)

andThen : Generator a, (a -> Generator b) -> Generator b
andThen = \@Generator gen1, transform ->
    @Generator \s1 ->
        (a, s2) = gen1 s1
        (@Generator gen2) = transform a

        gen2 s2

## Construct a [Generator] for 8-bit signed integers between two boundaries (inclusive)
i8 : I8, I8 -> Generator I8
i8 = \x, y ->
    (minimum, maximum) = sort x y
    # TODO: Remove these `I64` dependencies.
    range = maximum - minimum + 1 |> Num.toI64
    @Generator \s ->
        # TODO: Analyze this. The mod-ing might be biased towards a smaller offset!
        offset = permute s |> Num.toU8 |> mapToI8 |> Num.toI64 |> Num.subWrap (Num.toI64 Num.minI8) |> Num.rem range
        value = minimum |> Num.toI64 |> Num.addWrap offset |> Num.toI8
        (value, update s)

## Construct a [Generator] for 16-bit signed integers between two boundaries (inclusive)
i16 : I16, I16 -> Generator I16
i16 = \x, y ->
    (minimum, maximum) = sort x y
    # TODO: Remove these `I64` dependencies.
    range = maximum - minimum + 1 |> Num.toI64
    @Generator \s ->
        # TODO: Analyze this. The mod-ing might be biased towards a smaller offset!
        offset = permute s |> Num.toU16 |> mapToI16 |> Num.toI64 |> Num.subWrap (Num.toI64 Num.minI16) |> Num.rem range
        value = minimum |> Num.toI64 |> Num.addWrap offset |> Num.toI16
        (value, update s)

## Construct a [Generator] for 32-bit signed integers between two boundaries (inclusive)
i32 : I32, I32 -> Generator I32
i32 = \x, y ->
    (minimum, maximum) = sort x y
    # TODO: Remove these `I64` dependencies.
    range = maximum - minimum + 1 |> Num.toI64
    @Generator \s ->
        # TODO: Analyze this. The mod-ing might be biased towards a smaller offset!
        offset = permute s |> mapToI32 |> Num.toI64 |> Num.subWrap (Num.toI64 Num.minI32) |> Num.rem range
        value = minimum |> Num.toI64 |> Num.addWrap offset |> Num.toI32
        (value, update s)

## Construct a [Generator] for 8-bit unsigned integers between two boundaries (inclusive)
u8 : U8, U8 -> Generator U8
u8 = \x, y -> betweenU32 (Num.toU32 x) (Num.toU32 y) |> map Num.toU8

## u8, independent seeds
expect
    (num1, _) = step (seed 12345) (u8 0 10)
    (num2, _) = step (seed 54321) (u8 0 10)
    (num3, _) = step (seed 24680) (u8 0 10)

    num1 == 4 && num2 == 1 && num3 == 10

## u8, chained seeds
expect
    s0 = seed 32104

    (num1, s1) = step s0 (u8 0 10)
    (num2, s2) = step s1 (u8 0 10)
    (num3, _) = step s2 (u8 0 10)

    num1 == 2 && num2 == 8 && num3 == 7

## u8, limits
expect
    (num1, _) = step (seed 12345) (u8 3 3)
    (num2, _) = step (seed 54321) (u8 0 0)
    (num3, _) = step (seed 54321) (u8 Num.maxU8 Num.maxU8)
    (num4, _) = step (seed 24680) (u8 7 5)

    num1 == 3 && num2 == 0 && num3 == 255 && num4 == 6

## Construct a [Generator] for 16-bit unsigned integers between two boundaries (inclusive)
u16 : U16, U16 -> Generator U16
u16 = \x, y -> betweenU32 (Num.toU32 x) (Num.toU32 y) |> map Num.toU16

## u16, independent seeds
expect
    (num1, _) = step (seed 12345) (u16 0 500)
    (num2, _) = step (seed 54321) (u16 0 500)
    (num3, _) = step (seed 24680) (u16 0 500)

    num1 == 492 && num2 == 264 && num3 == 97

## u16, chained seeds
expect
    s0 = seed 32104

    (num1, s1) = step s0 (u16 0 500)
    (num2, s2) = step s1 (u16 0 500)
    (num3, _) = step s2 (u16 0 500)

    num1 == 38 && num2 == 402 && num3 == 272

## u16, limits
expect
    (num1, _) = step (seed 12345) (u16 12345 12345)
    (num2, _) = step (seed 54321) (u16 0 0)
    (num3, _) = step (seed 54321) (u16 Num.maxU16 Num.maxU16)
    (num4, _) = step (seed 24680) (u16 7 5)

    num1 == 12345 && num2 == 0 && num3 == 65535 && num4 == 6

## Construct a [Generator] for 32-bit unsigned integers between two boundaries (inclusive)
u32 : U32, U32 -> Generator U32
u32 = \x, y -> betweenU32 x y

## u32, independent seeds
expect
    (num1, _) = step (seed 12345) (u32 0 400000)
    (num2, _) = step (seed 54321) (u32 0 400000)
    (num3, _) = step (seed 24680) (u32 0 400000)

    num1 == 25799 && num2 == 280838 && num3 == 21173

## u32, chained seeds
expect
    s0 = seed 32104

    (num1, s1) = step s0 (u32 0 400000)
    (num2, s2) = step s1 (u32 0 400000)
    (num3, _) = step s2 (u32 0 400000)

    num1 == 260505 && num2 == 164515 && num3 == 246635

## u32, limits
expect
    (num1, _) = step (seed 12345) (u32 1234567 1234567)
    (num2, _) = step (seed 54321) (u32 0 0)
    (num3, _) = step (seed 54321) (u32 Num.maxU32 Num.maxU32)
    (num4, _) = step (seed 24680) (u32 7 5)

    num1 == 1234567 && num2 == 0 && num3 == 4294967295 && num4 == 6

## Generate one of the elements in the "nonempty list" (an element plus a [List] of additional elements),
## with a uniform distribution of probability for picking any one of them.
uniform : a, List a -> Generator a
uniform = \default, elems ->
    @Generator \s1 ->
        # We explicitly want the index to potentially be
        # out of bounds by 1 (so, the length of the list)
        # so that we can include the default value in the
        # uniform distribution.
        (index, s2) = step s1 (u32 0 (List.len elems |> Num.toU32)) # TODO switch from u32 to u64, and from Num.toU32 to Num.toU64

        answer =
            elems
            |> List.get (Num.toNat index)
            |> Result.withDefault default

        (answer, s2)

## uniform, independent seeds
expect
    (val1, _) = step (seed 12345) (uniform 1 [2, 3, 4, 5])
    (val2, _) = step (seed 54321) (uniform 1 [2, 3, 4, 5])
    (val3, _) = step (seed 24680) (uniform 1 [2, 3, 4, 5])

    val1 == 4 && val2 == 1 && val3 == 1

## uniform, chained seeds
expect
    s0 = seed 42134

    (val1, s1) = step s0 (uniform "foo" ["bar", "baz", "blah"])
    (val2, s2) = step s1 (uniform "foo" ["bar", "baz", "blah"])
    (val3, _) = step s2 (uniform "foo" ["bar", "baz", "blah"])

    val1 == "blah" && val2 == "foo" && val3 == "bar"

## uniform, limits
expect
    (val1, _) = step (seed 12345) (uniform "blah" [])
    (val2, _) = step (seed 54321) (uniform "blah" [])
    (val3, _) = step (seed 54321) (uniform "blah" [])

    val1 == "blah" && val2 == "blah" && val3 == "blah"


# ### Helpers for the above functions

betweenU32 : U32, U32 -> Generator U32
betweenU32 = \x, y ->
    minimum = Num.min x y
    range =
        Num.absDiff x y
        |> Num.addWrap 1

    @Generator \s ->
        # TODO: Analyze this. The mod-ing might be biased towards a smaller offset!
        (minimum + (permute s % range), update s)

mapToI8 : U8 -> I8
mapToI8 = \num ->
    Num.toI8 num
    |> Num.subWrap (Num.maxI8)
    |> Num.subWrap 1

mapToI16 : U16 -> I16
mapToI16 = \num ->
    Num.toI16 num
    |> Num.subWrap (Num.maxI16)
    |> Num.subWrap 1

mapToI32 : U32 -> I32
mapToI32 = \num ->
    Num.toI32 num
    |> Num.subWrap (Num.maxI32)
    |> Num.subWrap 1

## It's impossible to have a Str or List of more than Num.maxI64 elements or bytes
longestListAllowed : U64
longestListAllowed =
    Num.maxI64
    |> Num.toU64

list : Generator a, U64 -> Generator (List a)
list = \genElem, requestedMaxLen ->
    maxLen =
        Num.min requestedMaxLen longestListAllowed
        |> Num.toU32 # TODO remove this once we're doing 64-bit randomness

    @Generator \s1 ->
        (len, s2) = step s1 (u32 0 maxLen)

        List.withCapacity (Num.toNat len)
        |> listHelp genElem s2 len

listHelp : List a, Generator a, Seed, U32 -> (List a, Seed)
listHelp = \accum, genElem, s1, remaining ->
    if remaining == 0 then
        (accum, s1)
    else
        (nextElem, s2) = step s1 genElem

        accum
        |> List.append nextElem
        |> listHelp genElem s2 (Num.subWrap remaining 1)

## Guaranteed to be a valid Unicode Scalar Value,
## with uniform distribution across all scalars
unicodeScalar : Generator U32
unicodeScalar =
    @Generator \s1 ->
        (val1, s2) = step s1 (u32 0 0x100000)
        offset = 0x0800 * if val1 >= 0xD800 then 1 else 0
        (Num.addWrap val1 offset, s2)

printableAsciiByte : Generator U8
printableAsciiByte = u8 32 127 # The printable ASCII characters are in this range

printableAscii : U64 -> Generator Str
printableAscii = \requestedMaxBytes ->
    maxBytes =
        Num.min requestedMaxBytes longestListAllowed
        |> Num.toU32 # TODO remove this once we're doing 64-bit randomness

    @Generator \s1 ->
        (bytes, s2) = step s1 (u32 0 maxBytes)

        Str.withCapacity (Num.toNat bytes)
        |> printableAsciiHelp s2 bytes

printableAsciiHelp : Str, Seed, U32 -> (Str, Seed)
printableAsciiHelp = \accum, s1, remaining ->
    if remaining == 0 then
        (accum, s1)
    else
        (nextByte, s2) = step s1 printableAsciiByte

        accum
        |> Str.appendScalar (Num.toU32 nextByte) # TODO use Str.appendAscii once that exists
        |> Result.withDefault "" # This can never happen
        |> printableAsciiHelp s2 (Num.subWrap remaining 1)

## printableAscii, independent seeds
expect
    (str1, _) = step (seed 12345) (printableAscii 10)
    (str2, _) = step (seed 54321) (printableAscii 20)
    (str3, _) = step (seed 24680) (printableAscii 30)

    str1 == "G'p-" && str2 == "z?H%&;t*6Wv=" && str3 == "EF;4jvWv]>s,hJoX5"

## printableAscii, chained seeds
expect
    s0 = seed 32104

    (str1, s1) = step s0 (printableAscii 10)
    (str2, s2) = step s1 (printableAscii 20)
    (str3, _) = step s2 (printableAscii 30)

    str1 == "eF" && str2 == "tJ676}~3Lhjo85Vkeqg" && str3 == "0m"

## printableAscii, limits
expect
    (str1, _) = step (seed 12345) (printableAscii 0)
    (str2, _) = step (seed 54321) (printableAscii 1)

    str1 == "" && str2 == "z"

str : U64 -> Generator Str
str = \requestedMaxBytes ->
    maxBytes =
        # Subtract 4 from the longest allowed, so we don't
        # run over if the last scalar we happen to add
        # happens to take up 4 UTF-8 bytes.
        Num.min requestedMaxBytes (Num.subWrap longestListAllowed 4)
        |> Num.toU32 # TODO remove this once we're doing 64-bit randomness

    @Generator \s1 ->
        (bytes, s2) = step s1 (u32 0 maxBytes)

        Str.withCapacity (Num.toNat bytes)
        |> strHelp s2 bytes

strHelp : Str, Seed, U32 -> (Str, Seed)
strHelp = \accum, s1, remaining ->
    if remaining == 0 then
        (accum, s1)
    else
        (nextScalar, s2) = step s1 unicodeScalar

        accum
        |> Str.appendScalar nextScalar
        |> Result.withDefault "" # This can never happen
        |> strHelp s2 (Num.subWrap remaining 1)

## str, independent seeds
expect
    (str1, _) = step (seed 12345) (str 10)
    (str2, _) = step (seed 54321) (str 20)
    (str3, _) = step (seed 24680) (str 30)

    str1 == "𖉈򗎥𢬉󢳣" && str2 == "򩵚򬝄󯗢򻆀󦔹򃛍򙄜񆠞𱨗󉪁󄪣򺞿" && str3 == "򫒧񝘂񤅦򇴡𑒤񙘝򪐻󙤹򉝁񩎾򯇇򟄻󆵤򳴍󏸽򩈘𨷙"

## str, chained seeds
expect
    s0 = seed 32104

    (str1, s1) = step s0 (str 10)
    (str2, s2) = step s1 (str 20)
    (str3, _) = step s2 (str 30)

    str1 == "𐊽𜆧" && str2 == "𯵷񂛓򠆃򵰔󐷢򕨮򅋇򾘋򵌤򾈒󸑝񲵷򮫧𚜘񽒟񡩪򸼖󛻫񢹤" && str3 == "𻃜񝊇"

## str, limits
expect
    (str1, _) = step (seed 12345) (str 0)
    (str2, _) = step (seed 54321) (str 1)

    str1 == "" && str2 == "򩵚"

constant : a -> Generator a
constant = \val -> @Generator \s -> (val, s)

sort : Num a, Num a -> (Num a, Num a)
sort = \x, y ->
    if x < y then
        (x, y)
    else
        (y, x)

# ### PCG algorithms, constants, and wrappers
#
# Based on this paper: https://www.pcg-random.org/pdf/hmc-cs-2014-0905.pdf
# Based on this C++ header: https://github.com/imneme/pcg-c/blob/master/include/pcg_variants.h
# Abbreviations:
#     M = Multiplication (see section 6.3.4 on page 45 in the paper)
#     PCG = Permuted Congruential Generator
#     RXS = Random XorShift (see section 5.5.1 on page 36 in the paper)
#     XS = XorShift (see section 5.5 on page 34 in the paper)

# See `RXS M XS` constants (line 168?)
# and `_DEFAULT_` constants (line 276?)
# in the PCG C++ header (see link above).
defaultU32PermuteMultiplier = 277_803_737
defaultU32PermuteRandomXorShift = 28
defaultU32PermuteRandomXorShiftIncrement = 4
defaultU32PermuteXorShift = 22
defaultU32UpdateIncrement = 2_891_336_453
defaultU32UpdateMultiplier = 747_796_405
# TODO: Debug these untested 64-bit functions
# defaultU64PermuteMultiplier = 12_605_985_483_714_917_081
# defaultU64PermuteRandomXorShift = 59
# defaultU64PermuteRandomXorShiftIncrement = 5
# defaultU64PermuteXorShift = 43
# defaultU64UpdateIncrement = 1_442_695_040_888_963_407
# defaultU64UpdateMultiplier = 6_364_136_223_846_793_005
# TODO: Debug these untested 128-bit functions
# defaultU128PermuteMultiplier = (Num.shiftLeftBy 64 17_766_728_186_571_221_404) + 12_605_985_483_714_917_081
# defaultU128PermuteRandomXorShift = 122
# defaultU128PermuteRandomXorShiftIncrement = 6
# defaultU128PermuteXorShift = 86
# defaultU128UpdateIncrement = (Num.shiftLeftBy 64 6_364_136_223_846_793_005) + 1_442_695_040_888_963_407
# defaultU128UpdateMultiplier = (Num.shiftLeftBy 64 2_549_297_995_355_413_924) + 4_865_540_595_714_422_341

# See `pcg_output_rxs_m_xs_8_8` (on line 170?) in the PCG C++ header (see link above).
permute : Seed -> U32
permute = \@Seed num ->
    # See section 6.3.4 on page 45 in the PCG paper (see link above).
    xorBy =
        defaultU32PermuteRandomXorShift
        |> Num.shiftRightZfBy (Num.toU8 num)
        |> Num.addWrap defaultU32PermuteRandomXorShiftIncrement
        |> Num.shiftRightZfBy (Num.toU8 num)

    partial =
        num
        |> Num.bitwiseXor xorBy
        |> Num.mulWrap defaultU32PermuteMultiplier

    partial
    |> Num.bitwiseXor (Num.shiftRightZfBy defaultU32PermuteXorShift (Num.toU8 partial))

# See `pcg_oneseq_8_step_r` (line 409?) in the PCG C++ header (see link above).
update : Seed -> Seed
update = \@Seed num ->
    # See section 4.1 on page 20 in the PCG paper (see link above).
    num
    |> Num.mulWrap defaultU32UpdateMultiplier
    |> Num.addWrap defaultU32UpdateIncrement
    |> @Seed
