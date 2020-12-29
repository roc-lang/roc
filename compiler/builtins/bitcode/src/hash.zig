const std = @import("std");
const mem = std.mem;
const expectEqual = std.testing.expectEqual;

const primes = [6]u64{
    0xa0761d6478bd642f,
    0xe7037ed1a0b428db,
    0x8ebc6af09c88c6e3,
    0x589965cc75374cc3,
    0x1d8e4e27c47d124f,
    0xeb44accab455d165,
};

fn read_bytes(comptime bytes: u8, data: []const u8) u64 {
    return mem.readVarInt(u64, data[0..bytes], @import("builtin").endian);
}

fn read_8bytes_swapped(data: []const u8) u64 {
    return (read_bytes(4, data) << 32 | read_bytes(4, data[4 .. ]));
}

fn mum(a: u64, b :u64) u64 {
    var r : u128 = @intCast(u128, a) * @intCast(u128, b);
    r = (r >> 64) ^ r;

    return @truncate(u64, r);
}

pub fn hash(key: []const u8, init_seed: u64) u64 {
    const len = key.len;


    var seed = init_seed;

    var i: usize = 0;

    while (i + 32 <= len) {
         seed = mum(
            seed ^ primes[0],
            mum(
                read_bytes(8, key[i ..]) ^ primes[1],
                read_bytes(8, key[i + 8 ..]) ^ primes[2]
            ) ^
                mum(
                    read_bytes(8, key[i + 16 ..]) ^ primes[3],
                    read_bytes(8, key[i + 24 ..]) ^ primes[4]
                )
         );

         i += 32;
    }

    seed ^= primes[0];

    const rem_len = @truncate(u5, len);

    if (rem_len != 0) {

        const rem_bits = @truncate(u3, rem_len % 8);
        const rem_bytes = @truncate(u2, (len - 1) / 8);
        const rem_key = key[ i + @intCast(usize, rem_bytes) * 8 ..];

        const rest = switch (rem_bits) {
            0 => read_8bytes_swapped(rem_key),
            1 => read_bytes(1, rem_key),
            2 => read_bytes(2, rem_key),
            3 => read_bytes(2, rem_key) <<  8 | read_bytes(1, rem_key[2..]),
            4 => read_bytes(4, rem_key),
            5 => read_bytes(4, rem_key) <<  8 | read_bytes(1, rem_key[4..]),
            6 => read_bytes(4, rem_key) << 16 | read_bytes(2, rem_key[4..]),
            7 => read_bytes(4, rem_key) << 24 | read_bytes(2, rem_key[4..]) << 8 | read_bytes(1, rem_key[6..]),
        } ^
            primes[@intCast(usize, rem_bytes) + 1];

        seed = switch (rem_bytes) {
            0 => mum(seed, rest),
            1 => mum(read_8bytes_swapped(key[i .. ]) ^ seed, rest),
            2 => mum (
                read_8bytes_swapped(key[ i .. ]) ^ seed,
                read_8bytes_swapped(key[ i + 8 .. ]) & primes[2]
            ) ^
                mum(seed, rest),
            3 => mum(
                read_8bytes_swapped(key[ i .. ]) ^ seed,
                read_8bytes_swapped(key[ i + 8 ..]) ^ primes[2]
            ) ^
                mum(read_8bytes_swapped(key[ i + 16 .. ]) ^ seed, rest),
        };
    }

    return mum(seed, len ^ primes[5]);
}

test "test hash" {
    const fst_key : []const u8 = &[1]u8 { 0 };
    const fst_result : u64 = hash(fst_key, 0);
    const fst_expectation : u64 = 10120618241204775652;

    expectEqual(fst_expectation, fst_result);

    const snd_key : []const u8 = &[1]u8 { 1 };
    const snd_result : u64 = hash(snd_key, 0);
    const snd_expectation : u64 = 16604119901607610318;

    expectEqual(snd_expectation, snd_result);

    const thd_key : []const u8 = "";
    const thd_result : u64 = hash(thd_key, 0);
    const thd_expectation : u64 = 17969918002310452037;

    expectEqual(thd_expectation, thd_result);

    const frth_key : []const u8 = &[42]u8 {
        1,1,1,1,1,1,
        2,2,2,2,2,2,
        3,3,3,3,3,3,
        4,4,4,4,4,4,
        5,5,5,5,5,5,
        6,6,6,6,6,6,
        7,7,7,7,7,7
    };
    const frth_result : u64 = hash(frth_key, 0);
    const frth_expectation : u64 = 10505276342277112336;

    expectEqual(frth_expectation, frth_result);
}

//test "Render Indices" {
//    const print = std.debug.print;
//
//    const limit = 32;
//
//    var i : u8 = 0;
//
//    while (i < (2 * limit)) {
//        print("Index {} hashes to {}\n", .{ i, hash(std.mem.asBytes(&i), 0) % limit});
//        i += 1;
//    }
//}