#include <cstdint>
#include <cstring>

struct Slice {
    const char* ptr;
    uint64_t len;
};

static inline bool starts_with(const char* p, uint64_t n, const char* lit, uint64_t lit_n) {
    return n >= lit_n && __builtin_memcmp(p, lit, lit_n) == 0;
}

struct Word128 {
    uint64_t lo;
    uint64_t hi;
};

static inline uint16_t load16(const char* p) {
    uint16_t out;
    __builtin_memcpy(&out, p, sizeof(out));
    return out;
}

static inline uint32_t load32(const char* p) {
    uint32_t out;
    __builtin_memcpy(&out, p, sizeof(out));
    return out;
}

static inline uint64_t load64(const char* p) {
    uint64_t out;
    __builtin_memcpy(&out, p, sizeof(out));
    return out;
}

static inline Word128 load128(const char* p) {
    Word128 out;
    __builtin_memcpy(&out, p, sizeof(out));
    return out;
}

static inline constexpr uint16_t le16(char a, char b) {
    return static_cast<uint16_t>(static_cast<unsigned char>(a)) |
        (static_cast<uint16_t>(static_cast<unsigned char>(b)) << 8);
}

static inline constexpr uint32_t le32(char a, char b, char c, char d) {
    return static_cast<uint32_t>(le16(a, b)) |
        (static_cast<uint32_t>(le16(c, d)) << 16);
}

static inline constexpr uint64_t le64(
    char a, char b, char c, char d, char e, char f, char g, char h
) {
    return static_cast<uint64_t>(le32(a, b, c, d)) |
        (static_cast<uint64_t>(le32(e, f, g, h)) << 32);
}

static inline bool eq2(const char* p, uint16_t lit) {
    return load16(p) == lit;
}

static inline bool eq3(const char* p, uint16_t first2, char third) {
    return load16(p) == first2 && static_cast<unsigned char>(p[2]) == static_cast<unsigned char>(third);
}

static inline bool eq4(const char* p, uint32_t lit) {
    return load32(p) == lit;
}

static inline bool eq5(const char* p, uint32_t first4, char fifth) {
    return load32(p) == first4 && static_cast<unsigned char>(p[4]) == static_cast<unsigned char>(fifth);
}

static inline bool eq6(const char* p, uint32_t first4, uint16_t last2) {
    return load32(p) == first4 && load16(p + 4) == last2;
}

static inline bool eq7(const char* p, uint32_t first4, uint16_t next2, char last) {
    return load32(p) == first4 && load16(p + 4) == next2 &&
        static_cast<unsigned char>(p[6]) == static_cast<unsigned char>(last);
}

static inline bool eq8(const char* p, uint64_t lit) {
    return load64(p) == lit;
}

static inline bool eq16(const char* p, uint64_t lo, uint64_t hi) {
    const Word128 word = load128(p);
    return word.lo == lo && word.hi == hi;
}

static inline uint64_t byte_mask(uint64_t word, unsigned char byte) {
    const uint64_t repeated = static_cast<uint64_t>(byte) * 0x0101010101010101ULL;
    const uint64_t x = word ^ repeated;
    return (x - 0x0101010101010101ULL) & ~x & 0x8080808080808080ULL;
}

static inline const char* find_byte(const char* p, const char* end, unsigned char byte) {
    while (static_cast<uint64_t>(end - p) >= 16) {
        const uint64_t lo_mask = byte_mask(load64(p), byte);
        const uint64_t hi_mask = byte_mask(load64(p + 8), byte);
        if (lo_mask != 0) return p + (__builtin_ctzll(lo_mask) >> 3);
        if (hi_mask != 0) return p + 8 + (__builtin_ctzll(hi_mask) >> 3);
        p += 16;
    }

    while (p < end) {
        if (static_cast<unsigned char>(*p) == byte) return p;
        ++p;
    }
    return nullptr;
}

extern "C" __attribute__((noinline))
uint64_t discard_only(const char* p, uint64_t n) {
    if (n >= 5 && eq5(p, le32('G', 'E', 'T', ' '), '/')) {
        const char* cur = p + 5;
        const char* end = p + n;
        const char* dot = find_byte(cur, end, '.');
        if (dot != nullptr && static_cast<uint64_t>(end - dot) == 4 &&
            eq4(dot, le32('.', 't', 'x', 't'))) {
            return 1;
        }
    }

    const char* end = p + n;
    const char* dot = find_byte(p, end, '.');
    if (dot != nullptr && static_cast<uint64_t>(end - dot) == 5 &&
        eq5(dot, le32('.', 'j', 's', 'o'), 'n')) {
        return 2;
    }

    return 0;
}

extern "C" __attribute__((noinline))
uint64_t one_capture(const char* p, uint64_t n) {
    if (n < 5 || !eq5(p, le32('G', 'E', 'T', ' '), '/')) return 0;
    const char* cur = p + 5;
    const char* end = p + n;
    const char* dot = find_byte(cur, end, '.');
    if (dot == nullptr) return 0;
    if (static_cast<uint64_t>(end - dot) != 4) return 0;
    if (!eq4(dot, le32('.', 't', 'x', 't'))) return 0;
    return static_cast<uint64_t>(dot - cur) + 10;
}

extern "C" __attribute__((noinline))
uint64_t two_capture(const char* p, uint64_t n) {
    if (n < 3 || !eq3(p, le16('f', 'o'), 'o')) return 0;
    const char* cur = p + 3;
    const char* end = p + n;
    const char* b = find_byte(cur, end, 'b');
    if (b == nullptr) return 0;
    if (static_cast<uint64_t>(end - b) < 3) return 0;
    if (!eq3(b, le16('b', 'a'), 'z')) return 0;
    const char* after_baz = b + 3;
    const char* e = find_byte(after_baz, end, 'e');
    if (e == nullptr) return 0;
    if (static_cast<uint64_t>(end - e) != 3) return 0;
    if (!eq3(e, le16('e', 't'), 'c')) return 0;
    return static_cast<uint64_t>(b - cur) + static_cast<uint64_t>(e - after_baz) * 3 + 11;
}

extern "C" __attribute__((noinline))
uint64_t branchy(const char* p, uint64_t n) {
    const char* end = p + n;

    if (n >= 6 && eq6(p, le32('a', 'l', 'p', 'h'), le16('a', ':'))) {
        const char* a_start = p + 6;
        const char* semi = find_byte(a_start, end, ';');
        if (semi != nullptr && static_cast<uint64_t>(end - semi) >= 10 &&
            eq6(semi, le32(';', 'b', 'e', 't'), le16('a', ':'))) {
            const char* b_start = semi + 6;
            const char* end_delim = find_byte(b_start, end, ';');
            if (end_delim != nullptr && static_cast<uint64_t>(end - end_delim) == 4 &&
                eq4(end_delim, le32(';', 'e', 'n', 'd'))) {
                return static_cast<uint64_t>(semi - a_start) + static_cast<uint64_t>(end_delim - b_start) + 100;
            }
        }
    }

    const char* dot = find_byte(p, end, '.');
    if (dot != nullptr && static_cast<uint64_t>(end - dot) == 5 &&
        eq5(dot, le32('.', 'j', 's', 'o'), 'n')) {
        return static_cast<uint64_t>(dot - p) + 200;
    }

    if (n >= 4 && eq4(p, le32('l', 'o', 'g', ':'))) {
        const char* cur = p + 4;
        const char* colon = find_byte(cur, end, ':');
        if (colon != nullptr && static_cast<uint64_t>(end - colon) >= 7 &&
            eq7(colon, le32(':', 'l', 'e', 'v'), le16('e', 'l'), ':')) {
            const char* level_start = colon + 7;
            return static_cast<uint64_t>(end - level_start) + 300;
        }
    }

    return 0;
}

extern "C" __attribute__((noinline))
uint64_t long_literal(const char* p, uint64_t n) {
    if (n < 16 || !eq16(p,
        le64('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
        le64('i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'))) return 0;
    const char* cur = p + 16;
    const char* end = p + n;
    const char* q = find_byte(cur, end, 'q');
    if (q == nullptr) return 0;
    if (static_cast<uint64_t>(end - q) < 16) return 0;
    if (!eq16(q,
        le64('q', 'r', 's', 't', 'u', 'v', 'w', 'x'),
        le64('y', 'z', '0', '1', '2', '3', '4', '5'))) return 0;
    return static_cast<uint64_t>(q - cur) + 400;
}

extern "C" __attribute__((noinline))
uint64_t width_literals(const char* p, uint64_t n) {
    const char* end = p + n;

    if (n >= 2 && eq2(p, le16('a', 'b'))) {
        const char* cur = p + 2;
        const char* c = find_byte(cur, end, 'c');
        if (c != nullptr && static_cast<uint64_t>(end - c) == 2 &&
            eq2(c, le16('c', 'd'))) {
            return static_cast<uint64_t>(c - cur) + 2;
        }
    }

    if (n >= 8 && eq8(p, le64('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'))) {
        const char* cur = p + 8;
        const char* i = find_byte(cur, end, 'i');
        if (i != nullptr && static_cast<uint64_t>(end - i) == 8 &&
            eq8(i, le64('i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'))) {
            return static_cast<uint64_t>(i - cur) + 8;
        }
    }

    if (n >= 16 && eq16(p,
        le64('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
        le64('i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'))) {
        const char* cur = p + 16;
        const char* q = find_byte(cur, end, 'q');
        if (q != nullptr && static_cast<uint64_t>(end - q) == 16 &&
            eq16(q,
                le64('q', 'r', 's', 't', 'u', 'v', 'w', 'x'),
                le64('y', 'z', '0', '1', '2', '3', '4', '5'))) {
            return static_cast<uint64_t>(q - cur) + 16;
        }
    }

    return 0;
}
