// Ported from musl, which is licensed under the MIT license:
// https://git.musl-libc.org/cgit/musl/tree/COPYRIGHT
//
// https://git.musl-libc.org/cgit/musl/tree/src/math/__cos.c
// https://git.musl-libc.org/cgit/musl/tree/src/math/__cosdf.c
// https://git.musl-libc.org/cgit/musl/tree/src/math/__sin.c
// https://git.musl-libc.org/cgit/musl/tree/src/math/__sindf.c
// https://git.musl-libc.org/cgit/musl/tree/src/math/__tand.c
// https://git.musl-libc.org/cgit/musl/tree/src/math/__tandf.c
// https://git.musl-libc.org/cgit/musl/tree/src/math/__sinl.c
// https://git.musl-libc.org/cgit/musl/tree/src/math/__cosl.c
// https://git.musl-libc.org/cgit/musl/tree/src/math/__tanl.c

const std = @import("std");

pub const pi_4 = std.math.pi / 4.0;

/// kernel cos function on [-pi/4, pi/4], pi/4 ~ 0.785398164
/// Input x is assumed to be bounded by ~pi/4 in magnitude.
/// Input y is the tail of x.
///
/// Algorithm
///      1. Since cos(-x) = cos(x), we need only to consider positive x.
///      2. if x < 2^-27 (hx<0x3e400000 0), return 1 with inexact if x!=0.
///      3. cos(x) is approximated by a polynomial of degree 14 on
///         [0,pi/4]
///                                       4            14
///              cos(x) ~ 1 - x*x/2 + C1*x + ... + C6*x
///         where the remez error is
///
///      |              2     4     6     8     10    12     14 |     -58
///      |cos(x)-(1-.5*x +C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  )| <= 2
///      |                                                      |
///
///                     4     6     8     10    12     14
///      4. let r = C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  , then
///             cos(x) ~ 1 - x*x/2 + r
///         since cos(x+y) ~ cos(x) - sin(x)*y
///                        ~ cos(x) - x*y,
///         a correction term is necessary in cos(x) and hence
///              cos(x+y) = 1 - (x*x/2 - (r - x*y))
///         For better accuracy, rearrange to
///              cos(x+y) ~ w + (tmp + (r-x*y))
///         where w = 1 - x*x/2 and tmp is a tiny correction term
///         (1 - x*x/2 == w + tmp exactly in infinite precision).
///         The exactness of w + tmp in infinite precision depends on w
///         and tmp having the same precision as x.  If they have extra
///         precision due to compiler bugs, then the extra precision is
///         only good provided it is retained in all terms of the final
///         expression for cos().  Retention happens in all cases tested
///         under FreeBSD, so don't pessimize things by forcibly clipping
///         any extra precision in w.
pub fn cos(x: f64, y: f64) f64 {
    const C1 = 4.16666666666666019037e-02; // 0x3FA55555, 0x5555554C
    const C2 = -1.38888888888741095749e-03; // 0xBF56C16C, 0x16C15177
    const C3 = 2.48015872894767294178e-05; // 0x3EFA01A0, 0x19CB1590
    const C4 = -2.75573143513906633035e-07; // 0xBE927E4F, 0x809C52AD
    const C5 = 2.08757232129817482790e-09; // 0x3E21EE9E, 0xBDB4B1C4
    const C6 = -1.13596475577881948265e-11; // 0xBDA8FAE9, 0xBE8838D4

    const z = x * x;
    const zs = z * z;
    const r = z * (C1 + z * (C2 + z * C3)) + zs * zs * (C4 + z * (C5 + z * C6));
    const hz = 0.5 * z;
    const w = 1.0 - hz;
    return w + (((1.0 - w) - hz) + (z * r - x * y));
}

pub fn cosdf(x: f64) f32 {
    // |cos(x) - c(x)| < 2**-34.1 (~[-5.37e-11, 5.295e-11]).
    const C0 = -0x1ffffffd0c5e81.0p-54; // -0.499999997251031003120
    const C1 = 0x155553e1053a42.0p-57; //  0.0416666233237390631894
    const C2 = -0x16c087e80f1e27.0p-62; // -0.00138867637746099294692
    const C3 = 0x199342e0ee5069.0p-68; //  0.0000243904487962774090654

    // Try to optimize for parallel evaluation as in __tandf.c.
    const z = x * x;
    const w = z * z;
    const r = C2 + z * C3;
    return @floatCast(((1.0 + z * C0) + w * C1) + (w * z) * r);
}

pub fn cosx(x: f80, y: f80) f80 {
    const C1: f80 = 0.0416666666666666666136;
    const C2: f64 = -0.0013888888888888874;
    const C3: f64 = 0.000024801587301571716;
    const C4: f64 = -0.00000027557319215507120;
    const C5: f64 = 0.0000000020876754400407278;
    const C6: f64 = -1.1470297442401303e-11;
    const C7: f64 = 4.7383039476436467e-14;

    const z = x * x;
    const r = z * (C1 + z * (C2 + z * (C3 + z * (C4 +
        z * (C5 + z * (C6 + z * C7))))));
    const hz = 0.5 * z;
    const w = 1.0 - hz;

    return w + (((1.0 - w) - hz) + (z * r - x * y));
}

pub fn cosq(x: f128, y: f128) f128 {
    const C1: f128 = 0.04166666666666666666666666666666658424671;
    const C2: f128 = -0.001388888888888888888888888888863490893732;
    const C3: f128 = 0.00002480158730158730158730158600795304914210;
    const C4: f128 = -0.2755731922398589065255474947078934284324e-6;
    const C5: f128 = 0.2087675698786809897659225313136400793948e-8;
    const C6: f128 = -0.1147074559772972315817149986812031204775e-10;
    const C7: f128 = 0.4779477332386808976875457937252120293400e-13;
    const C8: f64 = -0.1561920696721507929516718307820958119868e-15;
    const C9: f64 = 0.4110317413744594971475941557607804508039e-18;
    const C10: f64 = -0.8896592467191938803288521958313920156409e-21;
    const C11: f64 = 0.1601061435794535138244346256065192782581e-23;

    const z = x * x;
    const r = z * (C1 + z * (C2 + z * (C3 + z * (C4 + z * (C5 + z * (C6 +
        z * (C7 + z * (C8 + z * (C9 + z * (C10 + z * C11))))))))));
    const hz = 0.5 * z;
    const w = 1.0 - hz;

    return w + (((1.0 - w) - hz) + (z * r - x * y));
}

/// kernel sin function on ~[-pi/4, pi/4] (except on -0), pi/4 ~ 0.7854
/// Input x is assumed to be bounded by ~pi/4 in magnitude.
/// Input y is the tail of x.
/// Input iy indicates whether y is 0. (if iy=0, y assume to be 0).
///
/// Algorithm
///      1. Since sin(-x) = -sin(x), we need only to consider positive x.
///      2. Callers must return sin(-0) = -0 without calling here since our
///         odd polynomial is not evaluated in a way that preserves -0.
///         Callers may do the optimization sin(x) ~ x for tiny x.
///      3. sin(x) is approximated by a polynomial of degree 13 on
///         [0,pi/4]
///                               3            13
///              sin(x) ~ x + S1*x + ... + S6*x
///         where
///
///      |sin(x)         2     4     6     8     10     12  |     -58
///      |----- - (1+S1*x +S2*x +S3*x +S4*x +S5*x  +S6*x   )| <= 2
///      |  x                                               |
///
///      4. sin(x+y) = sin(x) + sin'(x')*y
///                  ~ sin(x) + (1-x*x/2)*y
///         For better accuracy, let
///                   3      2      2      2      2
///              r = x *(S2+x *(S3+x *(S4+x *(S5+x *S6))))
///         then                   3    2
///              sin(x) = x + (S1*x + (x *(r-y/2)+y))
pub fn sin(x: f64, y: f64, iy: i32) f64 {
    const S1 = -1.66666666666666324348e-01; // 0xBFC55555, 0x55555549
    const S2 = 8.33333333332248946124e-03; // 0x3F811111, 0x1110F8A6
    const S3 = -1.98412698298579493134e-04; // 0xBF2A01A0, 0x19C161D5
    const S4 = 2.75573137070700676789e-06; // 0x3EC71DE3, 0x57B1FE7D
    const S5 = -2.50507602534068634195e-08; // 0xBE5AE5E6, 0x8A2B9CEB
    const S6 = 1.58969099521155010221e-10; // 0x3DE5D93A, 0x5ACFD57C

    const z = x * x;
    const w = z * z;
    const r = S2 + z * (S3 + z * S4) + z * w * (S5 + z * S6);
    const v = z * x;
    if (iy == 0) {
        return x + v * (S1 + z * r);
    } else {
        return x - ((z * (0.5 * y - v * r) - y) - v * S1);
    }
}

pub fn sindf(x: f64) f32 {
    // |sin(x)/x - s(x)| < 2**-37.5 (~[-4.89e-12, 4.824e-12]).
    const S1 = -0x15555554cbac77.0p-55; // -0.166666666416265235595
    const S2 = 0x111110896efbb2.0p-59; //  0.0083333293858894631756
    const S3 = -0x1a00f9e2cae774.0p-65; // -0.000198393348360966317347
    const S4 = 0x16cd878c3b46a7.0p-71; //  0.0000027183114939898219064

    // Try to optimize for parallel evaluation as in __tandf.c.
    const z = x * x;
    const w = z * z;
    const r = S3 + z * S4;
    const s = z * x;
    return @floatCast((x + s * (S1 + z * S2)) + s * w * r);
}

pub fn sinx(x: f80, y: f80, iy: i32) f80 {
    const S1: f80 = -0.166666666666666666671;
    const S2: f64 = 0.0083333333333333332;
    const S3: f64 = -0.00019841269841269427;
    const S4: f64 = 0.0000027557319223597490;
    const S5: f64 = -0.000000025052108218074604;
    const S6: f64 = 1.6059006598854211e-10;
    const S7: f64 = -7.6429779983024564e-13;
    const S8: f64 = 2.6174587166648325e-15;

    const z = x * x;
    const v = z * x;
    const r = S2 + z * (S3 + z * (S4 + z * (S5 +
        z * (S6 + z * (S7 + z * S8)))));

    if (iy == 0)
        return x + v * (S1 + z * r);

    return x - ((z * (0.5 * y - v * r) - y) - v * S1);
}

pub fn sinq(x: f128, y: f128, iy: i32) f128 {
    const S1: f128 = -0.16666666666666666666666666666666666606732416116558;
    const S2: f128 = 0.0083333333333333333333333333333331135404851288270047;
    const S3: f128 = -0.00019841269841269841269841269839935785325638310428717;
    const S4: f128 = 0.27557319223985890652557316053039946268333231205686e-5;
    const S5: f128 = -0.25052108385441718775048214826384312253862930064745e-7;
    const S6: f128 = 0.16059043836821614596571832194524392581082444805729e-9;
    const S7: f128 = -0.76471637318198151807063387954939213287488216303768e-12;
    const S8: f128 = 0.28114572543451292625024967174638477283187397621303e-14;
    const S9: f64 = -0.82206352458348947812512122163446202498005154296863e-17;
    const S10: f64 = 0.19572940011906109418080609928334380560135358385256e-19;
    const S11: f64 = -0.38680813379701966970673724299207480965452616911420e-22;
    const S12: f64 = 0.64038150078671872796678569586315881020659912139412e-25;

    const z = x * x;
    const v = z * x;
    const r = S2 + z * (S3 + z * (S4 + z * (S5 + z * (S6 + z * (S7 + z * (S8 +
        z * (S9 + z * (S10 + z * (S11 + z * S12)))))))));

    if (iy == 0)
        return x + v * (S1 + z * r);

    return x - ((z * (0.5 * y - v * r) - y) - v * S1);
}

/// kernel tan function on ~[-pi/4, pi/4] (except on -0), pi/4 ~ 0.7854
/// Input x is assumed to be bounded by ~pi/4 in magnitude.
/// Input y is the tail of x.
/// Input odd indicates whether tan (if odd = 0) or -1/tan (if odd = 1) is returned.
///
/// Algorithm
///      1. Since tan(-x) = -tan(x), we need only to consider positive x.
///      2. Callers must return tan(-0) = -0 without calling here since our
///         odd polynomial is not evaluated in a way that preserves -0.
///         Callers may do the optimization tan(x) ~ x for tiny x.
///      3. tan(x) is approximated by a odd polynomial of degree 27 on
///         [0,0.67434]
///                               3             27
///              tan(x) ~ x + T1*x + ... + T13*x
///         where
///
///              |tan(x)         2     4            26   |     -59.2
///              |----- - (1+T1*x +T2*x +.... +T13*x    )| <= 2
///              |  x                                    |
///
///         Note: tan(x+y) = tan(x) + tan'(x)*y
///                        ~ tan(x) + (1+x*x)*y
///         Therefore, for better accuracy in computing tan(x+y), let
///                   3      2      2       2       2
///              r = x *(T2+x *(T3+x *(...+x *(T12+x *T13))))
///         then
///                                  3    2
///              tan(x+y) = x + (T1*x + (x *(r+y)+y))
///
///      4. For x in [0.67434,pi/4],  let y = pi/4 - x, then
///              tan(x) = tan(pi/4-y) = (1-tan(y))/(1+tan(y))
///                     = 1 - 2*(tan(y) - (tan(y)^2)/(1+tan(y)))
pub fn tan(x_: f64, y_: f64, odd: bool) f64 {
    var x = x_;
    var y = y_;

    const T = [_]f64{
        3.33333333333334091986e-01, // 3FD55555, 55555563
        1.33333333333201242699e-01, // 3FC11111, 1110FE7A
        5.39682539762260521377e-02, // 3FABA1BA, 1BB341FE
        2.18694882948595424599e-02, // 3F9664F4, 8406D637
        8.86323982359930005737e-03, // 3F8226E3, E96E8493
        3.59207910759131235356e-03, // 3F6D6D22, C9560328
        1.45620945432529025516e-03, // 3F57DBC8, FEE08315
        5.88041240820264096874e-04, // 3F4344D8, F2F26501
        2.46463134818469906812e-04, // 3F3026F7, 1A8D1068
        7.81794442939557092300e-05, // 3F147E88, A03792A6
        7.14072491382608190305e-05, // 3F12B80F, 32F0A7E9
        -1.85586374855275456654e-05, // BEF375CB, DB605373
        2.59073051863633712884e-05, // 3EFB2A70, 74BF7AD4
    };
    const pio4 = 7.85398163397448278999e-01; // 3FE921FB, 54442D18
    const pio4lo = 3.06161699786838301793e-17; // 3C81A626, 33145C07

    var z: f64 = undefined;
    var r: f64 = undefined;
    var v: f64 = undefined;
    var w: f64 = undefined;
    var s: f64 = undefined;
    var a: f64 = undefined;
    var w0: f64 = undefined;
    var a0: f64 = undefined;
    var hx: u32 = undefined;
    var sign: bool = undefined;

    hx = @intCast(@as(u64, @bitCast(x)) >> 32);
    const big = (hx & 0x7fffffff) >= 0x3FE59428; // |x| >= 0.6744
    if (big) {
        sign = hx >> 31 != 0;
        if (sign) {
            x = -x;
            y = -y;
        }
        x = (pio4 - x) + (pio4lo - y);
        y = 0.0;
    }
    z = x * x;
    w = z * z;

    // Break x^5*(T[1]+x^2*T[2]+...) into
    // x^5(T[1]+x^4*T[3]+...+x^20*T[11]) +
    // x^5(x^2*(T[2]+x^4*T[4]+...+x^22*[T12]))
    r = T[1] + w * (T[3] + w * (T[5] + w * (T[7] + w * (T[9] + w * T[11]))));
    v = z * (T[2] + w * (T[4] + w * (T[6] + w * (T[8] + w * (T[10] + w * T[12])))));
    s = z * x;
    r = y + z * (s * (r + v) + y) + s * T[0];
    w = x + r;
    if (big) {
        s = @floatFromInt(1 - 2 * @as(i3, @intFromBool(odd)));
        v = s - 2.0 * (x + (r - w * w / (w + s)));
        return if (sign) -v else v;
    }
    if (!odd) {
        return w;
    }
    // -1.0/(x+r) has up to 2ulp error, so compute it accurately
    w0 = w;
    w0 = @bitCast(@as(u64, @bitCast(w0)) & 0xffffffff00000000);
    v = r - (w0 - x); // w0+v = r+x
    a = -1.0 / w;
    a0 = a;
    a0 = @bitCast(@as(u64, @bitCast(a0)) & 0xffffffff00000000);
    return a0 + a * (1.0 + a0 * w0 + a0 * v);
}

pub fn tandf(x: f64, odd: bool) f32 {
    // |tan(x)/x - t(x)| < 2**-25.5 (~[-2e-08, 2e-08]).
    const T = [_]f64{
        0x15554d3418c99f.0p-54, // 0.333331395030791399758
        0x1112fd38999f72.0p-55, // 0.133392002712976742718
        0x1b54c91d865afe.0p-57, // 0.0533812378445670393523
        0x191df3908c33ce.0p-58, // 0.0245283181166547278873
        0x185dadfcecf44e.0p-61, // 0.00297435743359967304927
        0x1362b9bf971bcd.0p-59, // 0.00946564784943673166728
    };

    const z = x * x;
    // Split up the polynomial into small independent terms to give
    // opportunities for parallel evaluation.  The chosen splitting is
    // micro-optimized for Athlons (XP, X64).  It costs 2 multiplications
    // relative to Horner's method on sequential machines.
    //
    // We add the small terms from lowest degree up for efficiency on
    // non-sequential machines (the lowest degree terms tend to be ready
    // earlier).  Apart from this, we don't care about order of
    // operations, and don't need to to care since we have precision to
    // spare.  However, the chosen splitting is good for accuracy too,
    // and would give results as accurate as Horner's method if the
    // small terms were added from highest degree down.
    const r = T[4] + z * T[5];
    const t = T[2] + z * T[3];
    const w = z * z;
    const s = z * x;
    const u = T[0] + z * T[1];
    const r0 = (x + s * u) + (s * w) * (t + w * r);
    return @floatCast(if (odd) -1.0 / r0 else r0);
}

pub fn tanx(x_: f80, y_: f80, odd: i32) f80 {
    const pio4: f80 = 0.785398163397448309628;
    const pio4lo: f80 = -1.25413940316708300586e-20;

    const T3: f80 = 0.333333333333333333180;
    const T5: f80 = 0.133333333333333372290;
    const T7: f80 = 0.0539682539682504975744;
    const T9: f64 = 0.021869488536312216;
    const T11: f64 = 0.0088632355256619590;
    const T13: f64 = 0.0035921281113786528;
    const T15: f64 = 0.0014558334756312418;
    const T17: f64 = 0.00059003538700862256;
    const T19: f64 = 0.00023907843576635544;
    const T21: f64 = 0.000097154625656538905;
    const T23: f64 = 0.000038440165747303162;
    const T25: f64 = 0.000018082171885432524;
    const T27: f64 = 0.0000024196006108814377;
    const T29: f64 = 0.0000078293456938132840;
    const T31: f64 = -0.0000032609076735050182;
    const T33: f64 = 0.0000023261313142559411;

    var x = x_;
    var y = y_;
    const big = @abs(x) >= 0.67434;
    var sign: i8 = 0;

    if (big) {
        if (x < 0) {
            sign = -1;
            x = -x;
            y = -y;
        }
        x = (pio4 - x) + (pio4lo - y);
        y = 0.0;
    }

    var z = x * x;
    var w = z * z;

    var r = T5 + w * (T9 + w * (T13 + w * (T17 + w * (T21 +
        w * (T25 + w * (T29 + w * T33))))));

    var v = z * (T7 + w * (T11 + w * (T15 + w * (T19 + w * (T23 +
        w * (T27 + w * T31))))));

    var s = z * x;
    r = y + z * (s * (r + v) + y) + T3 * s;
    w = x + r;

    if (big) {
        s = @as(f80, @floatFromInt(1 - 2 * odd));
        v = s - 2.0 * (x + (r - w * w / (w + s)));
        return if (sign == -1) -v else v;
    }

    if (odd == 0) {
        return w;
    }

    // if allow error up to 2 ulp, simply return
    // -1.0 / (x+r) here
    //
    // compute -1.0 / (x+r) accurately
    z = w + 0x1p32 - 0x1p32;
    v = r - (z - x);
    const a = -1.0 / w;
    const t = a + 0x1p32 - 0x1p32;
    s = 1.0 + t * z;
    return t + a * (s + t * v);
}

pub fn tanq(x_: f128, y_: f128, odd: i32) f128 {
    const pio4: f128 = 0x1.921fb54442d18469898cc51701b8p-1;
    const pio4lo: f128 = 0x1.cd129024e088a67cc74020bbea60p-116;

    const T3: f128 = 0x1.5555555555555555555555555553p-2;
    const T5: f128 = 0x1.1111111111111111111111111eb5p-3;
    const T7: f128 = 0x1.ba1ba1ba1ba1ba1ba1ba1b694cd6p-5;
    const T9: f128 = 0x1.664f4882c10f9f32d6bbe09d8bcdp-6;
    const T11: f128 = 0x1.226e355e6c23c8f5b4f5762322eep-7;
    const T13: f128 = 0x1.d6d3d0e157ddfb5fed8e84e27b37p-9;
    const T15: f128 = 0x1.7da36452b75e2b5fce9ee7c2c92ep-10;
    const T17: f128 = 0x1.355824803674477dfcf726649efep-11;
    const T19: f128 = 0x1.f57d7734d1656e0aceb716f614c2p-13;
    const T21: f128 = 0x1.967e18afcb180ed942dfdc518d6cp-14;
    const T23: f128 = 0x1.497d8eea21e95bc7e2aa79b9f2cdp-15;
    const T25: f128 = 0x1.0b132d39f055c81be49eff7afd50p-16;
    const T27: f128 = 0x1.b0f72d33eff7bfa2fbc1059d90b6p-18;
    const T29: f128 = 0x1.5ef2daf21d1113df38d0fbc00267p-19;
    const T31: f128 = 0x1.1c77d6eac0234988cdaa04c96626p-20;
    const T33: f128 = 0x1.cd2a5a292b180e0bdd701057dfe3p-22;
    const T35: f128 = 0x1.75c7357d0298c01a31d0a6f7d518p-23;
    const T37: f128 = 0x1.2f3190f4718a9a520f98f50081fcp-24;
    const T39: f64 = 0.000000028443389121318352;
    const T41: f64 = 0.000000011981013102001973;
    const T43: f64 = 0.0000000038303578044958070;
    const T45: f64 = 0.0000000034664378216909893;
    const T47: f64 = -0.0000000015090641701997785;
    const T49: f64 = 0.0000000029449552300483952;
    const T51: f64 = -0.0000000022006995706097711;
    const T53: f64 = 0.0000000015468200913196612;
    const T55: f64 = -0.00000000061311613386849674;
    const T57: f64 = 1.4912469681508012e-10;

    var x = x_;
    var y = y_;

    const big = @abs(x) >= 0.67434;
    var sign: i8 = 0;

    if (big) {
        if (x < 0) {
            sign = -1;
            x = -x;
            y = -y;
        }
        x = (pio4 - x) + (pio4lo - y);
        y = 0.0;
    }

    var z = x * x;
    var w = z * z;

    var r = T5 + w * (T9 + w * (T13 + w * (T17 + w * (T21 +
        w * (T25 + w * (T29 + w * (T33 + w * (T37 + w * (T41 +
            w * (T45 + w * (T49 + w * (T53 + w * T57))))))))))));

    var v = z * (T7 + w * (T11 + w * (T15 + w * (T19 + w * (T23 +
        w * (T27 + w * (T31 + w * (T35 + w * (T39 + w * (T43 +
            w * (T47 + w * (T51 + w * T55))))))))))));

    var s = z * x;
    r = y + z * (s * (r + v) + y) + T3 * s;
    w = x + r;

    if (big) {
        s = @as(f128, @floatFromInt(1 - 2 * odd));
        v = s - 2.0 * (x + (r - w * w / (w + s)));
        return if (sign == -1) -v else v;
    }

    if (odd == 0) {
        return w;
    }

    // if allow error up to 2 ulp, simply return
    // -1.0 / (x+r) here
    //
    // compute -1.0 / (x+r) accurately
    z = w + 0x1p32 - 0x1p32;
    v = r - (z - x);
    const a = -1.0 / w;
    const t = a + 0x1p32 - 0x1p32;
    s = 1.0 + t * z;
    return t + a * (s + t * v);
}
