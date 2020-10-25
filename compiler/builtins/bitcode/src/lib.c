#include <math.h>
#include <stdbool.h>

// float     -> f32
// double    -> f64
// int       -> i16
// long int  -> i32
// long long -> i64

bool is_finite_(double num) { return isfinite(num); }

double atan_(double num) { return atan(num); }

long long pow_int_(long long base, long long exp) {
  int acc = 1;

  while (exp > 1) {
    if ((exp & 1) == 1) {
      acc *= base;
    }
    exp /= 2;
    base *= base;
  }

  // Deal with the final bit of the exponent separately, since
  // squaring the base afterwards is not necessary and may cause a
  // needless overflow.
  if (exp == 1) {
    acc *= base;
  }

  return acc;
}
