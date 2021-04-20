
//
// header for a C implementation of arithmetic in GF(p^m) via precomputed
// tables of Zech's logarithms.
//
// This is here in case if you want to do arithmetic in fixed fields 
// (the tables can be exported by the Haskell library) in a C program.
//
// Implementation note: 
//
// field elements are represented by integers from the interval [-1...q-2]:
//  * -1 corresponds to 0
//  *  0 corresponds to 1
//  *  1 corresponds to g
//  *  k corresponds to g^k
//

#ifndef _ZECH_H_INCLUDED_

#include <stdint.h>
#include <stdbool.h>

// struct ZechTable {
//   int32_t prime;
//   int32_t dim;
//   int32_t q_minus_1;
//   int32_t log_minus_1;
//   int32_t embeds[];         // a flat array of int32-s of length p
//   int32_t zech_logs[];      // a flat array of int32-s of length (q-1)
// };

// typedef struct ZechTable *Zech;

typedef int32_t* Zech;
typedef int32_t  GF;

GF zech_neg(Zech, GF);
GF zech_add(Zech, GF, GF);
GF zech_sub(Zech, GF, GF);

GF zech_inv(Zech, GF);
GF zech_mul(Zech, GF, GF);
GF zech_div(Zech, GF, GF);
GF zech_pow(Zech, GF, int);

// embedding of the prime field (represented by 0..p-1) 
GF zech_embed(Zech, int);

// enumerate all field elements
int zech_enumerate(Zech, GF*);

static inline GF zech_zero(GF x) { return -1; }
static inline GF zech_one (GF x) { return  0; }
static inline GF zech_prim()     { return  1; }

static inline bool zech_is_zero(GF x) { return (x == -1); }
static inline bool zech_is_one (GF x) { return (x ==  0); }

#endif // _ZECH_H_INCLUDED_

