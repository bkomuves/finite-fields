
//
// C implementation of arithmetic in GF(p^m) via precomputed 
// tables of Zech's logarithms.
//
// This is here in case if you want to do arithmetic in fixed fields 
// (the tables can be exported by the Haskell library) in a C program.
//

#include "zech.h"

// struct ZechTable {
//   uint32_t prime;
//   uint32_t dim;
//   uint32_t q_minus_1;
//   uint32_t log_minus_1;
//   uint32_t embeds[];
//   uint32_t zech_logs[];
// };

#define PRIME(zech)     zech[0]
#define DIM(zech)       zech[1]
#define QMINUS1(zech)   zech[2]
#define LOGMINUS1(zech) zech[3]

#define EMBEDS(zech)    (zech + 4          )
#define ZECH_LOGS(zech) (zech + 4 + zech[0])

GF zech_neg(Zech zech , GF x) {
  if (x == -1) return x; else {
    int32_t n = QMINUS1(zech);
    int32_t c = x + LOGMINUS1(zech);
    return ( (c<n) ? c : (c-n) );
  }
}

GF zech_add(Zech zech, GF x, GF y) {
  if (x==-1) return y;
  if (y==-1) return x;
  int32_t n = QMINUS1(zech);
  int32_t *zech_logs = ZECH_LOGS(zech);
  if (x >= y) {
    int32_t d = zech_logs[x-y];
    if (d<0) return d; else {
      int32_t c = y + d;
      return ( (c<n) ? c : (c-n) );
    }
  }
  else {
    int32_t d = zech_logs[y-x];
    if (d<0) return d; else {
      int32_t c = x + d;
      return ( (c<n) ? c : (c-n) );
    }
  }
}

GF zech_sub(Zech zech, GF x, GF y) {
  return zech_add(zech, x, zech_neg(zech, y));
}

GF zech_inv(Zech zech, GF x) {
  if (x==-1) return x;      // 1/0 := 0
  if (x== 0) return x;      // 1/1  = 1
  return (QMINUS1(zech) - x);
}

GF zech_mul(Zech zech, GF x, GF y) {
  if (x==-1) return x;      // 0*y  = 0
  if (y==-1) return y;      // x*0  = 0
  int32_t n = QMINUS1(zech); 
  int32_t c = x + y;
  return ( (c<n) ? c : (c-n) );  
}

GF zech_div(Zech zech, GF x, GF y) {
  if (x==-1) return x;      // 0/y  = 0
  if (y==-1) return y;      // x/0 := 0
  int32_t n = QMINUS1(zech); 
  int32_t c = x - y;
  return ( (c>=0) ? c : (c+n) );  
}

GF zech_pow(Zech zech, GF x, int e) {
  if (x==-1) return x;      // 0^e  = 0 (we define 0^0 to be 0, because it's more consistent this way)
  if (e== 0) return 0;      // x^0  = 1
  if (x== 0) return x;      // 1^e  = 1
  if (e== 1) return x;      // x^1  = x
  int64_t n = QMINUS1(zech); 
  int64_t k = ((int64_t)x) * e;
  k = k%n;
  return ( (k>=0) ? k : (k+n) );  
}

GF zech_embed(Zech zech, int k) {
  int p = PRIME(zech);
  int32_t *embeds = EMBEDS(zech);
  if ((k>=0) && (k<p)) return embeds[k]; 
  else {
    int a = k%p;
    if (a < 0) a += p;
    return embeds[a];  
  }
}

int zech_enumerate(Zech zech, GF *tgt) {
  int32_t n = QMINUS1(zech) + 1;
  for(int32_t i=0; i<n; i++) tgt[i] = i-1;
  return n;
}


