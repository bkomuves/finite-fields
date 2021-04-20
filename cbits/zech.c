
//
// C implementation of arithmetic in GF(p^m) via precomputed 
// tables of Zech's logarithms.
//
// This is here in case if you want to do arithmetic in fixed fields 
// (the tables can be exported by the Haskell library) in a C program.
//

#include <stdio.h>
#include <stdlib.h>

#include "zech.h"

// struct ZechTable {
//   uint32_t prime;
//   uint32_t dim;
//   uint32_t q_minus_1;
//   uint32_t log_minus_1;
//   uint32_t embeds[];
//   uint32_t zech_logs[];
// };

int ipow(int x,int e) {
  if (x==0) return 0;
  if (e==0) return 1;
  int f = e >> 1;
  int powf;
  if (f==0) powf=1; else powf=ipow(x,f);
  return (e&1) ? (powf*powf*x) : (powf*powf);
}

Zech loadZechTable(const char* fname) {
  FILE *f = fopen(fname,"r");
  if (f==0) { return NULL; }
  int32_t header[4];
  int n = fread(header,4,4,f);
  if (n!=4) { fclose(f); return NULL; }
  int32_t p   = header[0];
  int32_t m   = header[1];
  int32_t qm1 = header[2];
  int32_t e   = header[3];
  bool ok1 = (ipow(p,m) == qm1+1);
  bool ok2 = ( (p==2) ? (e==0) : (e == qm1/2) );
  if (!(ok1&&ok2)) { fclose(f); return NULL; }
  int len = 4 + p + qm1;
  int32_t *ptr = (int32_t*) malloc( sizeof(int32_t) * len );
  if (ptr == NULL) { fclose(f); return NULL; }
  fseek(f, 0, SEEK_SET);
  int k = fread(ptr,4,len,f);
  fclose(f);
  if (k != len) { return NULL; }
  return ptr;
}

#define ZECH_PRIME(zech)     zech[0]
#define ZECH_DIM(zech)       zech[1]
#define ZECH_QMINUS1(zech)   zech[2]
#define ZECH_LOGMINUS1(zech) zech[3]

#define ZECH_EMBEDS(zech)    (zech + 4)
#define ZECH_LOGS(zech) (zech + 4 + zech[0])

GF zech_neg(Zech zech , GF x) {
  if (x == -1) return x; else {
    int32_t n = ZECH_QMINUS1(zech);
    int32_t c = x + ZECH_LOGMINUS1(zech);
    return ( (c<n) ? c : (c-n) );
  }
}

GF zech_add(Zech zech, GF x, GF y) {
  if (x==-1) return y;
  if (y==-1) return x;
  int32_t n = ZECH_QMINUS1(zech);
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
  return (ZECH_QMINUS1(zech) - x);
}

GF zech_mul(Zech zech, GF x, GF y) {
  if (x==-1) return x;      // 0*y  = 0
  if (y==-1) return y;      // x*0  = 0
  int32_t n = ZECH_QMINUS1(zech); 
  int32_t c = x + y;
  return ( (c<n) ? c : (c-n) );  
}

GF zech_div(Zech zech, GF x, GF y) {
  if (x==-1) return x;      // 0/y  = 0
  if (y==-1) return y;      // x/0 := 0
  int32_t n = ZECH_QMINUS1(zech); 
  int32_t c = x - y;
  return ( (c>=0) ? c : (c+n) );  
}

GF zech_pow(Zech zech, GF x, int e) {
  if (x==-1) return x;      // 0^e  = 0 (we define 0^0 to be 0, because it's more consistent this way)
  if (e== 0) return 0;      // x^0  = 1
  if (x== 0) return x;      // 1^e  = 1
  if (e== 1) return x;      // x^1  = x
  int64_t n = ZECH_QMINUS1(zech); 
  int64_t k = ((int64_t)x) * e;
  k = k%n;
  return ( (k>=0) ? k : (k+n) );  
}

GF zech_embed(Zech zech, int k) {
  int p = ZECH_PRIME(zech);
  int32_t *embeds = ZECH_EMBEDS(zech);
  if ((k>=0) && (k<p)) return embeds[k]; 
  else {
    int a = k%p;
    if (a < 0) a += p;
    return embeds[a];  
  }
}

int zech_enumerate(Zech zech, GF *tgt) {
  int32_t n = ZECH_QMINUS1(zech) + 1;
  for(int32_t i=0; i<n; i++) tgt[i] = i-1;
  return n;
}


