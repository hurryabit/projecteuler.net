// problem378.c
#include <stdio.h>
#include <stdlib.h>

typedef long unsigned nat_t;
const nat_t N  = 60000000;

nat_t div_max_pow(nat_t n, nat_t p) {
  nat_t k = 0;
  while( n % p == 0 ) {
    ++k;
    n /= p;
  }
  return k;
}

nat_t sum_range(nat_t *table, nat_t low, nat_t high) {
  nat_t sum = 0;
  for( nat_t i = low; i <= high; ++i )
    sum += table[i];
  return sum;
}

int main() {
  nat_t *dT = (nat_t *) calloc(N+1, sizeof(nat_t));
  --dT;
  for( nat_t n = 1; n <= N+1; ++n )
    dT[n] = 1;
  
  // calculate values of Euler's phi function
  for( nat_t n = 2; n <= N+1; n += 1 ) {
    if( dT[n] == 1 ) { // n is a prime
      for( nat_t i = n; i <= N+1; i += n )
        dT[i] *= div_max_pow(i, n) + 1;
    }
  }
  
  // calculate values of dT
  for( nat_t n = 1; n <= N; ++n ) {
    nat_t m, d1, d2;
    if( n & 1 ) { // n is odd
      m = n+1; d1 = dT[m]; d2 = dT[n];
    }
    else { // n is even
      m = n; d1 = dT[n]; d2 = dT[n+1];
    }
    nat_t k = div_max_pow(m, 2);
    dT[n] = d1 / (k+1) * k * d2;
  }
  
  nat_t max = 0;
  for( nat_t n = 1; n <= N; ++n ) {
    if( dT[n] > max )
      max = dT[n];
  }
  
  nat_t *dist = (nat_t *) calloc(max, sizeof(nat_t));
  --dist;
  
  nat_t *beyond = (nat_t *) calloc(N, sizeof(nat_t));
  --beyond;
  for( nat_t n = N; n >= 1; --n ) {
    beyond[n] = sum_range(dist, 1, dT[n]-1);
    ++dist[dT[n]];
  }

  for ( nat_t i = 1; i <= max; ++i )
    dist[i] = 0;
  nat_t result = 0;
  for( nat_t n = 1; n <= N; ++n ) {
    result += beyond[n] * (n-1 - sum_range(dist, 1, dT[n]));
    result %= 1000000000000000000;
    ++dist[dT[n]];
  }

  printf("%lu\n", result);
}