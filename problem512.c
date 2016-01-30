// problem512.c
#include <stdio.h>
#include <stdlib.h>

const long unsigned N  = 500000000;
const long unsigned I = N / 2;

int main() {
  long unsigned *table = (long unsigned *) calloc(I, sizeof(long unsigned));
  for( long unsigned i = 0; i < I; ++i )
    table[i] = 1;
  
  long unsigned result = 1;
  for( long unsigned n = 3; n < N; n += 2 ) {
    long unsigned i = n / 2;

    if( table[i] == 1 ) { // n is a prime
      for( long unsigned j = i; j < I; j += n )
        table[j] *= n-1;
      
      for( long unsigned m = n*n; m < N; m *= n )
        for( long unsigned j = m / 2; j < I; j += m )
          table[j] *= n;
    }

    result += table[i];
  }
  
  printf("%lu\n", result);
}
