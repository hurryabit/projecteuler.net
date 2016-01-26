// problem66.c

#include <math.h>
#include <stdio.h>

typedef enum { FALSE = 0, TRUE = 1 } BOOL;

int main() {
  unsigned long long d = 61, sd;
  unsigned long long y;
  unsigned long long x, x2;
  BOOL found_solution;
  FILE* output;

  output = fopen("problem66_1mrd.txt", "w");

  for( d = 1; d <= 1000; ++d )
  {
    sd = floor(sqrt(d));
    if (sd*sd == d)
      continue;

    found_solution = FALSE;

    for (y = 1; y <= 0x8000000; ++y) // y <= 0x8000000 = 2^27 1000y^2 < 2^64
    {
      x2 = 1+d*y*y;
      x = floor(sqrt(x2));
      if (x*x == x2) {
        found_solution = TRUE;
        break;
      }
    }

    if (found_solution) {
      printf("x=%lld, d=%lld\n", x, d);
    } else {
      printf("NO SOLUTION for %lld\n", d);
      fprintf(output, "%lld\n", d);
    }
  }

  fclose(output);

  return 0;
}
