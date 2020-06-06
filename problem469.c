// problem469.c
#include <stdio.h>

typedef long unsigned nat_t;

int main() {
  nat_t count = 0;
  double e = 2.0, p = 1.0, h;
  for( nat_t n = 3; n <= 100000000000; ++n ) {
    h = e;
    e = 2 * p / (n-2);
    p += h;
    if( n % 987563 == 0 )
      printf("E(%10lu) = %.14f\n", n+1, e / (n+1));
  }
}
