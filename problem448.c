// problem448.c

#include <stdio.h>

#define N 99999999019


int main() {
    long i;
    int r;
    for(i=0; i<N; ++i) {
        r = 2*r % 5;
        if(i % 1000000 == 0)
            printf("%d\n", (int) (i / 1000000));
    }
}