// problem176.cc

#include <algorithm>
#include <cstdio>

using namespace std;

const long unsigned LIMIT = 10000;
const unsigned TARGET = 47547;
unsigned counts[LIMIT];

long unsigned gcd(long unsigned a, long unsigned b) {
    while(b != 0) {
        long unsigned c = a % b;
        a = b;
        b = c;
    }
    return a;
}

int main() {
    for(long unsigned a = 0; a < LIMIT; ++a) {
        counts[a] = 0;
    }
    
    long unsigned a, b;
    for(long unsigned s = 1; 2*s+1 < LIMIT; ++s) {
        for(long unsigned u = s, v = s+1; u > 0; --u, ++v) {
            if(gcd(v, u) != 1)
                continue;
            
            long unsigned a = v*v-u*u, b = 2*u*v;
            for(long unsigned k = 1; k*a < LIMIT; ++k)
                ++counts[k*a];
            for(long unsigned k = 1; k*b < LIMIT; ++k)
                ++counts[k*b];
        }
    }
    
/*    printf("[");
    unsigned sum = 0;
//    long unsigned a;
    for(a = 1; sum < TARGET && a < LIMIT; ++a) {
        if(counts[a] > 0) {
//            if(sum > 0)
//                printf(",");
//            printf("%lu", a);
            sum += counts[a];
            if(sum > TARGET)
                sum -= counts[a];
            printf("%u\n", sum);
        }
    }
    //printf("]\n%u\n", sum);
    printf("%lu", a);*/
    
    for(a = 1; a <= 1000; ++a)
        if(counts[a] > 0)
            printf("%lu -> %u\n", a, counts[a]);
}