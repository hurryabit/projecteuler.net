// problem139.cc

#include <algorithm>
#include <cmath>
#include <cstdio>

using namespace std;

const long LIMIT = 99999999;

long gcd(long a, long b) {
    while(b != 0) {
        long c = a % b;
        a = b;
        b = c;
    }
    return a;
}
int main() {
    long count = 0;
    for(long u = 1; 2*(u+1)*(2*u+1) <= LIMIT; ++u) {
        for(long v = u+1; 2*v*(u+v) <= LIMIT; v+=2) {
            if(gcd(v, u) != 1)
                continue;
            long a = v*v - u*u, b = 2*u*v, c = u*u + v*v;
            if(c % (a-b) == 0) {
                long p = a+b+c;
                count += (long) (LIMIT / p);
            }
        }
    }
    
    printf("%ld\n", count);
}