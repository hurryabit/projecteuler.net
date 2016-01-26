// problem141.cc

#include <cmath>
#include <cstdio>
#include <set>

using namespace std;

const long N = 1e12;

inline bool is_square(long n) {
    long r = floor(sqrt(n));
    return n == r*r;
}

int main() {
    set<long> solutions;
    
    long n;
    for(long b = 1; b*b*b*b+b*b < N; ++b) {
        for(long a = b+1; a*a*a*b+b*b < N; ++a) {
            for(long r = 1; (n = a*a*a*b*r*r+b*b*r) < N; ++r) {
                if(is_square(n)) {
                    solutions.insert(n);
                }
            }
        }
    }
    
    long sum = 0;
    for(set<long>::const_iterator i = solutions.begin(); i != solutions.end(); ++i ) {
        sum += *i;
    }
    
    printf("%ld\n", sum);
}