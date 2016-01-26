// problem142.cc

#include <cmath>
#include <cstdio>
#include <list>
#include <map>

using namespace std;

inline long SQ(const long n) {
    return n*n;
}

bool is_square(long n) {
    long r = (long) sqrt(n);
    return SQ(r) == n;
}

long gcd(long a, long b) {
    while(b != 0) {
        long c = a % b;
        a = b;
        b = c;
    }
    return a;
}

const long LIMIT = 1000000;

typedef list< pair<long,long> > decomp_list;

map<long, decomp_list> memory;

long min_x, min_y, min_z, min_s = 0x7fffffffffffffff;

void check_sol(long a1, long b1, long a2, long b2, long c) {
    long d = SQ(b2) - SQ(a1);
    if(d % 2 == 0) {
        long x = d / 2, y = SQ(b2) - x, z = SQ(b1) - x;
        if(y - x == SQ(a1) && y+z == SQ(c) && is_square(z-y)) {
            long s = x+y+z;
            if(s < min_s) {
                min_x = x; min_y = y; min_z = z; min_s = s;
            }
        }
    }
}

int main() {
    for(long u0 = 1; 1+SQ(2*u0-1) <= LIMIT; ++u0) {
        for(long u = u0, v = u0+1; u > 0; --u, ++v) {
            if(gcd(v, u) != 1)
                continue;
            
            long a0 = SQ(v) - SQ(u), b0 = 2*u*v, c0 = SQ(u) + SQ(v);
            if(a0 > b0)
                swap(a0, b0);
            
            for(int k = 1; k*c0 <= LIMIT; ++k) {
                long a = k*a0, b = k*b0, c = k*c0;
                decomp_list& decomp_c = memory[c];
            
                if(!decomp_c.empty()) {
                    for(decomp_list::const_iterator i = decomp_c.begin(); i != decomp_c.end(); ++i) {
                        long a1 = a, b1 = b;
                        long a2 = i->first, b2 = i->second;
                        if(a1 > a2) {
                            swap(a1, a2);
                            swap(b1, b2);
                        }
                        
                        check_sol(a1, b1, a2, b2, c);
                        check_sol(a1, b1, b2, a2, c);
                    }
                }
                decomp_c.push_back(make_pair(a, b));
            }
        }
    }
    if(min_s < 0x7fffffffffffffff)
        printf("x=%ld, y=%ld, z=%ld, x+y+z=%ld\n", min_x, min_y, min_z, min_x+min_y+min_z);
    else
        printf("no solution found");
    return 0;
}