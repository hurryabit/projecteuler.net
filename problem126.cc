// problem126.cc

#include <cmath>
#include <iostream>

using namespace std;

long C(long n) {
    long res = 0;
    for(long l = 1; 6+12*(l-1)+4*(l-1)*(l-2) <= n; ++l) {
        long s0 = (long) sqrt(9*(l-1)*(l-1)-6*(l-1)*(l-2)+3*n/2) - 3*(l-1);
        for(long s = s0; 4*s-6+4*(l-1)*s+4*(l-1)*(l-2) <= n; ++s) {
            long t = (n-4*(l-1)*s-4*(l-1)*(l-2))/2;
            for(long a = 1; a <= s/3; ++a) {
                long d = (s-a)*(s-a)+4*a*(s-a)-4*t;
                long e = (long) sqrt(d);
                if(d == e*e) {
                    long b2 = (s-a)-e;
                    if(b2 % 2 == 0 && a <= b2/2) {
                        ++res;
                    }
                }
            }
        }
    }
    return res;
}

int main() {
/*    long n = 46;
    long c = C(n);
    cout << "C(" << n << ")=" << c << endl;*/
    long c = 1000;
    long n = 6;
    while(C(n) != c) {
        n += 2;
        if(n % 100 == 0) {
            cout << "@ n=" << n << endl;
        }
    }
    cout << "C(" << n << ")=" << c << endl;
}