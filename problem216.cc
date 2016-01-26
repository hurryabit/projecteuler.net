// problem216.cc

#include <iostream>
#include <random>
#include <vector>

using namespace std;

random_device rd;
default_random_engine gen(rd());

unsigned long pow(unsigned long x, unsigned long k, const unsigned long p) {
    unsigned long y = 1;
    while( k > 0 ) {
        if( k % 2 == 1 )
            y = (y*x) % p;
        x = (x*x) % p;
        k /= 2;
    }
    return y;
}

unsigned long sqrt(const unsigned long n, const unsigned long p) {
    if( pow(n, p/2, p) != 1 )
        return 0;

    unsigned long s = 1, q = p/2;
    while( q % 2 == 0 ) {
        ++s;
        q /= 2;
    }

    if( s == 1 )
        return pow(n, (p+1)/4, p);

    uniform_int_distribution<unsigned long> dist(1,p-1);
    unsigned long z;
    do {
        z = dist(gen);
    } while( pow(z, p/2, p) != p-1 );

    unsigned long r = pow(n, (q+1)/2, p),
             t = pow(n, q, p),
             m = s,
             c = pow(z, q, p);
    while( t != 1 ) {
        unsigned long t2i = t, i = 0;
        while( t2i != 1 ) {
            t2i = (t2i*t2i) % p;
            ++i;
        }
        unsigned long b = pow(c, 1 << (m-i-1), p);
        r = (r*b) % p;
        t = t * ((b*b) % p) % p;
        m = i;
        c = (b*b) % p;
    }

    return min(r, p-r);
}

int main() {
    vector<bool> primes(71000000, true);
    vector<bool> table(50000001, true);

    for( unsigned long p = 3; p < 71000000; p += 2 ) {
        if( primes[p] ) {
            for( unsigned long n = 2*p; n < 71000000; n += p ) {
                primes[n] = false;
            }

            unsigned long x = sqrt((p+1)/2, p);
            if( x > 0 ) {
                for( unsigned long n = x; n <= 50000000; n += p )
                    table[n] = false;
                for( unsigned long n = p-x; n <= 50000000; n += p )
                    table[n] = false;
            }
            if( 2*x*x - 1 == p )
                table[x] = true;
        }
    }

    unsigned long count = 0;
    for( unsigned long n = 2; n <= 50000000; ++n ) {
        if( table[n] )
            ++count;
    }

    cout << count << endl;
}
