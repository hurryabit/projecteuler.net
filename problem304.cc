// problem304.cc

#include <iostream>
#include <vector>

using namespace std;

typedef long unsigned nat_t;

const nat_t MAX_PRIMES = 10000000;

const nat_t START = 100000000000000;
const nat_t END = 4000000;


bool is_prime(const nat_t n, const vector<nat_t>& primes) {
    for( nat_t p = 2; p*p <=n; p = primes[p] ) {
        if( n % p == 0 )
            return false;
    }
    return true;
}

int main() {
    vector<nat_t> primes(MAX_PRIMES,1);
    primes[0] = MAX_PRIMES;
    primes[1] = 0;
    primes[2] = 3;

    nat_t last_p = 2;
    for( nat_t n = 3; n < MAX_PRIMES; n += 2 ) {
        if( primes[n] == 1 ) {
            primes[last_p] = n;
            for( nat_t k = n*n; k < MAX_PRIMES; k += (2*n) ) {
                primes[k] = 0;
            }
            last_p = n;
        }
    }
    primes[last_p] = MAX_PRIMES;

    vector<bool> large(4000000, true);
    for( nat_t p = 3; p < MAX_PRIMES; p = primes[p] ) {
        nat_t n = p - START % p;
        if( n % 2 == 0 )
            n += p;
        for( ; n < END; n += (2*p) ) {
            large[n] = false;
        }
    }

    nat_t count = 0;
    for( nat_t n = 1; count < 100000; n += 2 ) {
        if( large[n] ) {
            ++count;
            cout << START + n << endl;
        }
    }
}