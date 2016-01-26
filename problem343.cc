// problem343.cc

#include <iostream>
#include <vector>

using namespace std;

typedef long unsigned int nat_t;

const nat_t MAX_PRIMES = 100000000;

vector<nat_t> primes(MAX_PRIMES,0);
//vector<nat_t> max_factors(MAX_PRIMES,0);

void init_primes() {
    primes[0] = 0;
    primes[1] = 1;

    nat_t last_p = 2;
    for( nat_t n = 3; n < MAX_PRIMES; n += 2 ) {
        if( primes[n] == 0 ) {
            primes[last_p] = n;
            for( nat_t k = n; k < MAX_PRIMES; k += n ) {
                primes[k] = n;
            }
            last_p = n;
        }
    }
    primes[last_p] = MAX_PRIMES;

    for( nat_t n = 4; n < MAX_PRIMES; n *= 2 ) {
        primes[n] = 2;
    }
}


nat_t max_factor(nat_t n) {
    //cout << "max_factor(" << n << ")" << endl;
    if( n < MAX_PRIMES )
        return min(n, primes[n]);

    for( nat_t p = 2; p*p <= n; p = primes[p] ) {
        while( n % p == 0 )
            n /= p;
        if( n < MAX_PRIMES )
            return max(p, min(n, primes[n]));
    }
    return n;
}

int main() {
    init_primes();

    nat_t result = 0;
    for( nat_t k = 1; k <= 2000000; ++k ) {
        if( k % 1000 == 0 )
            cout << k << endl;
        result += max(max_factor(k+1), max_factor(k*k-k+1)) - 1;
    }
    cout << result << endl;
/*    for( nat_t k = 1; k <= 100; ++k ) {
        cout << k << ":" << max_factor(k, 2) << endl;
    }*/

}