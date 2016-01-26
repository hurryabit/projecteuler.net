// problem452.cc

#include <iostream>
#include <vector>

using namespace std;

typedef long unsigned int nat_t;

const nat_t LIMIT = 1e9;

int main() {
    vector<bool> is_prime(LIMIT,true);
    vector<nat_t> primes;
    primes.reserve(49e6);
    primes.push_back(2);
    for( nat_t p = 3; p < LIMIT; p += 2 ) {
        if( is_prime[p] ) {
            primes.push_back(p);
            for( nat_t n = p*p; n < LIMIT; n += (p+p) ) {
                is_prime[n] = false;
            }
        }
    }

    cout << primes.size() << endl;
}