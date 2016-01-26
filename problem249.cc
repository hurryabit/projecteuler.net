// problem249.cc

#include <iostream>
#include <vector>

using namespace std;

typedef long unsigned nat_t;

const nat_t LIMIT = 5000;
const nat_t PRECISION = 10000000000000000;

bool is_prime(nat_t n, const vector<nat_t> &primes) {
    for( auto &p: primes ) {
        if( n % p == 0 )
            return false;
        if( n < p*p )
            return true;
    }
    return true;
}

int main() {
    vector<nat_t> primes;
    primes.push_back(2);
    nat_t sum_primes = 2;
    for( nat_t n = 3; n < LIMIT; n += 2 ) {
        if( is_prime(n, primes) ) {
            primes.push_back(n);
            sum_primes += n;
        }
    }

    // count[n] during round p = number of subsets X of {2,3,...,p} satisfying ∑X = n
    vector<nat_t> count(sum_primes+1, 0);
    nat_t s = 0;
    count[0] = 1;

    for( auto &p: primes ) {
        s += p;
        for( nat_t n = s; n >= p; --n )
            (count[n] += count[n-p]) %= PRECISION;
    }

    nat_t solution = count[2];
    for( nat_t n = 3; n <= sum_primes; n += 2 ) {
        if( is_prime(n, primes) )
            (solution += count[n]) %= PRECISION;
    }

    cout << solution << endl;
}