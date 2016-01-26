// problem485.cc

#include <iostream>
#include <vector>

using namespace std;

typedef long unsigned nat_t;
typedef pair<nat_t,nat_t> nat2_t;

const nat_t MAX_PRIMES = 10000;
const nat_t RANGE = 100000000;
const nat_t WINDOW = 100000;


nat2_t restart_mx(const vector<nat_t>& divisors, nat_t n) {
    cout << "restarting maximum search @ " << n << endl;
    nat2_t mx = make_pair(n, divisors[n]);
    for( nat_t j = n-WINDOW+1; j <= n; ++j ) {
        if( divisors[j] >= mx.second ) {
            mx.first = j;
            mx.second = divisors[j];
        }
    }
    return mx;
}


int main() {
    vector<nat_t> divisors(RANGE+1,1);
    vector<bool> primes(MAX_PRIMES,true);
    primes[0] = false;
    primes[1] = false;

    for( nat_t p = 2; p < MAX_PRIMES; ++p ) {
        if( primes[p] ) {
            cout << "treating prime " << p << endl;
            for( nat_t n = p*p; n < MAX_PRIMES; n += p ) {
                primes[n] = false;
            }

            for( nat_t p2k = p, k = 1; p2k <= RANGE; p2k *= p, ++k ) {
                for( nat_t n = p2k; n <= RANGE; n += p2k ) {
                    (divisors[n] /= k) *= k+1;
                }
            }
        }
    }

    nat2_t mx = restart_mx(divisors, WINDOW);
    nat_t result = 0;
    for( nat_t n = WINDOW; n <= RANGE; ++n ) {
        if( divisors[n] >= mx.second ) {
            mx.first = n;
            mx.second = divisors[n];
        }
        if( n - mx.first >= WINDOW ) {
            mx = restart_mx(divisors, n);
        }
        result += mx.second;
    }

    cout << result << endl;
}