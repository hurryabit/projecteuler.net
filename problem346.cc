// problem346.cc

#include <iostream>
#include <set>

using namespace std;

typedef long unsigned nat_t;

const nat_t LIMIT = 1e12;

int main() {
    set<nat_t> sru;
    sru.insert(1);

    nat_t n;
    for( nat_t b = 2; (n = b*b+b+1) <= LIMIT; ++b ) {
        while( n <= LIMIT ) {
            sru.insert(n);
            n = b*n+1;
        }
    }

    nat_t sum = 0;
    for( auto &n: sru ) {
        sum += n;
    }

    cout << sum << endl;
}