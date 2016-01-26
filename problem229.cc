// problem229.cc

#include <iostream>
#include <vector>

using namespace std;

const unsigned SIZE = 2000000000;

int main() {
    vector<char> memory(SIZE+1, 0);
    unsigned sq, n;
    for( unsigned a = 1; (sq = a*a) < SIZE; ++a ) {
        unsigned n;
        for( unsigned b = 1; (n = sq + b*b) <= SIZE; ++b ) {
            memory[n] |= 0x01;
        }
    }

    for( unsigned a = 1; (sq = a*a) < SIZE; ++a ) {
        unsigned n;
        for( unsigned b = 1; (n = sq + 2*b*b) <= SIZE; ++b ) {
            memory[n] |= 0x02;
        }
    }

    for( unsigned a = 1; (sq = a*a) < SIZE; ++a ) {
        unsigned n;
        for( unsigned b = 1; (n = sq + 3*b*b) <= SIZE; ++b ) {
            memory[n] |= 0x04;
        }
    }

    for( unsigned a = 1; (sq = a*a) < SIZE; ++a ) {
        unsigned n;
        for( unsigned b = 1; (n = sq + 7*b*b) <= SIZE; ++b ) {
            memory[n] |= 0x08;
        }
    }

    unsigned count = 0;
    for( n = 0; n <= SIZE; ++n ) {
        if( memory[n] == 0x0F )
            ++count;
    }

    cout << count << endl;
}