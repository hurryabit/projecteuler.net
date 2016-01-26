// problem467.cc

#include <iostream>
#include <vector>

using namespace std;

long digitalRoot(long n) {
    long s = 0;
    while( n > 0 ) {
        s += n % 10;
        n /= 10;
    }
    return s < 10 ? s : digitalRoot(s);
}

void addDigit(long &result, long d) {
    result = (10*result+d) % 1000000007;
}

int main() {
    vector<bool> primes(105000, true);
    primes[0] = false;
    primes[1] = false;

    for( long p = 2; p < primes.size(); ++p ) {
        if( primes[p] ) {
            for( long n = p*p; n < primes.size(); n += p ) {
                primes[n] = false;
        }
    }
}

    const long size = 10000;
    vector<long> p10000, c10000;

    for( long n = 2; p10000.size() < size || c10000.size() < size; ++n ) {
        if( primes[n] && p10000.size() < size ) {
            p10000.push_back(digitalRoot(n));
        }
        if( !primes[n] && c10000.size() < size ) {
            c10000.push_back(digitalRoot(n));
        }
    }

    vector<vector<long>> table(size+1, vector<long>(size+1,0));

    for( long i = size-1; i >= 0; --i ) {
        for( long j = size-1; j >= 0; --j ) {
            table[i][j] = p10000[i] == c10000[j] ? table[i+1][j+1] + 1 : max(table[i+1][j], table[i][j+1]);
        }
    }

    long result = 0;
    long i = 0, j = 0;

    while( i < size && j < size ) {
        if( p10000[i] == c10000[j] ) {
            addDigit(result, p10000[i]);
            ++i;
            ++j;
        }
        else if( table[i][j+1] < table[i][j] || (table[i+1][j] == table[i][j] && p10000[i] < c10000[j]) ) {
            addDigit(result, p10000[i]);
            ++i;
        }
        else if( table[i+1][j] < table[i][j] || (table[i][j+1] == table[i][j] && p10000[i] > c10000[j]) ) {
            addDigit(result, c10000[j]);
            ++j;
        }
        else {
            cerr << "ERROR!" << endl;
            return -1;
        }
    }

    for( ; i < size; ++i ) {
        addDigit(result, p10000[i]);
    }
    for( ; j < size; ++j ){
        addDigit(result, c10000[j]);
    }

    cout << result << endl;
}
