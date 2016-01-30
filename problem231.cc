// problem231.cc

#include <fstream>
#include <iostream>

using namespace std;

const static int PRIMES_COUNT = 607;
unsigned primes[PRIMES_COUNT];

void fillPrimes() {
    ifstream primes_file("primes.txt");
    for (int i = 0; i < PRIMES_COUNT; ++i) {
        primes_file >> primes[i];
    }
    primes_file.close();
}

unsigned long long weight(unsigned n) {
    unsigned long long result = 0;
    int i = 0;
    unsigned prime = primes[i];

    while (n != 1 && i < PRIMES_COUNT) {
        if (n % prime == 0) {
            n /= prime;
            result += prime;
        } else {
            prime = primes[++i];
        }
    }

    if (n != 1) {
        result += n;
    }

    return result;
}

int main() {
    fillPrimes();

    unsigned long long result = 0;

    for (unsigned n = 15000001; n <= 20000000; ++n) {
        result += weight(n);
    }

    for (unsigned n = 1; n <= 5000000; ++n) {
        result -= weight(n);
    }

    cout << result << endl;

    return 0;
}
