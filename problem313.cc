// problem313.cc

#include <iostream>
#include <vector>

using namespace std;

typedef long unsigned nat_t;

int main() {
  nat_t LIMIT = 1000000;
  vector<bool> isPrime(LIMIT, true);
  
  nat_t result = 0;
  for( nat_t p = 3; p < LIMIT; p += 2 ) {
    if( isPrime[p] ) { 
      for( nat_t k = p*p; k < LIMIT; k += 2*p )
        isPrime[k] = false;
      result += (p*p+9) / 6 - (p*p+14) / 8;
    }
  }
  cout << 2*result << endl;
}