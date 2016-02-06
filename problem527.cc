// problem527.c

#include <iomanip>
#include <iostream>
#include <map>

using namespace std;

double r(long n) {
  double a = 0, m;
  for( long i = 1; i <= n; ++i ) {
    m = i;
    a += 2 + a / m - 1 / m;
  }
  return a / m;
}

double b(long n) {
  static map<long,double> memory;
  const auto it = memory.find(n);
  if( it != memory.end() )
    return it->second;

  double result;
  long k = n >> 1;
  if( n == 0 )
    result = 0;
  else if( (n & 1) == 0 ) { // n even
    result = 1 + (k * b(k) + (k-1) * b(k-1)) / n;
  }
  else { // n odd
    result = 1 + (2 * k * b(k)) / n;
  }
  memory.insert(make_pair(n, result));
  return result;
}

int main() {
  cout << setprecision(8) << fixed << r(10000000000) - b(10000000000) << endl;
}