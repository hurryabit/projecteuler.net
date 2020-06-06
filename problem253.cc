// problem253.cc

#include <iomanip>
#include <iostream>
#include <random>
#include <set>

using namespace std;

random_device rd;
//mt19937 gen(rd());
uniform_real_distribution<> dist(0, 1);

int simulate(int n) {
  vector<bool> occupied(n+2, false);
  int blocks = 0;
  int max_blocks = 0;

  for( int i = n; i >= 1; --i ) {
    int k = (int) floor(dist(rd) * i) + 1;

    int j = 0;
    for( int l = 1; l <= k; ++l ) {
      ++j;
      while( occupied[j] )
        ++j;
    }

    occupied[j] = true;
    blocks += 1 - occupied[j-1] - occupied[j+1];
    if( blocks > max_blocks )
      max_blocks = blocks;
  }
  return max_blocks;
}

int main() {
  cout << setprecision(8) << fixed;

  double sum_max_blocks;
  for( int n = 1; n <= 1000000000; ++n ) {
    sum_max_blocks += simulate(10);
    if( n % 10000 == 0 ) {
      double avg_max_blocks = sum_max_blocks / n;
      cout << n << " => " << avg_max_blocks << endl;
    }
  }
}