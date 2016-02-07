//
//  problem421.cc
//  ProjectEuler
//
//  Created by Martin Huschenbett on 07.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <algorithm>
#include <iostream>
#include <list>
#include <random>
#include <vector>

using namespace std;

random_device rd;
mt19937 gen(rd());

long pow_mod(long a, long k, long n) {
  long b = 1;
  while( k > 0 ) {
    if( (k & 1) == 1 )
      b = (a * b) % n;
    a = (a * a) % n;
    k = k >> 1;
  }
  return b;
}

long gcd(long a, long b) {
  while( b > 0 ) {
    long c = a % b;
    a = b;
    b = c;
  }
  return a;
}

long extended_euclid(long a, long b, long& r, long& s) {
  if( b == 0 ) {
    r = 1;
    s = 0;
    return a;
  }
  else {
    long q = a / b;
    long c = a - q * b;
    long d = extended_euclid(b, c, r, s);
    r -= q * s;
    swap(r, s);
    return d;
  }
}

long inv_mod(long a, long n) {
  long r, s;
  if( extended_euclid(a, n, r, s) != 1 )
    throw domain_error("modular inverse is not defined");
  return (r % n + n) % n;
}

list<long> distinct_prime_factors(long n, const vector<long>& primes) {
  list<long> factors;
  for( auto p = primes.begin(); p != primes.end() && *p * *p <= n; ++p ) {
    if( n % *p == 0 ) {
      factors.push_back(*p);
      while( n % *p == 0 )
        n /= *p;
    }
  }
  if( n > 1 )
    factors.push_back(n);
  return factors;
}

bool is_primitive(long a, long p, const vector<long>& primes) {
  long n = p-1;
  for( long q: distinct_prime_factors(n, primes) ) {
    if( pow_mod(a, n/q, p) == 1 )
      return false;
  }
  return true;
}

long find_primitive(long p, const vector<long>& primes) {
  uniform_int_distribution<long> dist(1, p-1);
  long a = 1;
  while( !is_primitive(a, p, primes) ) {
    a = dist(gen);
  }
  return a;
}

list<long> roots(long p, const vector<long>& primes) {
  list<long> roots;
  long d = gcd(15, p-1);
  long n = (p-1) / d;
  long y0 = (n/2 * inv_mod(15/d, n)) % n;
  long a = find_primitive(p, primes);
  for( long y = y0; y < p-1; y += n ) {
    roots.push_back(pow_mod(a, y, p));
  }
  //roots.sort();
  return roots;
}

vector<long> sieve_primes(long n) {
  vector<long> primes;
  primes.reserve(floor(1.1 * n / log(n)));
  vector<bool> is_prime = vector<bool>(n, true);
  for( long p = 2; p < n; ++p ) {
    if( is_prime[p] ) {
      primes.push_back(p);
      for( long i = p*p; i < n; i += p )
        is_prime[i] = false;
    }
  }
  return primes;
}

const long MAX_N = 100000000000;
const long M = 100000000;

int main() {
  vector<long> primes = sieve_primes(M);
/*  for( auto x: roots(5, primes) ) {
    cout << x << " ";
  }
  cout << endl;*/

  long result = MAX_N;
  for( auto p = primes.begin()+1; p != primes.end(); ++p ) {
    auto xs = roots(*p, primes);
    long q = MAX_N / *p;
    long r = MAX_N % *p;
    long count = q * xs.size();
    for( auto x: xs ) {
      if( x <= r )
        ++count;
    }
    result += count * *p;
  }
  cout << result << endl;
}
