// problem217.cc

#include <iostream>

using namespace std;

template<typename num, num mod> struct mod_t {
  num value;

  mod_t(num value_ = 0) : value(value_ % mod) {}

  mod_t<num, mod> operator+(mod_t<num, mod> x) {
    return mod_t<num, mod>(value + x.value);
  }

  mod_t<num, mod> operator*(mod_t<num, mod> x) {
    return mod_t<num, mod>(value * x.value);
  }

  mod_t<num, mod>& operator+=(mod_t<num, mod> x) {
    value += x.value;
    value %= mod;
    return *this;
  }

  mod_t<num, mod>& operator*=(mod_t<num, mod> x) {
    value *= x.value;
    value %= mod;
    return *this;
  }
};

template<typename num, num mod> ostream& operator<<(ostream& out, mod_t<num, mod> x) {
  out << x.value << " (mod " << mod << ")";
  return out;
}

template<typename num> struct multiset_t {
  num size;
  num sum;
  
  multiset_t() : size(0), sum(0) {}

  multiset_t(num size_, num sum_) : size(size_), sum(sum_) {};
  
  multiset_t<num>& operator|=(multiset_t<num> x) {
    size += x.size;
    sum  += x.sum;
    return *this;
  }
};

template<typename num> multiset_t<num> singleton(num value) {
  return multiset_t<num>(1, value);
}

template<typename num> multiset_t<num> operator|(multiset_t<num> x, multiset_t<num> y) { // union
  x |= y;
  return x;
}

template<typename num> multiset_t<num> operator+(multiset_t<num> x, multiset_t<num> y) {
  return {x.size * y.size, x.size * y.sum + y.size * x.sum};
}

template<typename num> multiset_t<num> operator*(num k, multiset_t<num> x) {
  x.sum *= k;;
  return x;
}

template<typename num> ostream& operator<<(ostream& out, multiset_t<num> x) {
    out << "multiset_t { size = " << x.size << ", sum = " << x.sum << " }";
    return out;
}

typedef long unsigned nat_t;
typedef mod_t<nat_t, 14348907> sum_t;

int main () {
  const nat_t K = 23;
  multiset_t<sum_t> xs[K+1][K*9+1];
  multiset_t<sum_t> ys[K+1][K*9+1];

  for( nat_t d = 1; d <= 9; ++d ) {
    xs[1][d] = singleton<sum_t>(d);
    ys[1][d] = singleton<sum_t>(d);
  }
  ys[0][0] = singleton<sum_t>(0);
  
  multiset_t<sum_t> t = {9, 45}, digits = {10, 45};
  sum_t ten2k = 10;
  for( nat_t k = 1; k <= K; ++k ) { // num of digits
    ys[k][0] = singleton<sum_t>(0);
    for( nat_t d = 1; d <= k*9; ++d ) { // sum of digits
      for( nat_t l = 0; l <= 9 && l <= d; ++l ) { // new last digit
        xs[k][d] |= sum_t(10) * xs[k-1][d-l] + singleton<sum_t>(l);
      }
      ys[k][d] = ys[k-1][d] | xs[k][d];
      t |= ten2k * xs[k][d] + ys[k][d];
      t |= ten2k*(sum_t(10) * xs[k][d] + digits) + ys[k][d];
    }
    ten2k *= 10;
  }

  cout << t.sum << endl;
}