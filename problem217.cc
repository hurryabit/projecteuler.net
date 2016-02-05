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
  multiset_t<sum_t> xs[K*9+1], xs_old[K*9+1];
  multiset_t<sum_t> ys[K*9+1], ys_old[K*9+1];

  multiset_t<sum_t> t = {9, 45}, digits = {10, 45};
  sum_t ten2k = 10;
  for( nat_t k = 1; k <= K; ++k ) { // num of digits
    if( k == 1 ) {
      xs[0] = multiset_t<sum_t>();
      ys[0] = singleton<sum_t>(0);
      for( nat_t d = 1; d <= 9; ++d ) {
        xs[d] = singleton<sum_t>(d);
        ys[d] = singleton<sum_t>(d);
      }
    }
    else {
      swap(xs, xs_old);
      swap(ys, ys_old);
      xs[0] = multiset_t<sum_t>();
      ys[0] = singleton<sum_t>(0);
      for( nat_t d = 1; d <= k*9; ++d ) { // sum of digits
        xs[d] = multiset_t<sum_t>();
        for( nat_t l = 0; l <= 9 && l <= d; ++l ) // new last digit
          xs[d] |= sum_t(10) * xs_old[d-l] + singleton<sum_t>(l);
        ys[d] = ys_old[d] | xs[d];
      }
    }

    for( nat_t d = 1; d <= k*9; ++d ) { // sum of digits
      t |= ten2k * xs[d] + ys[d];
      t |= ten2k*(sum_t(10) * xs[d] + digits) + ys[d];
    }
    ten2k *= 10;
  }

  cout << t.sum << endl;
}