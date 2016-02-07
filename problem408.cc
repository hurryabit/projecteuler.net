// problem408.cc

#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

template<typename num> num extended_euclid(num a, num b, num& r, num& s) {
  if( b == 0 ) {
    r = 1;
    s = 0;
    return a;
  }
  else {
    num q = a / b;
    num c = a % b;
    num d = extended_euclid(b, c, r, s);
    r -= q * s;
    swap(r, s);
    return d;
  }
}


template<typename num, num mod> struct mod_t {
  num value;

  mod_t<num, mod>& readjust() {
    value %= mod;
    if( value < 0 )
      value += mod;
    return *this;
  }

  inline mod_t(num value_ = 0) {
    value = value_;
    readjust();
  }

  inline mod_t<num, mod> operator+(mod_t<num, mod> x) const {
    return mod_t<num, mod>(value + x.value);
  }

  inline mod_t<num, mod> operator-(mod_t<num, mod> x) const {
    return mod_t<num, mod>(value - x.value);
  }

  inline mod_t<num, mod> operator*(mod_t<num, mod> x) const {
    return mod_t<num, mod>(value * x.value);
  }

  inline mod_t<num, mod>& operator+=(mod_t<num, mod> x) {
    value += x.value;
    return readjust();
  }

  inline mod_t<num, mod>& operator-=(mod_t<num, mod> x) {
    value -= x.value;
    return readjust();
  }

  inline mod_t<num, mod>& operator*=(mod_t<num, mod> x) {
    value *= x.value;
    return readjust();
  }

  mod_t<num, mod> inverse() const {
    num r, s;
    if( extended_euclid(value, mod, r, s) != 1 )
      throw domain_error("modular inverse is not defined");
    return r;
  }
};

template<typename num, num mod> ostream& operator<<(ostream& out, mod_t<num, mod> x) {
  out << x.value << " (mod " << mod << ")";
  return out;
}


using point_t = pair<long,long>;

point_t inline operator-(point_t p, point_t q) {
  return make_pair(p.first - q.first, p.second - q.second);
}

inline point_t flip(point_t p) {
  return make_pair(p.second, p.first);
}

bool below(point_t p, point_t q) {
  return p.first <= q.first && p.second <= q.second;
}

vector<point_t> gen_inadmissible_points(long n) {
  long sq = (long) floor(sqrt(n));
  vector<point_t> inadmissible_points;
  inadmissible_points.reserve(2*sq);

  for( long u = 3; u*u - (sq/u)*(sq/u) <= 2*sq; u += 2 ) {
    long v_min = 1;
    if( u*u >= 2*sq ) {
      v_min = (long) ceil(sqrt(u*u - 2*sq));
      if( v_min % 2 == 0 )
        ++v_min;
    }
    long v_max = min(u-2, sq/u);
    for( long v = v_min; v <= v_max; v += 2 ) {
      long a = (u*u-v*v) / 2, b = u*v;
      long k_max = sq / max( a, b);
      for( long k = 1; k <= k_max; ++k )
        inadmissible_points.push_back(make_pair(k*k*a*a,k*k*b*b));
    }
  }
  sort(inadmissible_points.begin(), inadmissible_points.end(),
       [](point_t p, point_t q) -> bool {
         return p.first + p.second < q.first + q.second;
       });
  return inadmissible_points;
}


template<typename num> using number_tables_t = pair<vector<num>,vector<num>>;

template<typename num> number_tables_t<num> gen_number_tables(long n) {
  long size = 2*n+1;
  vector<num> factorials(size), inverse_factorials(size);
  factorials[0] = 1;
  inverse_factorials[0] = 1;

  for( long k = 1; k <= size; ++k ) {
    factorials[k] = factorials[k-1] * k;
    inverse_factorials[k] = factorials[k].inverse();
  }

  return make_pair(factorials, inverse_factorials);
}

template<typename num> inline num num_grid_paths(point_t p, const number_tables_t<num>& number_tables) {
  long x = p.first, y = p.second;
  return number_tables.first[x+y] * number_tables.second[x] * number_tables.second[y];
}


using count_t = mod_t<long,1000000007>;

int main() {
  long n = 10000000;
  const auto number_tables = gen_number_tables<count_t>(n);

  auto ps = gen_inadmissible_points(n);
  ps.push_back(make_pair(n, n));
  long num_ps = ps.size();

  vector<count_t> num_adm_paths(num_ps);
  for( long i = 0; i < num_ps; ++i ) {
    point_t p = ps[i];
    num_adm_paths[i] = num_grid_paths(p, number_tables);
    for( long j = 0; j < i; ++j ) {
      for( point_t q: {ps[j], flip(ps[j])} ) {
        if( below(q, p) )
          num_adm_paths[i] -= num_adm_paths[j] * num_grid_paths(p-q, number_tables);
      }
    }
  }
  cout << num_adm_paths.back() << endl;
}
