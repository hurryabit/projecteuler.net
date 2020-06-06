// problem386.cc
#include<iostream>
#include<list>
#include<vector>

using namespace std;

typedef long unsigned nat_t;
typedef list<nat_t> fact_t;
typedef vector<fact_t> fact_table_t;

const int N = 100000000;

nat_t combinations(nat_t goal, list<nat_t>::const_iterator cur_it, const list<nat_t>::const_iterator end_it) {
  nat_t result = 1;
  for( ; cur_it != end_it; ++ cur_it )
    result *= *cur_it+1;
  return result;
  
  if( goal == 0 )
    return 1;

  if( cur_it == end_it )
    return 0;

  nat_t dim = min(*cur_it, goal);
  ++cur_it;
  //nat_t result = 0;
  for( nat_t k = 0; k <= dim; ++k )
    result += combinations(goal-k, cur_it, end_it);
  return result;
}

nat_t max_ac_length(const fact_t& fact) {
  list<nat_t> exps;
  nat_t max_exp = 0;
  
  fact_t::const_iterator it = fact.cbegin();
  while( it != fact.cend() ) {
    nat_t p = *it;
    nat_t exp = 0;
    while( it != fact.cend() && *it == p ) {
      ++it;
      ++exp;
    }

    exps.push_back(exp);
    if( exp > max_exp )
      max_exp = exp;
  }
  
  return combinations(max_exp, exps.cbegin(), exps.cend());
}


int main() {
  fact_table_t fact_table(N+1, fact_t());
  nat_t max = 0;
  nat_t p_max = 1;
  for( nat_t p = 2; p <= N; ++p ) {
    if( fact_table[p].empty() ) // p is prime
      for( nat_t q = p; q <= N; q *= p )
        for( nat_t m = q; m <= N; m += q )
          fact_table[m].push_back(p);
    
    //cout << "S[" << p << "] = " << max_ac_length(fact_table[p]) << endl;
    nat_t m = max_ac_length(fact_table[p]);
    if( m > max ) {
      max = m;
      p_max = p;
      cout << "new max " << max << " @ " << p_max << endl;
    }
  }
  
  cout << "final max " << max << " @ " << p_max << endl;
  return 0;
}