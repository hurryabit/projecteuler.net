// problem78.cc

#include <iostream>
#include <vector>

using namespace std;

const unsigned MOD = 1000000;

typedef vector<int> intvec;

int main() {
    vector<intvec> memo;
    memo.reserve(1000000);
    memo.push_back(intvec(1,1));
    
    int n = 0;
    while(memo[n][n] % MOD != 0) {
        ++n;
        intvec memo_n = intvec(n+1);
        memo_n[0] = 0;
        
        for(int k = 1; k <= n; ++k) {
            memo_n[k] = (memo[n-k][min(k,n-k)] + memo_n[k-1]) % MOD;
        }
        memo.push_back(memo_n);
    }
    
    cout << n << endl;
}