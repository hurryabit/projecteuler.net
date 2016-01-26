// problem260.cc

#include <bitset>
#include <vector>

using namespace std;

const int N = 1000;

void set_bit(bitset<N+1>& bits, int i) {
    if(i <= N)
        bits.set(i);
}

int main() {
    vector< vector< int > > table;
    table.resize(N+1);

    for(int j = 0; j <= N; ++j) {
        table[j].resize(j+1);
        for(int i = 0; i <= j; ++i) {
            bitset<N+1> bits;
            
            for(int d = 1; d <= i; ++d) {
                set_bit(bits, table[j][i-d]);
                set_bit(bits, table[j][i-d]+d);
                set_bit(bits, table[j-d][i-d]);
                set_bit(bits, table[j-d][i-d]+d);
            }
            for(int d = 1; d <= j-i; ++d) {
                set_bit(bits, table[j-d][i]);
                set_bit(bits, table[j-d][i]+d);
            }
            for(int d = j-i+1; d <= j; ++d) {
                set_bit(bits, table[i][j-d]);
                set_bit(bits, table[i][j-d]+d);
            }

            int k;
            for(k = 0; k <= N && bits[k]; ++k);
            table[j][i] = k;
            //printf("new triple: i=%d, j=%d, k=%d\n", i, j, k);
        }
    }
    
    int sum = 0;
    for(int j = 0; j <= N; ++j) {
        for(int i = 0; i <= j; ++i) {
            int k = table[j][i];
            if(k >= j && k <= N) {
                sum += (i+j+k);
            }
        }
    }
    
    printf("sum: %d\n", sum);
}