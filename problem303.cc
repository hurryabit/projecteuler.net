// problem303.cc

#include <cstdio>

long unsigned next(long unsigned m) {
    if(m % 10 < 2)
        return m+1;
    else
        return 10*next(m/10);
}

int main() {
    long unsigned sum = 0;
    for(int n = 1; n <= 1000; ++n) {
        long unsigned m = 1;
        while(m < 2222222222222222222 && m % n != 0)
            m = next(m);
        if(m % n != 0) {
            printf("no solution found for n = %d.\n", n);
            return 1;
        }
        else {
            printf("f(%d) = %lu\n", n, m);
            sum += m / n;
        }
    }
    
    printf("sum = %lu\n", sum);
}

// sum[9998] = 648549117390