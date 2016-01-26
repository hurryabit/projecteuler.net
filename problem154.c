// problem154.c

#include <stdio.h>

#define N 200000
#define E 12

int main() {
    int count = 0;
    int twos[N+1], fives[N+1];
    int i, j, k;

    twos[0] = 0;
    fives[0] = 0;
    for(int i = 1; i <= N; ++i) {
        twos[i] = i/2 + twos[i/2];
        fives[i] = i/5 + fives[i/5];
    }
    
    for(i = 0; 3*i <= N; ++i) {
        for(j = i; 2*j <= N-i; ++j) {
            k = N-i-j;
            if(fives[N] - fives[i] - fives[j] - fives[k] >= E && twos[N] - twos[i] - twos[j] - twos[k] >= E) {
                count += i != j && j != k ? 6 : 3;
            }
        }
    }
    printf("%d\n", count);
}