// project345.c

#include <fstream>
#include <iostream>

using namespace std;

const int N = 15;

int matrix[N][N];

void read_matrix() {
    ifstream file("problem345.txt");
    
    for(int i = 0; i < N; ++i) {
        for(int j = 0; j < N; ++j) {
            file >> matrix[i][j];
        }
    }
}

bool bad_patterns[N][N][N][N];

void init_bad_patterns() {
    for(int i1 = 0; i1 < N; ++i1) {
        for(int j1 = 0; j1 < N; ++j1) {
            for(int i2 = 0; i2 < N; ++i2) {
                for(int j2 = 0; j2 < N; ++j2) {
                    bad_patterns[i1][j1][i2][j2] = matrix[i1][j1] + matrix[i2][j2] < matrix[i1][j2] + matrix[i2][j1];
                }
            }
        }
    }
}

int j[N];

int maximum = 0;

void backtrack(int i1) {
    if(i1 == N) {
        int sum = 0;
        for(int i = 0; i < N; ++i) {
            sum += matrix[i][j[i]];
        }
        if(sum > maximum) {
            maximum = sum;
//            cout << sum << ": ";
//            for(int i = 0; i < N; ++i) {
//                cout << j[i] << "; ";
//            }
//            cout << endl;
        }
    }
    else {
        for(int j1 = 0; j1 < N; ++j1) {
            bool bad_j1 = false;
            for(int i2 = 0; i2 < i1; ++i2) {
                if(j[i2] == j1 || bad_patterns[i1][j1][i2][j[i2]]) {
                    bad_j1 = true;
                    break;
                }
            }
            if(bad_j1)
                continue;
            j[i1] = j1;
            backtrack(i1+1);
        }
    }
}

int main() {
    read_matrix();
    
    init_bad_patterns();
    
    backtrack(0);

    cout << maximum << endl;

    return 0;
}
