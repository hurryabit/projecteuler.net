// problem339.cc

#include <iostream>
#include <vector>

using namespace std;

const int n = 10000;
vector<double> cs(n+2, 0);
vector<double> ds(n+2, 0);

double expect(double d0, int k, int l) {
    int n = k+l;
    cs[0] = 0;
    ds[0] = d0;

    for( int i = 0; i < l; ++i ) {
        double q = cs[i] * (i-l) - n;
        cs[i+1] = (k+i) / q;
        ds[i+1] = ds[i] * (i-l) / q;
    }

    double x = n;
    for( int i = l; i > 0; --i ) {
        x = ds[i] - cs[i] * x;
    }
    return x;
}

int main() {
    double e43 = 0;
    for( int k = 1; k < n; ++k ) {
        e43 = expect(e43, k, k-1);
    }
    double e54 = expect(e43, n, n-1);
    double e64 = expect(e54, n+1, n-1);
    
    
    cout.precision(12);
    cout << (e43+e64)/2 << endl;
}