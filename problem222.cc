// problem222.cc

#include <cmath>
#include <cstdlib>
#include <ctime>
#include <iostream>

using namespace std;

const int N = 21;
double r[N];
double minimum = 1680;

double dist(int i, int j) {
    return sqrt( (r[i]+r[j])*(r[i]+r[j]) - (100-r[i]-r[j])*(100-r[i]-r[j]) );
}

bool bad_pattern(int i) {
    return i >=3 && dist(i-3,i-2) + dist(i-1,i) > dist(i-3,i-1) + dist(i-2,i);
}

void print() {
    cout << "[";
    for(int i = 0; i < N; ++i) {
        if(i > 0)
            cout << ",";
        cout << r[i];
    }
    cout << "]" << endl;
}

double length() {
    double l = r[0] + r[N-1];
    for(int j = 1; j < N; ++j)
        l += dist(j-1,j);
    return l;    
}

void swap(int i, int j) {
    double s = r[i];
    r[i] = r[j];
    r[j] = s;    
}

void permute() {
    for(int i = 0; i < N; ++i)
        r[i] = 30 + i;
    for(int i = 0; i < N; ++i) {
        int j = rand() % (N-i) + i;
        swap(i,j);
    }
}

bool improve() {
    bool changed = false;
    for(int i = 3; i < N; ++i) {
        if(bad_pattern(i)) {
            swap(i-2,i-1);
            changed = true;
        }
    }
    return changed;
}

void special() {
    for(int i = 0; i <= 10; ++i)
        r[i] = 50 - 2*i;
    for(int i = 20; i > 10; --i)
        r[i] = 2*i + 9;
}

int main() {
    srand(time(NULL));
//    while(true) {
        special();
        //permute();
        print();
        while(improve());
        double len = length();
        if(len < minimum) {
            minimum = len;
            cout << "New minimum: " << (int) round(1000*minimum) << " ";
            print();
        }
//    }
}