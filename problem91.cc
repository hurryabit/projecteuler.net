// problem91.cc

#include <iostream>

using namespace std;

static const int BOUND = 50;

int main() {
    int counter = 0;

    for (int x1 = 0; x1 <= BOUND; ++x1) {
        for (int y1 = 0; y1 <= BOUND; ++y1) {
            for (int x2 = 0; x2 <= BOUND; ++x2) {
                for (int y2 = 0; y2 <= BOUND; ++y2) {
                    if ((x1 != 0 || y1 != 0) && (x2 != 0 || y2 != 0) &&
                            (x1 != x2 || y1 != y2)) {
                        int a2 = x1*x1+y1*y1;
                        int b2 = x2*x2+y2*y2;
                        int c2 = (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2);
                        int d2;
                        if (a2 > b2) {
                          d2 = a2;
                          a2 = b2;
                          b2 = d2;
                        }
                        if (b2 > c2) {
                          d2 = b2;
                          b2 = c2;
                          c2 = d2;
                        }
                        if (a2+b2 == c2) {
                            ++counter;
                        }
                    }
                }
            }
        }
    }

    counter /= 2;
    cout << counter << endl;
    return 0;
}
