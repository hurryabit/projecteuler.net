// problem308.cc

#include <iostream>

using namespace std;

typedef long unsigned int nat_t;

#define TIC(steps) (pc += (steps))

int main() {
    nat_t x0 = 1, x1 = 0, x2 = 0, x3 = 0, pc = 0, primes = 0;

    do {
        TIC(x0+x3+1);
        x1 = x0;
        x2 = x0+1;
        x0 = 0;
        x3 = 0;

        bool gotoA = true;
        while( gotoA ) {
            cout << "    " << x0 << "  " << x1 << "  " << x2 << "  " << x3 << endl;
            TIC(2*x1+2);
            x3 += x1;
            x1 = min(x2,x3);
            x0 += x1;

            if( x2 >= x3) {
                TIC(2*x3);
                x2 -= x3;
                x3 = 0;
            }
            else {
                TIC(2*x2+1);
                x3 -= (x2+1);
                x2 = 0;

                if( x1 > 0 ) {
                    TIC(2*x0+1);
                    --x1;
                    x2 += x0;
                    x0 = 0;
                    ++x3;
                }
                else {
                    gotoA = false;
                }
            }
        }

        cout << x0 << "  " << x1 << "  " << x2 << "  " << x3 << endl;
 
        if( x1 == 0 && x2 == 0 && x3 == 0 ) {
            ++primes;
            cout << primes << ":" << pc << endl;
        }
        
    } while( primes < 10 );
}
