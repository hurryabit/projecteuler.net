// problem213.cc

#include <iostream>

using namespace std;

const size_t DIM = 30;
const size_t ROUNDS = 50;

int main() {
    double empty[DIM][DIM];
    for( size_t x = 0; x < DIM; ++x )
        for( size_t y = 0; y < DIM; ++y )
            empty[x][y] = 1;

    double distr[DIM][DIM];
    for( size_t gx = 0; gx < DIM; ++gx ) {
        for( size_t gy = 0; gy < DIM; ++gy ) {
            // init distr
            for( size_t lx = 0; lx < DIM; ++lx )
                for( size_t ly = 0; ly < DIM; ++ly )
                    distr[lx][ly] = 0;
            distr[gx][gy] = 1.0;

            // advance distr ROUNDS times
            size_t sy = (gx+gy) % 2;
            for( size_t r = 0; r < ROUNDS; ++r ) {
                for( size_t lx = 0; lx < DIM; ++lx ) {
                    for( size_t ly = (lx+sy) % 2; ly < DIM; ly += 2) {
                        double prob = distr[lx][ly] / (4 - (lx == 0 || lx == DIM-1) - (ly == 0 || ly == DIM-1));
                        distr[lx][ly] = 0;
                        if( lx > 0 )
                            distr[lx-1][ly] += prob;
                        if( lx < DIM-1 )
                            distr[lx+1][ly] += prob;
                        if( ly > 0 )
                            distr[lx][ly-1] += prob;
                        if( ly < DIM-1 )
                            distr[lx][ly+1] += prob;
                    }
                }
                sy = 1-sy;
            }

            // promote distr to empty
            for( size_t lx = 0; lx < DIM; ++lx ) {
                for( size_t ly = (lx+sy) % 2; ly < DIM; ly += 2) {
                    empty[lx][ly] *= 1-distr[lx][ly];
                }
            }
        }
    }

    double expect = 0;
    for( size_t x = 0; x < DIM; ++x )
        for( size_t y = 0; y < DIM; ++y )
            expect += empty[x][y];

    cout.precision(9);
    cout << expect << endl;
}