// problem201.cc

#include <iostream>
#include <map>
#include <vector>

using namespace std;

int main() {
    vector<map<unsigned,unsigned>> memory(338351);

    memory[0].insert(make_pair(0,1));

    unsigned max_reach = 0;
    for( unsigned r = 51; r <= 100; ++r ) {
        cout << "round " << r-50 << " starts" << endl;

        max_reach += r*r;
        for( unsigned i = max_reach; i > 0; --i ) {
            memory[i].clear();
            for( unsigned n = r-50; n <= r && n*n <= i; ++n ) {
                unsigned count = 0;
                for( const auto &mi: memory[i-n*n] ) {
                    if( mi.first < n )
                        count += mi.second;
                }
                if( count > 0 )
                    memory[i].insert(make_pair(n,count > 1 ? 2 : 1));
            }
        }
        memory[0].clear();
        cout << "round " << r-50 << " finished" << endl;
    }

    long unsigned result = 0;
    for( unsigned i = 1; i <= memory.size(); ++i ) {
        const auto& mi = memory[i];
        if( mi.size() == 1 && mi.begin()->second == 1 )
            result += i;
    }

    cout << result << endl;
}
