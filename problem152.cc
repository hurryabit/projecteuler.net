// problem152.cc

#include <iostream>
#include <vector>

using namespace std;

const unsigned NUMBERS[] = { 2,3,4,5,6,7,8,9,10,12,14,15,16,18,20,21,24,25,27,28,30,32,35,36,40,42,45,48,49,50,54,56,60,63,64,70,72,75,80 };
const unsigned DENOM = 2116800;
    
int main() {
    vector<unsigned> memory(DENOM/2+1, 0);
    memory[0] = 1;

    
}