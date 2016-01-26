// problem161.cc

#include <cstdio>

typedef unsigned int addr_t;

const unsigned MAX_ROW = 9;
const unsigned MAX_ROW2 = 2*MAX_ROW;

const unsigned MAX_COL = 12;

const addr_t MAX_ADDR = (MAX_ROW * MAX_COL) << MAX_ROW2;

const addr_t PATTERN_MASK = (0x01 << MAX_ROW2) - 1;
const addr_t PATTERN_HIGH = 0x01 << (MAX_ROW2-1);


enum tile_t {
    RED = (0x01 << MAX_ROW) | 0x03,
    GREEN = (0x03 << MAX_ROW) | 0x01,
    BLUE = (0x02 << MAX_ROW) | 0x03,
    YELLOW = (0x03 << (MAX_ROW-1)) | 0x01,
    GRAY = 0x07,
    BLACK = (0x01 << MAX_ROW) | (0x01 << MAX_ROW2) | 0x01
};

const unsigned MAX_TILE = 6;

const tile_t TILES[] = { RED, GREEN, BLUE, YELLOW, GRAY, BLACK };

typedef long long count_t;

count_t memory[MAX_ADDR+1];

inline unsigned get_offset(const addr_t addr) {
    return addr >> MAX_ROW2;
}

inline unsigned get_pattern(const addr_t addr) {
    return addr & PATTERN_MASK;
}

inline addr_t make_addr(const unsigned offset, const unsigned pattern) {
    return offset << MAX_ROW2 | pattern;
}

inline bool invalid_addr(addr_t addr) {
    unsigned count = get_offset(addr);
    unsigned pattern = get_pattern(addr) >> 1;
    while( pattern ) {
        if( pattern & 0x01 )
            ++count;
        pattern >>= 1;
    }
    return count % 3;
}

inline addr_t advance(const addr_t addr, const tile_t tile) {
    unsigned offset = get_offset(addr);
    unsigned pattern = get_pattern(addr);
    unsigned row = offset % MAX_ROW;
    unsigned col = offset / MAX_ROW;

    if( !(((tile == RED || tile == GREEN || tile == BLUE) && row+1 < MAX_ROW && col+1 < MAX_COL) ||
            (tile == YELLOW && row > 0 && col+1 < MAX_COL) ||
                (tile == GRAY && row+2 < MAX_ROW) ||
                    (tile == BLACK && col+2 < MAX_COL)) || (pattern & tile) )
        return MAX_ADDR + 1;

    pattern |= tile;
    while( pattern & 0x01 ) {
        pattern >>= 1;
        ++offset;
    }

    return make_addr(offset, pattern);
}

int main() {
    memory[MAX_ADDR] = 1;

    for(addr_t addr = MAX_ADDR; addr > 0;) {
        addr -= 2;
        if( invalid_addr(addr) )
            continue;

        count_t res = 0;
        for(unsigned i = 0; i < MAX_TILE; ++i) {
            addr_t next = advance(addr, TILES[i]);
            if( next <= MAX_ADDR )
                res += memory[next];
        }
        memory[addr] = res;
    }

    printf("%lli\n", memory[0]);
}