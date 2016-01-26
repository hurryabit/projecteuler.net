// problem238.cc

#include <cstdio>
#include <cstdlib>
#include <cstring>

char digits[20000000];
size_t length;

struct state {
    unsigned long value;
    size_t next;
    unsigned long count;
};

void init_digits() {
    unsigned long s0 = 14025256, sn = s0;
    char *pos = digits;

    do {
        sprintf(pos, "%lu", sn);
        while( *pos != '\0' ) {
            *pos -= '0';
            ++pos;
        }
        sn = (sn*sn) % 20300713;
    } while( sn != s0 );

    length = pos - digits;
}

int main(int argc, char *argv[]) {
    const unsigned long LIMIT = (unsigned long) atoi(argv[1]);

    unsigned long count = 0;
    for( unsigned long i = 0; i < LIMIT; ++i )
        count += i % 3;
    printf("%lu\n", count);
    return 0;

    init_digits();

    state *memory = new state[length];
    for( size_t i = 0; i < length; ++i ) {
        memory[i].value = 0;
        memory[i].next = i;
        memory[i].count = 0;
    }

//    printf("go!\n");
    unsigned long sum = 0;
    for( unsigned long cur = 1; cur <= LIMIT; ++cur ) {
        bool not_found = true;
        size_t mem = 0;
        while( not_found && mem < 50 /* length */ ) {
            state st = memory[mem];
            while( st.value < cur) {
                st.value += digits[st.next];
                ++(st.next);
                if( st.next == length )
                    st.next = 0;
            }
//            printf("memory value %lu at %lu\n", st.value, mem);

            if( st.value == cur ) {
                not_found = false;
                sum += (mem+1);
                ++(st.count);
//                printf("found %lu at %lu\n", cur, mem);
            }
            memory[mem] = st;

            ++mem;
        }

        if( not_found )
            printf("could not find %lu\n", cur);
    }

    
/*    unsigned long count = 0;
    for( size_t i = 0; i < 100; ++i ) {
        count += memory[i].count;
        printf("%3lu: %.2f%% (%10lu)\n", i+1, (double) 100 * count / LIMIT, count);
    }*/
    printf("%lu\n", sum);
}
