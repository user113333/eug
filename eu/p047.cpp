#include <iostream>

using namespace std;

#define DPF_LIMIT 1000000
unsigned int dpf[DPF_LIMIT];

unsigned int sol() {
    for (unsigned int i = 0; i < DPF_LIMIT; i++)
        dpf[i] = 0;
    for (unsigned int i = 2; i < DPF_LIMIT; i++) {
        if (dpf[i] != 0) continue;
        for (unsigned int pp = i; pp < DPF_LIMIT; pp += i) {
            dpf[pp]++;
        }
    }
    for (unsigned int i = 0; i < DPF_LIMIT-3; i++) {
        if (dpf[i+0] == 4 &&
            dpf[i+1] == 4 &&
            dpf[i+2] == 4 &&
            dpf[i+3] == 4) {
            return i;
        }
    }
    return 0;
}

int main()
{
    cout << sol() << endl;
    return 0;
}
