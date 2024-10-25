#include <bits/stdc++.h>
using namespace std;

#define N 10000000

bool are_permutations(unsigned long long a, unsigned long long b) {
    static int stevke[10];
    for (int i = 0; i < 10; i++) {
        stevke[i] = 0;
    }
    do {
        stevke[a % 10]++;
    } while ((a /= 10) != 0);
    do {
        stevke[b % 10]--;
    } while ((b /= 10) != 0);
    for (int i = 0; i < 10; i++) {
        if (stevke[i] != 0) return false;
    }
    return true;
}

int main() {
    static unsigned long long fi[N];
    vector<unsigned long long> pra;
    fi[1] = 1;
    for (int n = 2; n < N; n++) {
        fi[n] = 0;
    }

    // sieve
    for (int p = 2; p < N; p++) {
        if (fi[p] != 0) continue; // ni pra
        pra.push_back(p);
        for (int i = 2; i * p < N; i++) {
            fi[i * p] = 1;
        }

        // izracunaj fi za vsa pra stevila
        // fi(p) = p - 1
        fi[p] = p - 1;

        // izracunaj fi za vse potence pra stevila
        // fi(p^n) = p^(n-1) * (p - 1)
        for (int n = 2; round(pow(p, n)) < N; n++) {
            fi[(int)round(pow(p, n))] = round(pow(p, n - 1)) * (p - 1);
        }
    }

    // izracunaj fi za vse potence pra stevil
    // for (unsigned long long p : pra) {
    //     for (int i = 2; round(pow(p, i)) < N; i++) {
    //         unsigned long long pnai = round(pow(p, i));
    //         // fi(n^m) = n^(m-1) * fi(n)
    //         unsigned long long fi_pnai = round(pow(p, i-1)) * (p-1);
    //         // tu lahko naredim fi(n * pnai) = fi(n) * fi(pnai)
    //         // ker gcd(n, pnai) je enak 1
    //         fi[pnai] = fi_pnai;
    //     }
    // }

    auto other = [](unsigned long long p, unsigned long long i) {
        unsigned long long pnai = round(pow(p, i));
        // fi(n^m) = n^(m-1) * fi(n)
        unsigned long long fi_pnai = round(pow(p, i-1)) * (p-1);
        for (int n = 1; n * pnai < N; n++) {
            if (n % p == 0) continue;
            // tu lahko naredim fi(n * pnai) = fi(n) * fi(pnai)
            // ker gcd(n, pnai) je enak 1
            fi[n * pnai] = fi[n] * fi_pnai;
        }
    };

    for (unsigned long long p : pra) {
        for (int i = 1; round(pow(p, i)) < N; i++) {
            other(p, i);
        }
    }

    double minimum = 100000;
    int minimum_n = 0;
    for (int n = 2; n < N; n++) {
        if (!are_permutations(n, fi[n])) continue;
        double ratio = (double)n / (double)fi[n];
        if (ratio < minimum) {
            minimum = ratio;
            minimum_n = n;
        }
    }

    cout << minimum_n << endl; // 8319823
}
