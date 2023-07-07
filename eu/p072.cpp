#include <bits/stdc++.h>
using namespace std;

#define ll long long

int main() {
    vector<int> cp(1000001);
    ll sum = 0;

    for (int i = 2; i < 1000001; i++) {
    /* for (int i = 2; i < 5; i++) { */
        if (cp[i] != 0) {
            sum += cp[i];
            continue;
        } else {
            sum += i - 1;
        }

        ll pk = (ll)i;
        while (pk <= 1000000) {
            cp[pk] = pk - (pk / i);

            for (int j = 2; j <= floor(1000000.0 / pk); j++) {
                ll n = j * pk;
                if (cp[j] == 0 || j % i == 0) {
                    continue;
                }

                cp[n] = cp[j] * cp[pk];
            }

            pk *= i;
        }
    }

    cout << sum << endl;
}
