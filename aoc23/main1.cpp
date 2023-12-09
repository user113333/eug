#include <bits/stdc++.h>

using namespace std;

// AOC23 day 1 part 1

int cal_value(string& s) {
    int f = -1;
    int l = -1;
    for (char c : s) {
        if (c >= '0' && c <= '9') {
            if (f == -1) {
                f = c - '0';
            }
            l = c - '0';
        }
    }
    return f * 10 + l;
}

int main() {
    string s;
    int sum = 0;
    while (cin >> s) {
        sum += cal_value(s);
    }
    cout << sum;
    return 0;
}
