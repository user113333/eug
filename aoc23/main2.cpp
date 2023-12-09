#include <bits/stdc++.h>

using namespace std;
#define ll long long

// AOC23 day 1 part 2

string digits[] = { "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };

string parse(string s) {
    string out = "";
    while (s != "") {
        bool found = false;
        for (int i = 0; i < 9; i++) {
            if (s.compare(0, digits[i].length(), digits[i]) == 0) {
                out += '0' + 1 + i;
                s.erase(0, 1);
                found = true;
            }
        }
        if (found) {
            continue;
        }
        out += s[0];
        s.erase(0, 1);
    }
    return out;
}

int cal_value(string s) {
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
    cout << s << " " << (f * 10 + l) << endl;
    return f * 10 + l;
}

int main() {
    string s;
    int sum = 0;
    while (cin >> s) {
        sum += cal_value(parse(s));
    }
    cout << sum << endl;
    return 0;
}
