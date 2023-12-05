#include <bits/stdc++.h>
using namespace std;

string prev;
string next;

bool issign(char c) {
    return c == '*' || c == '#' || c == '$' || c == '+' || c == '%' || c == '&' || c == '*' || c == '-' || c == '/' || c == '/' || c == '=' || c == '@';
}

void find_signs(vector<string> lines) {
    set<char> signs;
    for (auto& line : lines) {
        for (int i = 0; i < line.size(); i++) {
            if (line[i] != '.' && !isdigit(line[i])) {
                signs.insert(line[i]);
            }
        }
    }

    for (auto& c : signs) {
        cout << c << " ";
    }
    cout << endl;
}

#define chk(X) if (issign(X)) { collision = true; }

int parse(vector<string> lines) {
    int sum = 0;
    int digit_start = -1;
    bool collision = false;
    for (int i = 0; i < lines.size(); i++) {
        string line = lines[i];
        for (int j = 0; j < line.size(); j++) {
            if (isdigit(line[j])) {
                if (digit_start == -1) {
                    digit_start = j;
                }
                chk(lines[i-1][j]);
                chk(lines[i+1][j]);
                chk(lines[i-1][j-1]);
                chk(lines[i][j-1]);
                chk(lines[i+1][j-1]);
                chk(lines[i-1][j+1]);
                chk(lines[i][j+1]);
                chk(lines[i+1][j+1]);
            } else {
                if (digit_start != -1) {
                    string ss = line.substr(digit_start, j - digit_start);
                    int num = atoi(ss.c_str());
                    if (collision) {
                        sum += num;
                    }
                    digit_start = -1;
                    collision = false;
                }
            }
        }
    }
    return sum;
}

int main() {
    string line;
    vector<string> lines;
    while (cin >> line) {
        lines.push_back("." + line + ".");
    }
    lines.push_back(string(lines[0].size(), '.'));
    lines.insert(lines.begin(), string(lines[0].size(), '.'));
    if (false) {
        find_signs(lines);
    } else {
        cout << parse(lines) << endl;
    }
    return 0;
}
