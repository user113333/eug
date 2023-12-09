#include <bits/stdc++.h>
using namespace std;

// AOC23 day 3 part 2

string prev;
string next;

bool issign(char c) {
    return c == '*';
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

struct Point {
    int i, j;
    bool operator<(const Point& rhs) const {
        if (i == rhs.i) return j < rhs.j;
        return i < rhs.i;
    }
    bool operator==(const Point& rhs) const {
        return i == rhs.i && j == rhs.j;
    }
};

struct GearNum {
    Point gear;
    int num;
};

#define chk(I,J) if (issign(lines[I][J])) { gears_set.insert({I, J}); collision = true; }

int parse(vector<string> lines) {
    int sum = 0;
    int digit_start = -1;
    set<Point> gears_set;
    vector<GearNum> gearnum;
    bool collision = false;
    for (int i = 0; i < lines.size(); i++) {
        string line = lines[i];
        for (int j = 0; j < line.size(); j++) {
            if (isdigit(line[j])) {
                if (digit_start == -1) {
                    digit_start = j;
                }
                chk(i-1,j);
                chk(i+1,j);
                chk(i-1,j-1);
                chk(i,j-1);
                chk(i+1,j-1);
                chk(i-1,j+1);
                chk(i,j+1);
                chk(i+1,j+1);
            } else {
                if (digit_start != -1) {
                    string ss = line.substr(digit_start, j - digit_start);
                    int num = atoi(ss.c_str());
                    if (collision) {
                        for (auto& g : gears_set) {
                            gearnum.push_back({g,num});
                        }
                    }
                    digit_start = -1;
                    collision = false;
                    gears_set.clear();
                }
            }
        }
    }
    for (int i = gearnum.size() - 1; i >= 0; i--) {
        int count = 1;
        long long prod = gearnum[i].num;
        for (int j = i - 1; j >= 0; j--) {
            if (gearnum[i].gear == gearnum[j].gear) {
                count++;
                prod = prod * gearnum[j].num;
            }
        }
        if (count == 2) {
            sum += prod;
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
