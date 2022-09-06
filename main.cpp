#include <iostream>
#include <unordered_map>
#include <vector>

#define LAST stack[stack.size() - 1]
#define FIRST LAST * 2
#define SECOND (LAST - 1) / 3
#define PASS ((LAST - 1) % 3 == 0)
#define CONTINUE map[i] = LAST; stack.push_back(i); continue;
#define ROLLBACK stack.pop_back(); continue;

int main() {
    std::unordered_map<int, int> map;
    std::vector<int> stack = { 1, 2 };

    map[1] = 2;

    while (true) {
        if (stack.size() == 1) {
            break;
        }

        if (LAST > 2000000) {
            // std::cout << LAST << " -> length: " << stack.size() << std::endl;
            map[LAST] = -1;
            ROLLBACK;
        }
        
        int i;
        
        i = FIRST;
        if (!map.count(i)) {
            CONTINUE;
        }

        if (!PASS) {
            ROLLBACK;
        }

        i = SECOND;
        if (!map.count(i)) {
            CONTINUE;
        }

        ROLLBACK;
    }

    int count = 0;

    for (int i = 1; i < 1000000; i++) {
        if (!map.count(i)) {
            std::cout << i << " ";
            count++;
        }
    }

    std::cout << count << "\n";
}
