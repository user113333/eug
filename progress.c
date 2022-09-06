#include <stdio.h>

static const int length = 20;

void progress(float val) {
    int ar = 0;

    printf("[");
    for (int i = 0; i < length; i++) {
        if ((float)i / length <= val) {
            printf("=");
        } else {
            if (!ar) {
                printf(">");
                ar = 1;
            } else {
                printf(".");
            }
        }
    }
    printf("] ");

    printf("%.0f\n", val * 100);
}
