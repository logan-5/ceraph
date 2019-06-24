#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

void expect(bool b) {
    if (!b) {
        puts("'expect' failure!");
        exit(1);
    }
}
void expect_fail(bool b) {
    if (b) {
        puts("'expect_fail' failure!");
        exit(1);
    }
}

void print_int(int i) {
    printf("%d\n", i);
}
