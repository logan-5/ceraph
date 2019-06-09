#include <stdbool.h>
#include <stdlib.h>

void expect(bool b) {
    if (!b) {
        exit(1);
    }
}
void expect_fail(bool b) {
    if (b) {
        exit(1);
    }
}
