#include "stdio.h"

main() {

    int R = 1;
    int D = 2;
    int r = 0; int d = 0;

    r = 0; d = 0; printf("r=%d d=%d r|d==r=%d\n", r, d, (r|d)==r);
    r = 0; d = D; printf("r=%d d=%d r|d==r=%d\n", r, d, (r|d)==r);
    r = R; d = 0; printf("r=%d d=%d r|d==r=%d\n", r, d, (r|d)==r);
    r = R; d = D; printf("r=%d d=%d r|d==r=%d\n", r, d, (r|d)==r);
}

