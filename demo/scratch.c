
#include "stdio.h"
#include "stdint.h"

main() {
  printf("#%02x\n", (int8_t)1);
  printf("#%02x\n", (int8_t)127);
  printf("#%02x\n", (int8_t)128);
  printf("#%02x\n", (int8_t)255);
  printf("#%02x\n", 255);

//  printf("c  #%02x\n", (char)255);
//  printf("uc #%02x\n", (unsigned char)255);
}
