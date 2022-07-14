
void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int main() {

    short value[] ={ 0x0102, 0x0506 };
    int valueB[] ={ 1, 2,3,4,5,6 };
    //int value[] ={ 0x01020304 };

//    int * ooo = value;

    int value1 = value[0];
    int value2 = value[1];

  int b1 = valueB[0];
  int b2 = valueB[1];

/*    if (value[0] != 1234567890) {
      halt(1);
    }
    if (value[1] != 987654321) {
      halt(2);
    }
*/

//    halt(0);
}
