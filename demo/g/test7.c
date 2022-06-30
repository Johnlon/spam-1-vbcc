
void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int main() {

    int value[] ={ 0x01020304, 0x05060708 };
    //int value[] ={ 0x01020304 };

//    int * ooo = value;

    int value1 = value[0];
    int value2 = value[1];

/*    if (value[0] != 1234567890) {
      halt(1);
    }
    if (value[1] != 987654321) {
      halt(2);
    }
*/

//    halt(0);
}
