/*int glob1;
void pop() {
    int pop1 = 1;
    glob1 = pop1;

}
*/

int main() {
    long myLong=0xbeaf;
    long yourLong=0xfefe;

    if (myLong == yourLong) {
        myLong=0xaa;
    }
    else 
    {
        myLong=0xbb * yourLong;
    }
    return 99;

  //  return myInt;
}

