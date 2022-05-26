/*int glob1;
void pop() {
    int pop1 = 1;
    glob1 = pop1;

}
*/
extern int externalFn(); 
extern int externalFn2(); 

int static1=1;
int static2=2;

int sub(int e) {
  int o = externalFn(); // will cause gen_var_header
  o = externalFn2(); // will cause gen_var_header
  return e;
}

int main() {
    long myLong=0xbeaf;
    long yourLong=0xfefe;

    if (myLong == yourLong) {
        myLong=0xaa;
    }
    else 
    {
        myLong=0xbb + yourLong;
    }

    int o = sub(123);
    return 99;

  //  return myInt;
}

