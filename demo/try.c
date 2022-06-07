
int bar(long a, long b)
{
    long x, y;
    long p=1,q=2,r=3,s=4,t=5;

    x = 555;
    y = a+b+x + p + q + r + s +t;;
    return y;
}

int foo(void) {
  return bar(111,222);
}
