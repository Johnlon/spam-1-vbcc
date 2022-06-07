
int sub(int subParamA, int subParamB, int subParamC, int subParamD, int subParamE, int subParamF   )
{
  int t = sub(subParamA, subParamB, subParamC, 444,555,666);
  return t;
}

int main() {
    int o = sub(11, 22, 33, 44, 55, 66);
    return o;
}

