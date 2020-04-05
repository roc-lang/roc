struct triple {
   double a;
   double b;
   double c;
};

struct triple f(void) {
   struct triple s;

   s.a = 1.1;
   s.b = 2.2;
   s.c = 3.3;

   return s;
}

int main(void)
{
   struct triple s = f();

   return 0;
}
