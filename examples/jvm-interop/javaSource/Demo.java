package javaSource;

import java.util.Arrays;

public class Demo {

   static {
      System.loadLibrary("interop");
   }

   public static native String sayHello(String num);

   public static native int[] mulArrByScalar(int[] arr, int scalar);

   public static native long factorial(long n);


   public static void main(String[] args) {

      // string demo
      System.out.println(sayHello("Brendan") + "\n");

      // array demo
      int[] arr = {10, 20, 30, 40};
      int x = 3;
      System.out.println("Array " + Arrays.toString(arr) +
                         " multipled by " + x +
                         " results in " + Arrays.toString(mulArrByScalar(arr, x)) +
                         "\n");

      // number + panic demo
      // This can be implemented more peacefully but for sake of demonstration-
      // this will panic from the roc side if n is negative
      // and in turn will throw a JVM RuntimeException
      long n = -1;
      System.out.println("Factorial of " + n + " is " + factorial(n) + "\n");

   }
}
