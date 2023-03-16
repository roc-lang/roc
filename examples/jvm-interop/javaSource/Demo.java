package javaSource;

public class Demo {
   static {
      System.loadLibrary("interop");
   }
   public static native String sayHello(String num);
   // public static native int[] mularr(int[] arr);

   public static void main(String[] args) {
      System.out.println(sayHello("Brendan"));
      // int[] arr = {10, 20, 30, 40};
      // System.out.println(mularr(arr));
   }
}
