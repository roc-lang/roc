package javaSource;

public class Greeter {
   static {
      System.loadLibrary("interop");
   }
   public static native String sayHello(int num);

   public static void main(String[] args) {
      System.out.println(sayHello(420));
   }
}
