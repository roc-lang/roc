package javaSource;

public class HelloJNI {
   static {
      System.loadLibrary("demo");
   }
   public static native String sayHello(int num);

   public static void main(String[] args) {
      System.out.println(sayHello(420));
   }
}
