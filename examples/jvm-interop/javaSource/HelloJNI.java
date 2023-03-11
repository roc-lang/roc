package javaSource;

public class HelloJNI {
   static {
      System.loadLibrary("demo");
   }
   public native String sayHello(int num);

   public static void main(String[] args) {
      System.out.println(new HelloJNI().sayHello(420));
   }
}
