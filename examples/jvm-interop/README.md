# JVM interop
This is a demo for calling Roc code from Java, and some other JVM languages.


## Prerequisites

The following was tested on NixOS, with `openjdk 17.0.5` and `clang 13.0.1` but should work with most recent versions of those (jdk>=10) on most modern Linux and MacOS.\
You're welcome to test on your machine and tell me (via [Zulip](https://roc.zulipchat.com/#narrow/pm-with/583319-dank)) if you ran into any issues or limitations.

## Goal
We'll make a few examples, showing basic data type convertions and function calls between Roc and Java (and later some other JVM languages):
- A string formatter.
- A Function that multiples an array by a scalar.
- A factorial function that, for the sake of demonstration, throws a RuntimeException for negative integers.

This will be done with the help of [Java Native Interface](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/).
We will be using C to bridge between Java and Roc.

## Structure
As the time of writing this post, the following is the current bare bones tree of a jvm-interop:

``` console
.
├── impl.roc
├── platform.roc
├── bridge.c
└── javaSource
    └── Demo.java
```

impl.roc is the application where we actually implement our native Roc functions.\
platform.roc as the name suggests contains platform logic, (but doesn't really have much here, mostly just) exposes functions to the host - bridge.c\
bridge.c is the JNI bridge, it's the host that implements the Roc functions (e.g roc_alloc) and the JNI functions that act like the bridge between Roc and Java (bridge as in, doing type conversions between the languages, needed jvm boilerplate, etc). 

For each of our native Roc functions, in the application (impl.roc), we have a corresponding `Java_javaSource_Demo_FUNC` C function that handles the "behind the scenes", this includes type conversions between the languages, transforming roc panics into java exceptions and basically all the glue code necessary.


Just so you know what to expect, our Roc functions look like this;
``` coffee
interpolateString : Str -> Str
interpolateString = \name ->
    "Hello from Roc \(name)!!!🤘🤘🤘"


mulArrByScalar : List I32, I32 -> List I32
mulArrByScalar  = \arr, scalar ->
    List.map arr \x -> x * scalar


factorial : I64 -> I64
factorial = \n ->
    if n < 0 then
        # while we get the chance,  examplify a roc panic in an interop
        crash "No negatives here!!!"
    else if n == 0 then
        1
    else
        n * (factorial (n - 1))
```

Nothing too crazy. Again, do note how we crash if n < 0, see how this would play out from the Java side. 

Now let's take a quick look on the Java side of things;

``` java
public class Demo {

   static {
      System.loadLibrary("interop");
   }

   public static native String sayHello(String num);

   public static native int[] mulArrByScalar(int[] arr, int scalar);

   public static native long factorial(long n) throws RuntimeException;


   public static void main(String[] args) {

      // string demo
      System.out.println(sayHello("Brendan") + "\n");

      // array demo
      int[] arr = {10, 20, 30, 40};
      int x = 3;
      System.out.println(Arrays.toString(arr) +
                         " multipled by " + x +
                         " results in " + Arrays.toString(mulArrByScalar(arr, x)) +
                         "\n");

      // number + panic demo
      long n = 5;
      System.out.println("Factorial of " + n + " is " + factorial(n));

   }
}
```
First we load our library - "interop", which is a shared library (`.so` file) that our Roc+C code compiles to.\
Then, we declare our native functions with suitable types and throws annotation.\
Finally in main we test it out with some inputs.

## See it in action
##### For brevity's sake we'll run the build script and omit some of its (intentionally) verbose output:

```console
[nix-shell:~/dev/roc/examples/jvm-interop]$ ./build.sh && java javaSource.Greeter
Hello from Roc Brendan!!!🤘🤘🤘

[10, 20, 30, 40] multipled by 3 results in [30, 60, 90, 120]

Factorial of 5 is 120
```
That's pretty cool!\
Let's also see what happens if in the code above we define n to be -1:
``` console
[nix-shell:~/dev/roc/examples/jvm-interop]$ ./build.sh && java javaSource.Greeter
Hello from Roc Brendan!!!🤘🤘🤘

[10, 20, 30, 40] multipled by 3 results in [30, 60, 90, 120]

Exception in thread "main" java.lang.RuntimeException: No negatives here!!!
	at javaSource.Demo.factorial(Native Method)
	at javaSource.Demo.main(Demo.java:36)
```
And as we expected, it runs the first two examples fine, throws a RuntimeException on the third.

Since we're talking JVM Bytecode, we can pretty much call our native function from any language that speaks JVM Bytecode.

Note: The JNI code depends on a dynamic lib, containing our native implementation, that now resides in our working directory.\
So in the following examples, we'll make sure that our working directory is in LD_LIBRARY_PATH.\
Generally speaking, you'd paobably add your dynamic library to a spot that's already on your path, for convenience sake.\
So first, we run:

```console
[nix-shell:~/dev/roc/examples/jvm-interop]$ export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH
```

Now, let's try Kotlin!
```console
[nix-shell:~/dev/roc/examples/jvm-interop]$ kotlin
Welcome to Kotlin version 1.7.20 (JRE 17.0.5+8-nixos)
Type :help for help, :quit for quit

>>> import javaSource.Demo

>>> Demo.sayHello("Kotlin Users")
res1: kotlin.String = Hello from Roc Kotlin Users!!!🤘🤘🤘

>>> Demo.mulArrByScalar(intArrayOf(10, 20, 30, 40), 101).contentToString()
res2: kotlin.String = [1010, 2020, 3030, 4040]

>>> Demo.factorial(10)
res3: kotlin.Long = 3628800
```
And it just works, out of the box!

Now let's do Scala

```console
[nix-shell:~/dev/roc/examples/jvm-interop]$ scala
Welcome to Scala 2.13.10 (OpenJDK 64-Bit Server VM, Java 17.0.5).
Type in expressions for evaluation. Or try :help.

scala> import javaSource.Demo
import javaSource.Demo

scala> Demo.sayHello("Scala Users")
val res0: String = Hello from Roc Scala Users!!!🤘🤘🤘

scala> Demo.mulArrByScalar(Array(10, 20, 30, 40), 1001)
val res1: Array[Int] = Array(10010, 20020, 30030, 40040)

scala> Demo.factorial(-2023)
java.lang.RuntimeException: No negatives here!!!
  at javaSource.Demo.factorial(Native Method)
  ... 32 elided
```
And it also works beautifully.

Last one - Clojure
Do note that in Clojure you need to add a `-Sdeps '{:paths ["."]}'` flag to add the working directory to paths.
``` console
[nix-shell:~/dev/roc/examples/jvm-interop]$ clj -Sdeps '{:paths ["."]}'
Clojure 1.11.1
user=> (import 'javaSource.Demo)
javaSource.Demo

user=> (Demo/sayHello "Clojure Users")
"Hello from Roc Clojure Users!!!🤘🤘🤘"

user=> (seq (Demo/mulArrByScalar (int-array [10 20 30]) 9)) ; seq to pretty-print
(90 180 270)

user=> (Demo/factorial 15)
1307674368000
```

Test it out on your favorite JVM lang!\
And again, if anything goes not according to plan, tell me in the link above and we'll figure it out.

## Notes on building
The process is basically the following:
1. Build our application + platform .roc files with (`roc build impl.roc --no-link`) into an object file
2. Generate a C header file (for bridge.c's) using java.
3. Bundle up the C bridge together with our object file into a shared object.

And that's it, use that shared object from your JVM language. Note every JVM language has its own way to declare that native library so you may want to look at it, or do like in the demo and declare it in java and use the binding from anywhere.

I suggest reading the build script (build.sh) and adjusting according to your setup.
