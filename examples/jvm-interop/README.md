# JVM interop
This is a demo for calling Roc code from Java, and some other JVM languages.


## Prerequisites

The following was tested on NixOS, with `openjdk 17.0.5` and`clang 13.0.1` but should work with most recent versions of those (jdk>=10) on most modern Linux and MacOS.\
You're welcome to test on your machine and tell me (via [Zulip](https://roc.zulipchat.com/#narrow/pm-with/583319-dank)) if you ran into any issues or limitations.

## Goal
Our goal here is quite simple- have java take in a number -> pass it to Roc -> Roc formats a string with the number -> pass the string back to java.
This will be done with the help of [Java Native Interface](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/).
We will be using C to bridge between Java and Roc.

## Structure
As the time of writing this post, the following is the current bare bones tree of a jvm-interop:

``` sh
 .
├──  bridge.c
├──  javaSource
│  └──  Greeter.java
├──  main.roc          # application main
└──  platform
   ├──  host.c
   └──  main.roc       # main for host
```

bridge.c is the JNI bridge. The interesting part of it is the function `Java_javaSource_Greeter_sayHello`, this function will accept a `jint` and return a `jstring`.

In this function, the number, encoded as bytes, will be passed to the platform.\
The platform will then pass the number to the application which in turn will create our newly formatted string. Just so you know what to expect, the formatting function looks like this:
``` coffee
main : U64 -> Str
main = \num ->
    if num == 0 then
        "I need a positive number here!"
    else
        str = Num.toStr num
        "The number was \(str), OH YEAH!!! 🤘🤘"
```

The Roc string, formatted with the java integer, will then be converted into a Java String the JVM could understand.

I mentioned the C code will accept a number, but let's step back and see how we declare our native C function and pass the number to it, from Java:

``` java
package javaSource;

public class Greeter {
   static {
      System.loadLibrary("interop"); // this loads the dynamic library created from our JNI code!
   }
   public static native String sayHello(int num);

   public static void main(String[] args) {
      System.out.println(sayHello(420));
   }
}

```


## See it in action
##### For brevity's sake we'll run the build script and ommit some of its (intentionally) verbose output:

```console
[dankey@computer:~/dev/roc/examples/jvm-interop]$ ./build.sh && java javaSource.Greeter
The number was 420, OH YEAH!!! 🤘🤘
```
That's pretty cool!\
Since we're talking JVM Bytecode, we can pretty much call our native function from any language that speaks JVM Bytecode.

Note: The JNI code depends on a dynamic lib, containing our native implementation, that now resides in our working directory.\
So in the following examples, we'll make sure that our working directory is in LD_LIBRARY_PATH.\
I generally speaking, you'd paobably add your dynamic library to a spot that's already on your path, for convenience sake.\
So first, we run:

```console
[nix-shell:~/dev/roc/examples/jvm-interop]$ export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH
```

Now, let's try Kotlin!
```console
[nix-shell:~/dev/roc/examples/jvm-interop]$ kotlin
Welcome to Kotlin version 1.7.20 (JRE 17.0.5+8-nixos)
Type :help for help, :quit for quit
>>> import javaSource.Greeter
>>> Greeter.sayHello(69)
res1: kotlin.String = The number was 69, OH YEAH!!! 🤘🤘
```
And it just works, out of the box!

Now let's do Scala

```console
[nix-shell:~/dev/roc/examples/jvm-interop]$ scala
Welcome to Scala 2.13.10 (OpenJDK 64-Bit Server VM, Java 17.0.5).
Type in expressions for evaluation. Or try :help.

scala> import javaSource.Greeter
import javaSource.Greeter

scala> Greeter.sayHello(1337)
val res0: String = The number was 1337, OH YEAH!!! 🤘🤘

```
And it also works beautifully.

Test it out on your favorite JVM lang!\
And again, if anything goes not according to plan, tell me in the link above and we'll figure it out.

## Notes on building
I suggest reading the build script and uncommenting according to your setup.\
But one note on something that may not be obvious:\
As of the time of writing this document, `roc build --lib` generates a shared object with the suffix `.so.1.0`.\
This `.0` suffix is unneeded in any part of the build, so we can simply rename it.
But one does depend on `libhello.so` (without `.1`), so we symlink into it.


