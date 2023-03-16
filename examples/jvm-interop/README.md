# JVM interop
This is a demo for calling Roc code from Java, and some other JVM languages.


## Prerequisites

The following was tested on NixOS, with `openjdk 17.0.5` and `clang 13.0.1` but should work with most recent versions of those (jdk>=10) on most modern Linux and MacOS.\
You're welcome to test on your machine and tell me (via [Zulip](https://roc.zulipchat.com/#narrow/pm-with/583319-dank)) if you ran into any issues or limitations.

## Goal
Our goal here is quite simple - have Java take in a number -> pass it to Roc -> Roc formats a string with the number -> pass the string back to Java.
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

For now we care about the function `Java_javaSource_Demo_sayHello`, this function will accept a `jstring` denoting the name and return a `jstring` denoting the message from Roc.

In this function, the name will be passed to the platform.\
The platform will then call the application with the given name, and we'll get back our formatted java String.\

Just so you know what to expect, the formatting function looks like this:
``` coffee
interpolateString : Str -> Str
interpolateString = \name ->
    "Hello from Roc \(name)!!!🤘🤘🤘"
```


I mentioned the C code will accept a name, but let's step back and see how we declare our native C function and pass the name to it, in Java:

``` java
package javaSource;

public class Demo {
   static {
      System.loadLibrary("interop"); // this loads the dynamic library created from our JNI code!
   }
   public static native String sayHello(String name);

   public static void main(String[] args) {
      System.out.println(sayHello("Brendan"));
   }
}

```


## See it in action
##### For brevity's sake we'll run the build script and omit some of its (intentionally) verbose output:

```console
[dankey@computer:~/dev/roc/examples/jvm-interop]$ ./build.sh && java javaSource.Greeter
Hello from Roc Brendan!!!🤘🤘🤘
```
That's pretty cool!\
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
