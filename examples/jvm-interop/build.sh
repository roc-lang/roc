#!/bin/sh

set -euxo

# don't forget to validate that $JAVA_HOME is defined, the following would not work without it!
# set it either globally or here
# export JAVA_HOME=/your/java/installed/dir
# in nixos, to set it globally, i needed to say `programs.java.enable = true;` in `/etc/nixos/configuration.nix`


../../target/release/roc build --lib
mv libhello.so.1.0 libhello.so.1
ln -sf libhello.so.1 libhello.so

# for clang's compilation
export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH

# needs jdk10 +
# "-h ." is for placing the jni.h header in the cwd.
# the "javaSource" directory may seem redundant (why not just a plain java file),
# but this is the way of java packaging
# we could go without it with an "implicit" package, but that would ache later on,
# especially with other jvm langs
javac -h . javaSource/HelloJNI.java

clang \
    -c -fPIC \
    -I"$JAVA_HOME/include" \
    -I"$JAVA_HOME/include/linux" \
    -o demo.o HelloJNI.c

clang -shared -o libdemo.so demo.o -L. -lhello

# then run
java javaSource.HelloJNI
