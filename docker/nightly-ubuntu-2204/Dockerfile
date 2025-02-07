FROM ubuntu:22.04

RUN apt-get update --fix-missing
RUN apt-get upgrade --yes

RUN apt-get install --yes wget

# you can leave out libc-dev and binutils if you don't need the REPL
RUN apt-get install --yes libc-dev binutils

RUN wget -q -O roc.tar.gz https://github.com/roc-lang/roc/releases/download/0.0.0-alpha2-rolling/roc-linux_x86_64-0-alpha2-rolling.tar.gz

RUN mkdir /usr/lib/roc

RUN tar -xvz -f roc.tar.gz --directory /usr/lib/roc --strip-components=1

ENV PATH="$PATH:/usr/lib/roc"
