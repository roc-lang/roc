FROM debian:latest

RUN apt-get update --fix-missing
RUN apt-get upgrade --yes

RUN apt-get install --yes wget

# you can leave out libc-dev and binutils if you don't need the REPL
RUN apt-get install --yes libc-dev binutils

RUN wget -q -O roc.tar.gz https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-linux_x86_64-latest.tar.gz

RUN mkdir /usr/lib/roc

RUN tar -xvz -f roc.tar.gz --directory /usr/lib/roc --strip-components=1

ENV PATH="$PATH:/usr/lib/roc"
