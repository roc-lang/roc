FROM debian:buster

RUN apt-get update --fix-missing
RUN apt-get upgrade --yes
RUN apt-get install --yes \
    wget \
    libc-dev \
    binutils \
    build-essential \
    clang
RUN wget -q -O roc.tar.gz https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-old_linux_x86_64-latest.tar.gz
RUN mkdir /usr/lib/roc
# --strip-components=1 not needed once https://github.com/roc-lang/roc/issues/4118 is solved.
RUN tar -xvz -f roc.tar.gz --directory /usr/lib/roc --strip-components=1

ENV PATH="$PATH:/usr/lib/roc"
