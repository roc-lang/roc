FROM debian:bullseye-slim
WORKDIR /earthbuild

prep-debian:
    RUN apt -y update

install-script:
    FROM +prep-debian
    COPY developer_install.sh ./
    RUN ./developer_install.sh