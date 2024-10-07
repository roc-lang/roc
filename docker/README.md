# Official Docker Images

The latest [official Docker images](https://hub.docker.com/u/roclang) are available on the Docker Hub.

For example, to pull the latest `nightly-ubuntu-2204` image from the Docker Hub, run the following command in a terminal (assuming you have already installed [Docker](https://docs.docker.com/engine/install/)):

```bash
docker pull roclang/nightly-ubuntu-2204
```

To start an interactive container based on this image, run:

```bash
docker run -it --rm roclang/nightly-ubuntu-2204
```

Note: the `--rm` option will delete the container when you exit. Remove this option is you want to keep the container instead.

Inside the container, you can use the `roc` command directly, for example:

```bash
roc version
roc repl
```

Alternatively, you can start a container and run a Roc REPL in one shot:

```bash
docker run -it --rm roclang/nightly-ubuntu-2204 /usr/lib/roc/roc repl
```

If you want to create you own Docker image based on this image, add the following in your `Dockerfile`:

```Dockerfile
FROM roclang/nightly-ubuntu-2204:latest
```
