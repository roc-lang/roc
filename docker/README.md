# Official Docker Images

The latest official Docker images are available on the [Docker Hub](https://hub.docker.com/u/roclang).

For example, if you run the following command in a terminal, Docker will pull the latest `nightly-ubuntu-2204` image from the Docker Hub and start an interactive container based on this image, running the `roc repl` command:

```bash
docker run -it --rm roclang/nightly-ubuntu-2204 /usr/lib/roc/roc repl
```
