# syntax=docker/dockerfile:1
FROM ubuntu:latest
RUN apt-get update && apt-get install -y gfortran make python3 python3-pip rsync graphviz
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/
WORKDIR /snes
COPY . .
RUN make clobber && make tests
RUN make clobber && make testl
RUN make -C docs clobber && uv run make -C docs html
CMD ["/bin/bash", "--login", "-c"]
