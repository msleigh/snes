# syntax=docker/dockerfile:1
FROM ubuntu:latest
RUN apt-get update && apt-get install -y gfortran make python3 python3-pip
RUN python3 -m pip install --upgrade pip
RUN python3 -m pip install uv
WORKDIR /snes
COPY . .
RUN make clobber && make tests
RUN make clobber && make testl
RUN make -C docs clobber && make -C docs html
CMD ["/bin/bash", "--login", "-c"]
