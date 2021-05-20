# syntax=docker/dockerfile:1
FROM ubuntu:latest
RUN apt-get update && apt-get install -y gfortran make
WORKDIR /snes
COPY . .
RUN make clean && make tests
RUN make clean && make testl
CMD ["/bin/bash", "--login", "-c"]