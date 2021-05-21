# syntax=docker/dockerfile:1
FROM ubuntu:latest
RUN apt-get update && apt-get install -y gfortran make
WORKDIR /snes
COPY . .
RUN make clobber && make tests
RUN make clobber && make testl
CMD ["/bin/bash", "--login", "-c"]