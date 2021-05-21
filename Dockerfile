# syntax=docker/dockerfile:1
FROM ubuntu:latest
RUN apt-get update && apt-get install -y gfortran make doxygen
WORKDIR /snes
COPY . .
RUN make clobber && make tests
RUN make clobber && make testl
RUN doxygen
CMD ["/bin/bash", "--login", "-c"]