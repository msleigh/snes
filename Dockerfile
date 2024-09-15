# syntax=docker/dockerfile:1
FROM ubuntu:latest
RUN apt-get update && apt-get install -y gfortran make doxygen graphviz
WORKDIR /snes
COPY . .
RUN make clobber && make tests
RUN make clobber && make testl
RUN make -C docs clobber && make -C docs html
CMD ["/bin/bash", "--login", "-c"]
