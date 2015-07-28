#-*- mode:conf; -*-

FROM ubuntu:14.04

ENV DEBIAN_FRONTEND noninteractive
ENV LANG en_US.UTF-8
RUN locale-gen en_US.UTF-8

# NOTE: This next block can speed up your repeat assembly times
# significantly. Uncomment to allow. Requires apt-cacher-ng running on
# the docker host.
RUN apt-get update \
 && apt-get install -y net-tools \
 && echo "Acquire::http { Proxy \"http://$(netstat -nr|grep '^0\.0\.0\.0'|awk '{print $2}'):3142\"; };" \
    | tee /etc/apt/apt.conf.d/02proxy

RUN apt-get update \
 && apt-get install -y netbase ca-certificates libgmp10 \
 && apt-get clean \
 && update-ca-certificates
