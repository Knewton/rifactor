#-*- mode:conf; -*-

FROM rifactor:lib

ENV GHCVER=7.8.4
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN apt-add-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y ghc-$GHCVER libffi-dev zlib1g-dev git wget
ENV PATH=/opt/ghc/$GHCVER/bin:$PATH

RUN wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key \
  | apt-key add -
RUN echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main' \
  | tee /etc/apt/sources.list.d/fpco.list
RUN apt-get update
RUN apt-get install -y stack
