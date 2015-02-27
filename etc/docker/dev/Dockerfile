#-*- mode:conf; -*-

FROM knewton/rifactor:lib
MAINTAINER Tim Dysinger <tim@knewton.com>

ENV GHCVER=7.8.4
ENV CABALVER=1.22
RUN apt-get update
RUN apt-get install -y python-software-properties
RUN add-apt-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y cabal-install-$CABALVER ghc-$GHCVER libffi-dev zlib1g-dev git
ENV PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH

ADD ./ /usr/local/src/rifactor/
WORKDIR /usr/local/src/rifactor
RUN rm -rf .cabal-sandbox cabal.sandbox.config dist
RUN cabal update
RUN cabal sandbox init --sandbox=/usr/local
RUN cabal install -j \
  --enable-tests --enable-benchmarks --enable-documentation \
  --force-reinstalls --only-dependencies
WORKDIR /
RUN rm -rf /usr/local/src/rifactor

