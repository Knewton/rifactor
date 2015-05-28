#-*- mode:conf; -*-

FROM rifactor:lib

ENV GHCVER=7.8.4
ENV CABALVER=1.20
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN apt-add-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y ghc-$GHCVER cabal-install-$CABALVER libffi-dev zlib1g-dev git wget
ENV PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH

ADD ./ /usr/local/src/rifactor/
WORKDIR /usr/local/src/rifactor
RUN rm -rf .cabal-sandbox cabal*config dist
RUN wget http://www.stackage.org/lts/cabal.config
RUN cabal update
RUN cabal sandbox init --sandbox=/usr/local
RUN cabal install -j --only-dependencies
WORKDIR /
RUN rm -rf /usr/local/src/rifactor
