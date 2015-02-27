#-*- mode:conf; -*-

FROM knewton/rifactor:dev
MAINTAINER Tim Dysinger <tim@knewton.com>

ADD ./ /usr/local/src/rifactor/
WORKDIR /usr/local/src/rifactor
RUN rm -rf .cabal-sandbox cabal.sandbox.config dist
RUN cabal update
RUN cabal sandbox init --sandbox=/usr/local
RUN cabal install -j
WORKDIR /
RUN rm -rf /usr/local/src/rifactor
