#-*- mode:conf; -*-

FROM rifactor:dev

ADD ./ /usr/local/src/rifactor/
WORKDIR /usr/local/src/rifactor
RUN rm -rf .cabal-sandbox cabal*config dist
RUN wget http://www.stackage.org/lts/cabal.config
RUN cabal update
RUN cabal sandbox init --sandbox=/usr/local
RUN cabal install -j --ghc-options='-rtsopts -threaded -with-rtsopts=-N'
WORKDIR /
RUN rm -rf /usr/local/src/rifactor
