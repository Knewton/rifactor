#-*- mode:conf; -*-

FROM rifactor:dev

ADD ./ /usr/local/src/rifactor/
WORKDIR /usr/local/src/rifactor
RUN stack install
WORKDIR /
